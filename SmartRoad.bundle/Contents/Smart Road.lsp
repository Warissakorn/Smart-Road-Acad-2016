;;; ==========================================================================
;;; AutoLISP: Smart Road & Auto-Junction Generator (ASCII Safe Version)
;;; Commands:
;;;   SMARTROADTOOLBAR - Show the Smart Road toolbar
;;;   SMARTROAD - Open the road settings dialog
;;;   DROAD - Draw new road (Line & Arc) with auto-junction & fillet
;;;   RNET  - Convert multiple centerlines (Line/Arc/Polyline) to road network
;;;   SRROAD, SRWALK, SRTRANSIT, SRJUNCTION, SREDIT - 2D corridor and plan tools
;;;   SRQUICK, SRPROFILE, SRSECTION - Quick corridor build, profile and section edit
;;;   SRCONNECT, SRQUANTITY - Connect existing road edges and measure 2D quantities
;;; ==========================================================================

(vl-load-com)

(if (not *droad-width*)  (setq *droad-width* 6.0))
(if (not *droad-fillet*) (setq *droad-fillet* 3.0))

;; --------------------------------------------------------------------------
;; 1. Layer Setup (ROAD-CENTER and ROAD-EDGE)
;; --------------------------------------------------------------------------
(defun sr:drop (count items)
  (repeat count (setq items (cdr items)))
  items
)

(defun droad:ensure-layers (doc / lyrs lyr-c lyr-e)
  (setq lyrs (vla-get-Layers doc))
  (if (not (tblsearch "LTYPE" "CENTER"))
    (progn
      (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes doc) "CENTER" "acadiso.lin"))
      (if (not (tblsearch "LTYPE" "CENTER"))
        (vl-catch-all-apply 'vla-Load (list (vla-get-Linetypes doc) "CENTER" "acad.lin"))
      )
    )
  )
  (if (not (tblsearch "LAYER" "ROAD-CENTER"))
    (progn
      (setq lyr-c (vla-Add lyrs "ROAD-CENTER"))
      (vla-put-Color lyr-c 1)
      (if (tblsearch "LTYPE" "CENTER") (vla-put-Linetype lyr-c "CENTER"))
    )
  )
  (if (not (tblsearch "LAYER" "ROAD-EDGE"))
    (progn
      (setq lyr-e (vla-Add lyrs "ROAD-EDGE"))
      (vla-put-Color lyr-e 3)
    )
  )
  (princ)
)

;; --------------------------------------------------------------------------
;; 2. Store & Read Road Width via XData
;; --------------------------------------------------------------------------
(defun droad:set-width-xdata (en w / ed)
  (regapp "DROAD_APP")
  (setq ed (entget en))
  (setq ed (append ed (list (list -3 (list "DROAD_APP" (cons 1040 (float w)))))))
  (entmod ed)
)

(defun droad:get-width-xdata (en def-w / xd app val)
  (setq xd (assoc -3 (entget en '("DROAD_APP"))))
  (if (and xd
           (setq app (cdr (assoc "DROAD_APP" (cdr xd))))
           (setq val (cdr (assoc 1040 app))))
    val
    def-w
  )
)

;; --------------------------------------------------------------------------
;; 3. Geometry & Corridor Helper Functions
;; --------------------------------------------------------------------------
(defun droad:flat-to-pts (lst / pts)
  (while lst
    (setq pts (cons (list (car lst) (cadr lst) (caddr lst)) pts)
          lst (cdddr lst))
  )
  (reverse pts)
)

(defun droad:wcs-angle (p1 p2 / dx dy)
  (setq dx (- (car p2) (car p1))
        dy (- (cadr p2) (cadr p1)))
  (if (and (equal dx 0.0 1e-10) (equal dy 0.0 1e-10))
    0.0
    (atan dy dx)
  )
)

(defun droad:ccw-diff (a1 a2 / d)
  (setq d (rem (- a2 a1) (* 2.0 pi)))
  (if (< d 0.0) (+ d (* 2.0 pi)) d)
)

(defun droad:unit-vec (v / len)
  (setq len (distance '(0.0 0.0 0.0) v))
  (if (> len 1e-8)
    (mapcar '(lambda (x) (/ x len)) v)
    '(1.0 0.0 0.0)
  )
)

(defun droad:dedupe-pts (pts tol / res p)
  (foreach p pts
    (if (not (vl-some '(lambda (q) (<= (distance p q) tol)) res))
      (setq res (cons p res))
    )
  )
  (reverse res)
)

(defun droad:point-in-corridor-p (pt cl-en half-w / p-close d prm s-prm e-prm deriv vec)
  (if (and cl-en (entget cl-en))
    (progn
      (setq p-close (vlax-curve-getClosestPointTo cl-en pt)
            d       (distance pt p-close))
      (if (< d (- half-w 1e-3))
        (if (vlax-curve-isClosed cl-en)
          T
          (progn
            (setq prm   (vlax-curve-getParamAtPoint cl-en p-close)
                  s-prm (vlax-curve-getStartParam cl-en)
                  e-prm (vlax-curve-getEndParam cl-en))
            (cond
              ((<= (abs (- prm s-prm)) 1e-4)
               (setq deriv (vlax-curve-getFirstDeriv cl-en s-prm)
                     vec   (mapcar '- pt (vlax-curve-getStartPoint cl-en)))
               (>= (apply '+ (mapcar '* vec deriv)) -1e-3))
              ((<= (abs (- prm e-prm)) 1e-4)
               (setq deriv (vlax-curve-getFirstDeriv cl-en e-prm)
                     vec   (mapcar '- pt (vlax-curve-getEndPoint cl-en)))
               (<= (apply '+ (mapcar '* vec deriv)) 1e-3))
              (T T)
            )
          )
        )
        nil
      )
    )
    nil
  )
)

(defun droad:explode-to-simple (vla-obj / nm exp-res lyr o)
  (setq nm (vla-get-ObjectName vla-obj))
  (cond
    ((or (= nm "AcDbLine") (= nm "AcDbArc"))
     (list vla-obj))
    ((wcmatch nm "*Polyline*")
     (setq lyr     (vla-get-Layer vla-obj)
           exp-res (vl-catch-all-apply 'vlax-invoke (list vla-obj 'Explode)))
     (if (not (vl-catch-all-error-p exp-res))
       (progn
         (foreach o exp-res (vla-put-Layer o lyr))
         (vla-Delete vla-obj)
         exp-res
       )
       (list vla-obj)
     ))
    (T (list vla-obj))
  )
)

(defun droad:split-simple-curve (vla-obj pts spc / en p-start p-end prms pt pt-on prm clean-prms all-prms res u1 u2 pt1 pt2 pt-m lyr obj-name cen rad a1 am a2 new-o)
  (if (or (not vla-obj) (vlax-erased-p vla-obj))
    nil
    (progn
      (setq en      (vlax-vla-object->ename vla-obj)
            p-start (vlax-curve-getStartParam en)
            p-end   (vlax-curve-getEndParam en))
      (foreach pt pts
        (setq pt-on (vlax-curve-getClosestPointTo en pt))
        (if (<= (distance pt pt-on) 1e-3)
          (progn
            (setq prm (vlax-curve-getParamAtPoint en pt-on))
            (if (and prm
                     (> (- prm p-start) 1e-4)
                     (> (- p-end prm) 1e-4))
              (setq prms (cons prm prms))
            )
          )
        )
      )
      (if (null prms)
        (list vla-obj)
        (progn
          (setq prms (vl-sort prms '<))
          (foreach prm prms
            (if (or (null clean-prms) (> (abs (- prm (car clean-prms))) 1e-4))
              (setq clean-prms (cons prm clean-prms))
            )
          )
          (setq clean-prms (reverse clean-prms)
                all-prms   (append (list p-start) clean-prms (list p-end))
                lyr        (vla-get-Layer vla-obj)
                obj-name   (vla-get-ObjectName vla-obj))
          (while (cdr all-prms)
            (setq u1       (car all-prms)
                  u2       (cadr all-prms)
                  all-prms (cdr all-prms)
                  pt1      (vlax-curve-getPointAtParam en u1)
                  pt2      (vlax-curve-getPointAtParam en u2))
            (cond
              ((= obj-name "AcDbLine")
               (setq new-o (vla-AddLine spc (vlax-3d-point pt1) (vlax-3d-point pt2)))
               (vla-put-Layer new-o lyr)
               (setq res (cons new-o res)))
              ((= obj-name "AcDbArc")
               (setq pt-m (vlax-curve-getPointAtParam en (* 0.5 (+ u1 u2)))
                     cen  (vlax-get vla-obj 'Center)
                     rad  (vla-get-Radius vla-obj)
                     a1   (droad:wcs-angle cen pt1)
                     am   (droad:wcs-angle cen pt-m)
                     a2   (droad:wcs-angle cen pt2))
               (if (< (droad:ccw-diff a1 am) (droad:ccw-diff a1 a2))
                 (setq new-o (vla-AddArc spc (vlax-3d-point cen) rad a1 a2))
                 (setq new-o (vla-AddArc spc (vlax-3d-point cen) rad a2 a1))
               )
               (vla-put-Layer new-o lyr)
               (setq res (cons new-o res)))
            )
          )
          (vla-Delete vla-obj)
          (reverse res)
        )
      )
    )
  )
)

;; --------------------------------------------------------------------------
;; 4. Auto-Extend Centerline Ends When Snapped to Existing Road Edge
;; --------------------------------------------------------------------------
(defun droad:extend-cl-to-junctions (cl-en def-w spc / ss-cl i ocl old-cls p-s p-e near-s near-e old-w d u-dir p-far tmp-ln raw-int hits p-hit ext-ln was-lw)
  (if (not (vlax-curve-isClosed cl-en))
    (progn
      (setq ss-cl (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-CENTER") (cons 410 "Model"))))
      (if ss-cl
        (progn
          (setq i 0)
          (repeat (sslength ss-cl)
            (setq ocl (ssname ss-cl i)
                  i   (1+ i))
            (if (not (equal ocl cl-en))
              (setq old-cls (cons ocl old-cls))
            )
          )
        )
      )
      (if old-cls
        (progn
          ;; Check Start Point
          (setq p-s (vlax-curve-getStartPoint cl-en))
          (if (not (vl-some '(lambda (c) (<= (distance p-s (vlax-curve-getClosestPointTo c p-s)) 1e-3)) old-cls))
            (progn
              (foreach ocl old-cls
                (setq old-w (droad:get-width-xdata ocl def-w)
                      d     (distance p-s (vlax-curve-getClosestPointTo ocl p-s)))
                (if (and (null near-s) (<= d (+ (* 0.5 old-w) 0.35)))
                  (setq near-s (cons ocl old-w))
                )
              )
              (if near-s
                (progn
                  (setq u-dir   (droad:unit-vec (vlax-curve-getFirstDeriv cl-en (vlax-curve-getStartParam cl-en)))
                        p-far   (mapcar '- p-s (mapcar '(lambda (x) (* x (* 1.5 (cdr near-s)))) u-dir))
                        tmp-ln  (vla-AddLine spc (vlax-3d-point p-s) (vlax-3d-point p-far))
                        raw-int (vl-catch-all-apply 'vlax-invoke (list tmp-ln 'IntersectWith (vlax-ename->vla-object (car near-s)) acExtendNone)))
                  (vla-Delete tmp-ln)
                  (if (and (not (vl-catch-all-error-p raw-int)) raw-int)
                    (progn
                      (setq hits  (vl-sort (droad:flat-to-pts raw-int) '(lambda (a b) (< (distance p-s a) (distance p-s b))))
                            p-hit (car hits))
                      (if (> (distance p-s p-hit) 1e-3)
                        (progn
                          (setq ext-ln (vla-AddLine spc (vlax-3d-point p-hit) (vlax-3d-point p-s))
                                was-lw (= (cdr (assoc 0 (entget cl-en))) "LWPOLYLINE"))
                          (vla-put-Layer ext-ln "ROAD-CENTER")
                          (setvar "PEDITACCEPT" 1)
                          (vl-cmdf "_.PEDIT" cl-en "_Join" (ssadd (vlax-vla-object->ename ext-ln)) "" "")
                          (if (= 1 (logand 1 (getvar "CMDACTIVE"))) (command))
                          (if (and (not was-lw) (null (entget cl-en))) (setq cl-en (entlast)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
          ;; Check End Point
          (setq p-e (vlax-curve-getEndPoint cl-en))
          (if (not (vl-some '(lambda (c) (<= (distance p-e (vlax-curve-getClosestPointTo c p-e)) 1e-3)) old-cls))
            (progn
              (foreach ocl old-cls
                (setq old-w (droad:get-width-xdata ocl def-w)
                      d     (distance p-e (vlax-curve-getClosestPointTo ocl p-e)))
                (if (and (null near-e) (<= d (+ (* 0.5 old-w) 0.35)))
                  (setq near-e (cons ocl old-w))
                )
              )
              (if near-e
                (progn
                  (setq u-dir   (droad:unit-vec (vlax-curve-getFirstDeriv cl-en (vlax-curve-getEndParam cl-en)))
                        p-far   (mapcar '+ p-e (mapcar '(lambda (x) (* x (* 1.5 (cdr near-e)))) u-dir))
                        tmp-ln  (vla-AddLine spc (vlax-3d-point p-e) (vlax-3d-point p-far))
                        raw-int (vl-catch-all-apply 'vlax-invoke (list tmp-ln 'IntersectWith (vlax-ename->vla-object (car near-e)) acExtendNone)))
                  (vla-Delete tmp-ln)
                  (if (and (not (vl-catch-all-error-p raw-int)) raw-int)
                    (progn
                      (setq hits  (vl-sort (droad:flat-to-pts raw-int) '(lambda (a b) (< (distance p-e a) (distance p-e b))))
                            p-hit (car hits))
                      (if (> (distance p-e p-hit) 1e-3)
                        (progn
                          (setq ext-ln (vla-AddLine spc (vlax-3d-point p-e) (vlax-3d-point p-hit))
                                was-lw (= (cdr (assoc 0 (entget cl-en))) "LWPOLYLINE"))
                          (vla-put-Layer ext-ln "ROAD-CENTER")
                          (setvar "PEDITACCEPT" 1)
                          (vl-cmdf "_.PEDIT" cl-en "_Join" (ssadd (vlax-vla-object->ename ext-ln)) "" "")
                          (if (= 1 (logand 1 (getvar "CMDACTIVE"))) (command))
                          (if (and (not was-lw) (null (entget cl-en))) (setq cl-en (entlast)))
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  cl-en
)

;; --------------------------------------------------------------------------
;; 5. Offset Edges, Trim Junctions & Apply Curb Fillets
;; --------------------------------------------------------------------------
(defun droad:process-road (cl-en width fillet-rad spc / half-w vla-cl off1 off2 new-off new-simple new-enames o no oo ss-cl ss-ed i en old-cls old-edge-vlas hit-p raw active-old-simple all-int-pts split-new split-old kept-old kept-new p-mid)
  (setq half-w (* 0.5 width)
        cl-en  (droad:extend-cl-to-junctions cl-en width spc)
        vla-cl (vlax-ename->vla-object cl-en))

  ;; Offset Left & Right Road Edges
  (setq off1 (vl-catch-all-apply 'vlax-invoke (list vla-cl 'Offset half-w))
        off2 (vl-catch-all-apply 'vlax-invoke (list vla-cl 'Offset (- half-w))))

  (if (or (vl-catch-all-error-p off1) (vl-catch-all-error-p off2) (null off1) (null off2))
    (progn
      (if (listp off1) (foreach o off1 (vla-Delete o)))
      (if (listp off2) (foreach o off2 (vla-Delete o)))
      (princ "\nCannot offset both sides of the centerline at this width.")
      (exit)
    )
  )
  (vla-put-Layer vla-cl "ROAD-CENTER")
  (droad:set-width-xdata cl-en width)
  (setq new-off (append off1 off2))

  (foreach o new-off
    (vla-put-Layer o "ROAD-EDGE")
    (setq new-simple (append new-simple (droad:explode-to-simple o)))
  )
  (setq new-enames (mapcar 'vlax-vla-object->ename new-simple))

  ;; Collect Existing Centerlines
  (setq ss-cl (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-CENTER") (cons 410 "Model"))))
  (if ss-cl
    (progn
      (setq i 0)
      (repeat (sslength ss-cl)
        (setq en (ssname ss-cl i)
              i  (1+ i))
        (if (not (equal en cl-en))
          (setq old-cls (cons en old-cls))
        )
      )
    )
  )

  ;; Collect Existing Road Edges
  (setq ss-ed (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-EDGE") (cons 410 "Model"))))
  (if ss-ed
    (progn
      (setq i 0)
      (repeat (sslength ss-ed)
        (setq en (ssname ss-ed i)
              i  (1+ i))
        (if (not (member en new-enames))
          (setq old-edge-vlas (cons (vlax-ename->vla-object en) old-edge-vlas))
        )
      )
    )
  )

  ;; Filter Intersecting Old Edges
  (foreach o old-edge-vlas
    (setq hit-p nil)
    (foreach no new-simple
      (setq raw (vl-catch-all-apply 'vlax-invoke (list o 'IntersectWith no acExtendNone)))
      (if (and (not (vl-catch-all-error-p raw)) raw)
        (setq hit-p T)
      )
    )
    (if hit-p
      (setq active-old-simple (append active-old-simple (droad:explode-to-simple o)))
      (progn
        (setq en    (vlax-vla-object->ename o)
              p-mid (vlax-curve-getPointAtParam en (* 0.5 (+ (vlax-curve-getStartParam en) (vlax-curve-getEndParam en)))))
        (if (and (droad:point-in-corridor-p (vlax-curve-getStartPoint en) cl-en half-w)
                 (droad:point-in-corridor-p p-mid cl-en half-w)
                 (droad:point-in-corridor-p (vlax-curve-getEndPoint en) cl-en half-w))
          (vla-Delete o)
        )
      )
    )
  )

  ;; Split & Trim Overlapping Segments at Junctions
  (if active-old-simple
    (progn
      (foreach no new-simple
        (foreach oo active-old-simple
          (setq raw (vl-catch-all-apply 'vlax-invoke (list no 'IntersectWith oo acExtendNone)))
          (if (and (not (vl-catch-all-error-p raw)) raw)
            (setq all-int-pts (append all-int-pts (droad:flat-to-pts raw)))
          )
        )
      )
      (setq all-int-pts (droad:dedupe-pts all-int-pts 1e-3))

      (foreach no new-simple
        (setq split-new (append split-new (droad:split-simple-curve no all-int-pts spc)))
      )
      (foreach oo active-old-simple
        (setq split-old (append split-old (droad:split-simple-curve oo all-int-pts spc)))
      )

    )
    (setq split-new new-simple)
  )

  (foreach o split-old
    (if (droad:edge-in-road-p o (list cl-en) width)
      (vla-Delete o)
      (setq kept-old (cons o kept-old))
    )
  )
  (foreach o split-new
    (if (droad:edge-in-road-p o old-cls width)
      (vla-Delete o)
      (setq kept-new (cons o kept-new))
    )
  )
  (if (> fillet-rad 0.0)
    (droad:fillet-junctions kept-new kept-old fillet-rad)
  )
  cl-en
)

(defun droad:edge-in-road-p (obj centers default-width / en mid)
  (setq en (vlax-vla-object->ename obj)
        mid (vlax-curve-getPointAtParam en
              (* 0.5 (+ (vlax-curve-getStartParam en)
                        (vlax-curve-getEndParam en)))))
  (vl-some
    '(lambda (center)
       (droad:point-in-corridor-p mid center
         (* 0.5 (droad:get-width-xdata center default-width))))
    centers)
)

(defun droad:matching-ends (first second / p q match)
  (foreach p (list (vlax-curve-getStartPoint first) (vlax-curve-getEndPoint first))
    (foreach q (list (vlax-curve-getStartPoint second) (vlax-curve-getEndPoint second))
      (if (equal p q 1e-3) (setq match p))
    )
  )
  match
)

(defun droad:fillet-junctions (new-edges old-edges radius / new old a b corner other-a other-b pick-a pick-b last-entity vec-a vec-b cosine tangent)
  (setvar "FILLETRAD" radius)
  (setvar "TRIMMODE" 1)
  (foreach new new-edges
    (foreach old old-edges
      (if (and (not (vlax-erased-p new)) (not (vlax-erased-p old))
               (= "AcDbLine" (vla-get-ObjectName new))
               (= "AcDbLine" (vla-get-ObjectName old))
               (setq a (vlax-vla-object->ename new)
                     b (vlax-vla-object->ename old)
                     corner (droad:matching-ends a b)))
        (progn
          (setq other-a (droad:opposite-end a corner)
                other-b (droad:opposite-end b corner)
                pick-a (mapcar '(lambda (x y) (* 0.5 (+ x y))) corner other-a)
                pick-b (mapcar '(lambda (x y) (* 0.5 (+ x y))) corner other-b)
                vec-a (droad:unit-vec (mapcar '- other-a corner))
                vec-b (droad:unit-vec (mapcar '- other-b corner))
                cosine (apply '+ (mapcar '* vec-a vec-b)))
          (if (and (equal (caddr other-a) (caddr other-b) 1e-8)
                   (< (abs cosine) 0.999999)
                   (setq tangent (* radius (sqrt (/ (+ 1.0 cosine) (- 1.0 cosine)))))
                   (< tangent (- (min (distance corner other-a) (distance corner other-b)) 1e-3)))
            (progn
              (setq last-entity (entlast))
              (vl-cmdf "_.FILLET" (list a (trans pick-a 0 1)) (list b (trans pick-b 0 1)))
              (if (and (not (equal last-entity (entlast)))
                       (= "ARC" (cdr (assoc 0 (entget (entlast))))))
                (vla-put-Layer (vlax-ename->vla-object (entlast)) "ROAD-EDGE")
              )
            )
          )
        )
      )
    )
  )
)

(defun droad:opposite-end (en pt / start)
  (setq start (vlax-curve-getStartPoint en))
  (if (equal start pt 1e-3) (vlax-curve-getEndPoint en) start)
)

(defun droad:settings (/ width radius)
  (initget 6)
  (setq width (getdist (strcat "\nRoad width <" (rtos *droad-width* 2 2) ">: ")))
  (initget 4)
  (setq radius (getdist (strcat "\nJunction fillet radius, 0 for square corners <"
                               (rtos *droad-fillet* 2 2) ">: ")))
  (if width (setq *droad-width* width))
  (if radius (setq *droad-fillet* radius))
)

(defun droad:restore (doc vars undo-open / item)
  (foreach item vars (setvar (car item) (cdr item)))
  (if undo-open (vla-EndUndoMark doc))
)

(defun droad:run (draw prompt-settings / *error* doc spc vars undo-open ss before en index processed skipped)
  (defun *error* (msg)
    (droad:restore doc vars undo-open)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg))
    )
    (if undo-open (princ "\nUse U to undo drawing changes from this run."))
    (princ)
  )
  (cond
    ((= 1 (getvar "CVPORT"))
     (princ "\nSwitch to Model space or activate a model viewport."))
    ((= 0 (logand 1 (getvar "UNDOCTL")))
     (princ "\nEnable UNDO (All) before running Smart Road."))
    (T
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
           spc (vla-get-ModelSpace doc)
           vars (mapcar '(lambda (name) (cons name (getvar name)))
                  '("PEDITACCEPT" "FILLETRAD" "TRIMMODE" "PLINEWID" "PLINETYPE"))
           processed 0
           skipped 0)
     (if prompt-settings (droad:settings))
     (if draw
       (progn
         (vla-StartUndoMark doc)
         (setq undo-open T before (entlast))
         (setvar "PLINEWID" 0.0)
         (setvar "PLINETYPE" 2)
         (princ "\nDraw the centerline with PLINE. Use Arc for curved sections.")
         (command "_.PLINE")
         (while (= 1 (logand 1 (getvar "CMDACTIVE"))) (command pause))
         (setq en (entlast))
         (if (and en (not (equal before en))
                  (= "LWPOLYLINE" (cdr (assoc 0 (entget en)))))
           (setq ss (ssadd en))
         )
       )
       (progn
         (princ "\nSelect centerlines (LINE, ARC or LWPOLYLINE): ")
         (setq ss (ssget "_:L" '((0 . "LINE,ARC,LWPOLYLINE") (410 . "Model"))))
       )
     )
     (if ss
       (progn
         (if (not undo-open)
           (progn (vla-StartUndoMark doc) (setq undo-open T))
         )
         (setq index 0)
         (repeat (sslength ss)
           (setq en (ssname ss index) index (1+ index))
           (if (and (droad:planar-center-p en)
                    (not (droad:get-width-xdata en nil)))
             (progn
               (if (= 0 processed) (droad:ensure-layers doc))
               (droad:process-road en *droad-width* *droad-fillet* spc)
               (setq processed (1+ processed))
             )
             (setq skipped (1+ skipped))
           )
         )
         (princ (strcat "\nRoads created: " (itoa processed)
                        ". Centerlines skipped: " (itoa skipped) "."))
       )
     )
     (droad:restore doc vars undo-open)
    )
  )
  (princ)
)

(defun droad:planar-center-p (en / data kind start end)
  (setq data (entget en) kind (cdr (assoc 0 data))
        start (vlax-curve-getStartPoint en)
        end (vlax-curve-getEndPoint en))
  (and (> (vlax-curve-getDistAtParam en (vlax-curve-getEndParam en)) 1e-6)
       (if (= kind "LINE")
         (equal (caddr start) (caddr end) 1e-8)
         (equal (cdr (assoc 210 data)) '(0.0 0.0 1.0) 1e-8)))
)

(defun droad:write-dialog (file / line)
  (foreach line
    '("smart_road : dialog {"
      "  label = \"Smart Road\";"
      "  : boxed_column {"
      "    label = \"Road settings (drawing units)\";"
      "    : edit_box { key = \"width\"; label = \"Full road width\"; edit_width = 14; }"
      "    : edit_box { key = \"radius\"; label = \"Junction fillet radius\"; edit_width = 14; }"
      "    : text { label = \"Set the radius to 0 for square corners.\"; }"
      "  }"
      "  : text { key = \"message\"; width = 51; }"
      "  : row {"
      "    : button { key = \"draw\"; label = \"&Draw centerline\"; is_default = true; }"
      "    : button { key = \"select\"; label = \"&Select centerlines\"; }"
      "    : button { key = \"quick_profile\"; label = \"Quick Profile\"; }"
      "    : button { key = \"cancel\"; label = \"Cancel\"; is_cancel = true; }"
      "  }"
      "}")
    (write-line line file)
  )
)

(defun droad:accept-dialog (choice / width-text radius-text width radius)
  (setq width-text (vl-string-trim " " (get_tile "width"))
        radius-text (vl-string-trim " " (get_tile "radius"))
        width (if (= width-text (rtos *droad-width* 2 8))
                *droad-width*
                (distof width-text 2))
        radius (if (= radius-text (rtos *droad-fillet* 2 8))
                 *droad-fillet*
                 (distof radius-text 2)))
  (cond
    ((or (null width) (<= width 0.0))
     (set_tile "message" "Enter a road width greater than 0.")
     (mode_tile "width" 2)
     (mode_tile "width" 3))
    ((or (null radius) (< radius 0.0))
     (set_tile "message" "Enter a fillet radius of 0 or greater.")
     (mode_tile "radius" 2)
     (mode_tile "radius" 3))
    (T
     (setq *droad-width* width *droad-fillet* radius)
     (done_dialog choice))
  )
)

(defun droad:dialog ( / *error* path file dialog-id choice)
  (defun *error* (msg)
    (if file (close file))
    (if (and dialog-id (> dialog-id 0)) (unload_dialog dialog-id))
    (if (and path (findfile path)) (vl-file-delete path))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg))
    )
    (princ)
  )
  (setq path (vl-filename-mktemp "smart-road-" nil ".dcl")
        file (open path "w"))
  (if file
    (progn
      (droad:write-dialog file)
      (close file)
      (setq file nil dialog-id (load_dialog path))
      (if (and (> dialog-id 0) (new_dialog "smart_road" dialog-id))
        (progn
          (set_tile "width" (rtos *droad-width* 2 8))
          (set_tile "radius" (rtos *droad-fillet* 2 8))
          (action_tile "width" "(set_tile \"message\" \"\")")
          (action_tile "radius" "(set_tile \"message\" \"\")")
          (action_tile "draw" "(droad:accept-dialog 1)")
          (action_tile "select" "(droad:accept-dialog 2)")
          (action_tile "quick_profile" "(done_dialog 3)")
          (action_tile "cancel" "(done_dialog 0)")
          (mode_tile "width" 2)
          (setq choice (start_dialog))
        )
        (princ "\nSmart Road could not load its settings dialog.")
      )
    )
    (princ "\nSmart Road could not write its temporary dialog file.")
  )
  (if (and dialog-id (> dialog-id 0)) (unload_dialog dialog-id))
  (if (findfile path) (vl-file-delete path))
  choice
)

(defun c:SMARTROAD (/ choice)
  (if (= 1 (getvar "CVPORT"))
    (princ "\nSwitch to Model space or activate a model viewport.")
    (progn
      (setq choice (droad:dialog))
      (cond
        ((eq choice 1) (droad:run T nil))
        ((eq choice 2) (droad:run nil nil))
        ((eq choice 3) (c:SRPROFILE))
      )
    )
  )
  (princ)
)

(defun c:DROAD () (droad:run T T))
(defun c:RNET () (droad:run nil T))
(defun droad:toolbar-icons (/ roots root folder)
  (setq roots (mapcar 'getenv '("APPDATA" "PROGRAMDATA" "ProgramFiles")))
  (while (and roots (not folder))
    (setq root (car roots) roots (cdr roots))
    (if root
      (setq folder (findfile
        (strcat root "\\Autodesk\\ApplicationPlugins\\SmartRoad.bundle\\Contents\\Icons\\SmartRoadStyle16.bmp")))
    )
  )
  (if (not folder)
    (setq folder (findfile "SmartRoadStyle16.bmp"))
  )
  (if folder (vl-filename-directory folder))
)

(setq sr:toolbar-spec
  '(("Smart Road Settings" "Open the Smart Road settings dialog" "^C^CSMARTROAD " "SmartRoadSettings")
    ("Draw Road" "Draw a new road centerline" "^C^CDROAD " "SmartRoadDraw")
    ("Select Centerlines" "Build roads from existing centerlines" "^C^CRNET " "SmartRoadSelect")
    ("Road Corridor" "Build a road using an editable cross section" "^C^CSRROAD " "SmartRoadCorridor")
    ("Walkway" "Build a walkway, crosswalk, refuge island or ramp plan symbol" "^C^CSRWALK " "SmartRoadWalk")
    ("Transit" "Build bus and rail corridors, stops and platforms" "^C^CSRTRANSIT " "SmartRoadTransit")
    ("Junction" "Draw a tee, crossing, roundabout or turn lane" "^C^CSRJUNCTION " "SmartRoadJunction")
    ("Edit Corridor" "Edit, preview and rebuild a corridor" "^C^CSREDIT " "SmartRoadEdit")
    ("Quick Build" "Build selected alignments using the last corridor profile" "SRQUICK " "SmartRoadQuick")
    ("Pick Section" "Pick two points to change a corridor section" "^C^CSRSECTION " "SmartRoadSection")
    ("Connect Edges" "Trim two road edges into a junction corner" "^C^CSRCONNECT " "SmartRoadConnect")
    ("Quantities" "Report lengths and plan areas in the drawing or CSV" "SRQUANTITY " "SmartRoadQuantity")
    ("Road Appearance" "Fill road surfaces and generate lane markings" "^C^CSRSTYLE " "SmartRoadStyle")))

(defun droad:toolbar-buttons (toolbar folder start / index spec button base)
  (setq index start)
  (foreach spec (sr:drop start sr:toolbar-spec)
    (setq button (vla-AddToolbarButton toolbar index (car spec) (cadr spec)
                                       (caddr spec) :vlax-false)
          base (strcat folder "\\" (cadddr spec)))
    (vla-SetBitmaps button (strcat base "16.bmp") (strcat base "32.bmp"))
    (setq index (1+ index))
  )
  (if (= start 0)
    (progn
      (vla-Dock toolbar acToolbarDockTop)
      (vla-put-Visible toolbar :vlax-true)))
)

(defun droad:legacy-toolbar-p (toolbar / count index matches)
  (setq count (vla-get-Count toolbar))
  (if (member count '(3 8 10 12))
    (progn
      (setq index 0 matches T)
      (repeat count
        (setq matches (and matches
                           (equal (vla-get-Name (vla-Item toolbar index))
                                  (car (nth index sr:toolbar-spec))))
              index (1+ index)))
      matches))
)

(defun droad:toolbar (show / groups toolbars toolbar folder icon result create start)
  (setq groups (vla-get-MenuGroups (vlax-get-acad-object))
        toolbars (vla-get-Toolbars (vla-Item groups 0))
        toolbar (vl-catch-all-apply 'vla-Item (list toolbars "Smart Road"))
        create (vl-catch-all-error-p toolbar))
  (if (or create (droad:legacy-toolbar-p toolbar))
    (progn
      (setq folder (droad:toolbar-icons))
      (if (and show (not folder))
        (progn
          (setq icon (getfiled "Locate SmartRoadStyle16.bmp in the Icons folder"
                               "SmartRoadStyle16.bmp" "bmp" 0))
          (if icon (setq folder (vl-filename-directory icon)))
        )
      )
      (if folder
        (progn
          (setq start (if create 0 (vla-get-Count toolbar)))
          (if create (setq toolbar (vla-Add toolbars "Smart Road")))
          (setq result (vl-catch-all-apply 'droad:toolbar-buttons (list toolbar folder start)))
          (if (vl-catch-all-error-p result)
            (progn
              (if create
                (vla-Delete toolbar)
                (while (> (vla-get-Count toolbar) start)
                  (vla-Delete (vla-Item toolbar (1- (vla-get-Count toolbar))))))
              (princ (strcat "\nSmart Road toolbar: " (vl-catch-all-error-message result)))))))
    )
  )
  (if (and show (not (vl-catch-all-error-p toolbar))
           (not (and create (vl-catch-all-error-p result))))
    (vla-put-Visible toolbar :vlax-true))
  (princ)
)

(defun c:SMARTROADTOOLBAR (/ result)
  (setq result (vl-catch-all-apply 'droad:toolbar (list T)))
  (if (vl-catch-all-error-p result)
    (princ (strcat "\nSmart Road toolbar: " (vl-catch-all-error-message result)))
  )
  (princ)
)

;; Editable 2D corridors and plan symbols.
(setq sr:bands
  '(("Lane" 3.5 7) ("Median" 2.0 2) ("Shoulder" 1.5 8)
    ("Parking" 2.5 8) ("Cycle" 1.75 4) ("Bus" 3.5 1)
    ("BRT" 3.5 1) ("Curb" 0.15 8) ("Walkway" 2.0 3)
    ("Rail" 1.435 6) ("TrackGap" 2.0 8) ("Platform" 3.0 5)))

(defun sr:number (label default allow-zero / value)
  (initget (if allow-zero 4 6))
  (setq value (getreal (strcat "\n" label " <" (rtos default 2 4) ">: ")))
  (if value value default)
)

(defun sr:preset-metres (group / option)
  (cond
    ((equal group "Road")
     (initget "TwoWay OneWay Divided Complete")
     (setq option (getkword "\nRoad [TwoWay/OneWay/Divided/Complete] <TwoWay>: "))
     (cond
       ((equal option "OneWay") '(nil (("Lane" 3.5) ("Lane" 3.5))))
       ((equal option "Divided")
        '((("Median" 1.0) ("Lane" 3.5) ("Lane" 3.5) ("Shoulder" 1.5))
          (("Median" 1.0) ("Lane" 3.5) ("Lane" 3.5) ("Shoulder" 1.5))))
       ((equal option "Complete")
        '((("Lane" 3.5) ("Parking" 2.5) ("Cycle" 1.75) ("Curb" 0.15) ("Walkway" 2.0))
          (("Lane" 3.5) ("Parking" 2.5) ("Cycle" 1.75) ("Curb" 0.15) ("Walkway" 2.0))))
       (T '((("Lane" 3.5)) (("Lane" 3.5))))))
    ((equal group "Walkway")
     (initget "OneSide BothSides Independent")
     (setq option (getkword "\nWalkway [OneSide/BothSides/Independent] <Independent>: "))
     (cond
       ((equal option "OneSide") '(nil (("Curb" 0.15) ("Walkway" 2.0))))
       ((equal option "BothSides") '((("Curb" 0.15) ("Walkway" 2.0)) (("Curb" 0.15) ("Walkway" 2.0))))
       (T '((("Walkway" 1.0)) (("Walkway" 1.0))))))
    (T
     (initget "Bus BRT SingleRail DoubleRail")
     (setq option (getkword "\nTransit [Bus/BRT/SingleRail/DoubleRail] <Bus>: "))
     (cond
       ((equal option "DoubleRail")
        '((("TrackGap" 1.0) ("Rail" 1.435)) (("TrackGap" 1.0) ("Rail" 1.435))))
       ((equal option "SingleRail") '((("Rail" 0.7175)) (("Rail" 0.7175))))
       ((equal option "BRT") '((("BRT" 3.5)) (("BRT" 3.5))))
       (T '(nil (("Bus" 3.5))))))
  )
)

(defun sr:scale-profile (profile scale)
  (mapcar '(lambda (side)
             (mapcar '(lambda (band) (list (car band) (* scale (cadr band)))) side))
          profile)
)

(defun sr:scale-default-profile (profile)
  (sr:scale-profile profile (sr:unit-scale))
)

(defun sr:preset (group)
  (sr:scale-default-profile (sr:preset-metres group))
)

(defun sr:profile-valid-p (profile)
  (and (listp profile) (= (length profile) 2)
       (or (car profile) (cadr profile))
       (vl-every
         '(lambda (side)
            (and (listp side) (<= (length side) 16)
                 (vl-every
                   '(lambda (band)
                      (and (listp band) (= (length band) 2)
                           (assoc (car band) sr:bands)
                           (numberp (cadr band)) (> (cadr band) 0.0)))
                   side)))
         profile))
)

(defun sr:profile-summary (profile / side text band)
  (foreach side profile
    (setq text (if text "\nRight:" "\nLeft:"))
    (foreach band side
      (setq text (strcat text " " (car band) " " (rtos (cadr band) 2 4))))
    (princ (if side text (strcat text " (empty)")))
  )
)

(defun sr:edit-side (side label / action kind spec index width)
  (while (progn
           (initget "Add Insert Width Remove Clear Done")
           (setq action (getkword (strcat "\n" label " [Add/Insert/Width/Remove/Clear/Done] <Done>: ")))
           (and action (not (equal action "Done"))))
    (cond
      ((equal action "Clear") (setq side nil))
      ((member action '("Remove" "Width"))
       (if side
         (progn
           (initget 6)
           (setq index (getint "\nBand number, counted from the alignment: "))
           (if (and index (<= index (length side)))
             (progn
               (setq width (if (equal action "Width")
                             (list (list (car (nth (1- index) side))
                                         (sr:number "New band width" (cadr (nth (1- index) side)) nil)))))
               (setq side (append (reverse (sr:drop (- (length side) index -1) (reverse side)))
                                  width (sr:drop index side))))))))
      ((< (length side) 16)
       (setq index (1+ (length side)))
       (if (equal action "Insert")
         (progn
           (initget 6)
           (setq index (getint "\nInsert before band number: "))))
       (if (and index (<= index (1+ (length side))))
         (progn
           (initget "Lane Median Shoulder Parking Cycle Bus BRT Curb Walkway Rail TrackGap Platform")
           (setq kind (getkword
             "\nBand [Lane/Median/Shoulder/Parking/Cycle/Bus/BRT/Curb/Walkway/Rail/TrackGap/Platform]: "))
           (if kind
             (progn
               (setq spec (assoc kind sr:bands))
               (setq side (append
                 (reverse (sr:drop (- (length side) index -1) (reverse side)))
                 (list (list kind (sr:number "Band width" (* (sr:unit-scale) (cadr spec)) nil)))
                 (sr:drop (1- index) side))))))))
      (T (princ "\nA side can have up to 16 bands.")))
    (princ (strcat "\n" label " (from alignment outward): " (vl-prin1-to-string side)))
  )
  side
)

(defun sr:template-key ()
  (sr:profile-template-key (getstring "\nTemplate name (letters, digits, underscores): "))
)

(defun sr:configure (profile / action key saved)
  (sr:profile-summary profile)
  (while (progn
           (initget "Left Right Save Load Done Cancel")
           (setq action (getkword "\nProfile [Left/Right/Save/Load/Done/Cancel] <Done>: "))
           (and action (not (member action '("Done" "Cancel")))))
    (cond
      ((equal action "Left") (setq profile (list (sr:edit-side (car profile) "Left") (cadr profile))))
      ((equal action "Right") (setq profile (list (car profile) (sr:edit-side (cadr profile) "Right"))))
      ((equal action "Save")
       (if (and (sr:profile-valid-p profile) (setq key (sr:template-key)))
         (progn
           (initget "Yes No")
           (if (or (not (getenv key))
                   (equal "Yes" (getkword "\nReplace saved template? [Yes/No] <No>: ")))
             (setenv key (sr:pack-profile profile))))))
      ((equal action "Load")
       (if (and (setq key (sr:template-key)) (setq saved (getenv key))
                (not (vl-catch-all-error-p
                       (setq saved (vl-catch-all-apply 'sr:profile-ui-read-template (list saved)))))
                (sr:profile-valid-p saved))
         (setq profile saved)
         (princ "\nNo valid template with that name."))))
    (sr:profile-summary profile)
  )
  (if (and (not (equal action "Cancel")) (sr:profile-valid-p profile)) profile)
)

(defun sr:layer (name color / existing)
  (if (not sr:previewing)
    (cond
      ((setq existing (tblsearch "LAYER" name))
       (if (/= 0 (logand 4 (cdr (assoc 70 existing))))
         (progn (princ (strcat "\nUnlock layer " name " and try again.")) (exit))))
      ((not (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                             '(100 . "AcDbLayerTableRecord") (cons 2 name)
                             '(70 . 0) (cons 62 color) '(6 . "Continuous"))))
       (exit))))
  name
)

(defun sr:own (entity / data tag)
  (if (null entity) (exit))
  (setq sr:new (cons entity sr:new))
  (if sr:owner
    (progn
      (setq data (entget entity)
            tag (list "SR-CORRIDOR-EDGE" (cons 1005 sr:owner)))
      (if sr:span
        (setq tag (append tag (list (cons 1000 sr:edge-signature)
                                    (cons 1040 (float (car sr:span)))
                                    (cons 1040 (float (cadr sr:span)))))))
      (if (not (entmod (append data (list (list -3 tag))))) (exit))))
  entity
)

(defun sr:poly (pts closed layer / data point vertices)
  (cond
    ((< (length pts) 2) nil)
    (sr:previewing
     (while (cdr pts)
       (grdraw (trans (car pts) 0 1) (trans (cadr pts) 0 1) 3)
       (setq pts (cdr pts))))
    (T
     (foreach point pts
       (setq vertices (cons (cons 10 (list (car point) (cadr point))) vertices)))
     (setq data (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                      '(100 . "AcDbPolyline") (cons 90 (length pts))
                      (cons 70 (if closed 1 0)) (cons 38 (caddr (car pts)))))
     (if (entmake (append data (reverse vertices))) (sr:own (entlast)) (exit))))
)

(defun sr:circle (pt radius layer / data)
  (setq data (list '(0 . "CIRCLE") (cons 8 layer) (cons 10 pt) (cons 40 radius)))
  (if (entmake data) (sr:own (entlast)) (exit))
)

(defun sr:arc (pt radius start end layer)
  (if (entmake (list '(0 . "ARC") '(100 . "AcDbEntity") (cons 8 layer)
                    '(100 . "AcDbCircle") (cons 10 pt) (cons 40 radius)
                    '(100 . "AcDbArc") (cons 50 start) (cons 51 end)))
    (sr:own (entlast))
    (exit))
)

(defun sr:pick-axis (/ selected entity)
  (if (setq selected (entsel "\nSelect open LINE, ARC or lightweight polyline: "))
    (progn
      (setq entity (car selected))
      (if (and (equal (cdr (assoc 410 (entget entity))) "Model")
               (member (cdr (assoc 0 (entget entity))) '("LINE" "ARC" "LWPOLYLINE"))
               (not (vlax-curve-isClosed entity))
               (droad:planar-center-p entity))
        entity
        (progn (princ "\nChoose an open, horizontal curve in Model Space.") nil))))
)

(defun sr:length (axis)
  (vlax-curve-getDistAtParam axis (vlax-curve-getEndParam axis))
)

(defun sr:unit-scale (/ scale)
  (setq scale (cdr (assoc (getvar "INSUNITS")
    '((1 . 39.3700787402) (2 . 3.280839895) (3 . 0.000621371192)
      (4 . 1000.0) (5 . 100.0) (6 . 1.0) (7 . 0.001)
      (8 . 39370078.7402) (9 . 39370.0787402) (10 . 1.093613298)
      (11 . 1.0e10) (12 . 1.0e9) (13 . 1.0e6) (14 . 10.0)
      (15 . 0.1) (16 . 0.01) (17 . 1.0e-9) (18 . 6.684587122e-12)
      (19 . 1.057000834e-16) (20 . 3.240779289e-17) (21 . 3.280833333)))))
  (if scale scale 1.0)
)

(defun sr:bisect-stations (axis start finish p q tolerance depth / middle pt chord sag radius)
  (setq middle (* 0.5 (+ start finish))
        pt (vlax-curve-getPointAtDist axis middle)
        chord (mapcar '(lambda (x y) (* 0.5 (+ x y))) p q))
  (setq sag (distance pt chord))
  (if (and sr:envelope (> sag 1e-12))
    (progn
      (setq radius (/ (+ (* sag sag) (expt (* 0.5 (distance p q)) 2)) (* 2.0 sag))
            sag (* sag (+ 1.0 (/ sr:envelope radius))))))
  (if (and (< depth 18) (> sag tolerance))
    (append (sr:bisect-stations axis start middle p pt tolerance (1+ depth))
            (sr:bisect-stations axis middle finish pt q tolerance (1+ depth)))
    (list finish))
)

(defun sr:stations (axis start finish / kind param dist breaks first last stations tolerance)
  (setq kind (cdr (assoc 0 (entget axis))) breaks (list start finish))
  (if (equal kind "LWPOLYLINE")
    (progn
      (setq param 1)
      (while (< param (vlax-curve-getEndParam axis))
        (setq dist (vlax-curve-getDistAtParam axis param))
        (if (< start dist finish) (setq breaks (cons dist breaks)))
        (setq param (1+ param)))))
  (setq breaks (vl-sort breaks '<) first (car breaks) stations (list first)
        tolerance (* 0.005 (sr:unit-scale)))
  (foreach last (cdr breaks)
    (setq stations (append
      (if (equal kind "LINE") (list last)
        (reverse (sr:bisect-stations axis first last
          (vlax-curve-getPointAtDist axis first)
          (vlax-curve-getPointAtDist axis last) tolerance 0)))
      stations)
      first last))
  (reverse stations)
)

(defun sr:arc-offset (axis start finish offset layer / data center radius p q)
  (setq data (entget axis)
        center (cdr (assoc 10 data))
        radius (- (cdr (assoc 40 data)) offset))
  (if (<= radius 1e-8)
    (progn (princ "\nA road boundary exceeds the inside radius of the alignment.") (exit)))
  (setq p (vlax-curve-getPointAtDist axis start)
        q (vlax-curve-getPointAtDist axis finish))
  (sr:arc center radius (droad:ccw-diff 0.0 (droad:wcs-angle center p))
         (droad:ccw-diff 0.0 (droad:wcs-angle center q)) layer)
)

(defun sr:sampled-offset-at (axis station offset / length pt before after a b na nb miter divisor)
  (setq length (sr:length axis)
        pt (vlax-curve-getPointAtDist axis station)
        before (vlax-curve-getPointAtDist axis (max 0.0 (- station 0.001)))
        after (vlax-curve-getPointAtDist axis (min length (+ station 0.001)))
        a (if (equal before pt 1e-10) nil (droad:unit-vec (mapcar '- pt before)))
        b (if (equal after pt 1e-10) nil (droad:unit-vec (mapcar '- after pt))))
  (if (not a) (setq a b))
  (if (not b) (setq b a))
  (setq na (list (- (cadr a)) (car a) 0.0)
        nb (list (- (cadr b)) (car b) 0.0)
        miter (droad:unit-vec (mapcar '+ na nb))
        divisor (max 0.2 (apply '+ (mapcar '* miter na))))
  (mapcar '+ pt (mapcar '(lambda (component) (* component (/ offset divisor))) miter))
)

(defun sr:offset-at (axis station offset / data point center radius)
  (setq data (entget axis))
  (if (and (equal (cdr (assoc 0 data)) "ARC")
           (setq center (cdr (assoc 10 data)))
           (setq radius (cdr (assoc 40 data))))
    (progn
      (if (<= (- radius offset) 1e-8)
        (progn (princ "\nA road boundary exceeds the inside radius of the alignment.") (exit)))
      (setq point (vlax-curve-getPointAtDist axis station))
      (mapcar '(lambda (p c) (+ p (* (/ offset radius) (- c p)))) point center))
    (sr:sampled-offset-at axis station offset))
)

(defun sr:profile-offsets (profile / side offset sign band spec result)
  (setq sign 1.0)
  (foreach side profile
    (setq offset 0.0)
    (foreach band side
      (setq spec (assoc (car band) sr:bands)
            offset (+ offset (* sign (cadr band)))
            result (cons (list offset (strcat "SR-" (strcase (car band))) (caddr spec)) result)))
    (setq sign (- sign)))
  (reverse result)
)

(defun sr:profile-envelope (profile)
  (apply 'max (mapcar '(lambda (side) (apply '+ (cons 0.0 (mapcar 'cadr side)))) profile))
)

(defun sr:section (axis start finish profile / exact stations edge layer sr:envelope)
  (setq sr:envelope (sr:profile-envelope profile))
  (if (> (- finish start) 1e-6)
    (progn
      (setq exact (and (not sr:previewing) (equal "ARC" (cdr (assoc 0 (entget axis))))))
      (if (not exact) (setq stations (sr:stations axis start finish)))
      (foreach edge (sr:profile-offsets profile)
        (setq layer (sr:layer (cadr edge) (caddr edge)))
        (if exact
          (sr:arc-offset axis start finish (car edge) layer)
          (sr:poly (mapcar '(lambda (station) (sr:offset-at axis station (car edge))) stations)
                   nil layer)))))
)

(defun sr:pair-bands (first second sign / a b pairs kind old-offset new-offset)
  (setq old-offset 0.0 new-offset 0.0)
  (while (or first second)
    (setq a (car first) b (car second))
    (cond
      ((and a b (equal (car a) (car b)))
       (setq kind (car a)
             old-offset (+ old-offset (* sign (cadr a)))
             new-offset (+ new-offset (* sign (cadr b)))
             first (cdr first) second (cdr second)))
      ((and b (or (not a) (assoc (car a) (cdr second))))
       (setq kind (car b)
             new-offset (+ new-offset (* sign (cadr b)))
             second (cdr second)))
      (T
       (setq kind (car a)
             old-offset (+ old-offset (* sign (cadr a)))
             first (cdr first))))
    (setq pairs (cons (list old-offset new-offset kind) pairs)))
  (reverse pairs)
)

(defun sr:transition (axis start finish first second / pairs edge stations ratio sr:envelope)
  (setq sr:envelope (max (sr:profile-envelope first) (sr:profile-envelope second)))
  (if (> (- finish start) 1e-6)
    (progn
      (setq pairs (append (sr:pair-bands (car first) (car second) 1.0)
                          (sr:pair-bands (cadr first) (cadr second) -1.0))
            stations (sr:stations axis start finish))
      (foreach edge pairs
        (sr:poly
          (mapcar '(lambda (station)
            (setq ratio (/ (- station start) (- finish start)))
            (sr:offset-at axis station (+ (* (- 1.0 ratio) (car edge)) (* ratio (cadr edge)))))
            stations)
          nil (sr:layer (strcat "SR-" (strcase (caddr edge)))
                        (caddr (assoc (caddr edge) sr:bands)))))))
)

;; A span is (start end profile optional-end-profile).
(defun sr:spans (axis model / base cursor finish override begin end profile taper spans)
  (setq base (car model) cursor 0.0 finish (sr:length axis))
  (foreach override (cadr model)
    (setq begin (min finish (car override)) end (min finish (cadr override))
          profile (caddr override) taper (min (cadddr override) (* 0.5 (- end begin))))
    (if (> end begin)
      (progn
        (setq spans (cons (list cursor begin base nil) spans)
              spans (cons (list begin (+ begin taper) base profile) spans)
              spans (cons (list (+ begin taper) (- end taper) profile nil) spans)
              spans (cons (list (- end taper) end profile base) spans)
              cursor end))))
  (vl-remove-if
    '(lambda (span) (<= (- (cadr span) (car span)) 1e-6))
    (reverse (cons (list cursor finish base nil) spans)))
)

(defun sr:axis-signature (axis / item text index code first second)
  (setq first 0 second 0)
  (foreach item (entget axis)
    (if (member (car item) '(0 10 11 38 40 42 50 51 70 210))
      (progn
        (setq text (vl-prin1-to-string item) index 1)
        (repeat (strlen text)
          (setq code (ascii (substr text index 1))
                first (rem (+ (* first 127) code) 16000357)
                second (rem (+ (* second 113) code) 16003981)
                index (1+ index))))))
  (strcat (itoa first) ":" (itoa second))
)

(defun sr:span-safe-p (axis span / from to start finish sr:envelope stations spine inner outer sign good pair side)
  (setq start (car span) finish (cadr span) from (caddr span)
        to (if (cadddr span) (cadddr span) from)
        sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to))
        stations (sr:stations axis start finish)
        spine (sr:quantity-offset-vertices axis stations start finish 0.0 0.0)
        good T)
  (foreach side '(0 1)
    (setq sign (if (= side 0) 1.0 -1.0) inner spine)
    (foreach pair (sr:pair-bands (nth side from) (nth side to) sign)
      (if good
        (setq outer (sr:quantity-offset-vertices axis stations start finish (car pair) (cadr pair))
              good (sr:quantity-strip-area inner outer sign) inner outer))))
  good
)

(defun sr:render-spans (axis spans / sr:span sr:edge-signature)
  (if sr:owner (setq sr:edge-signature (sr:axis-signature axis)))
  (foreach sr:span spans
    (if (not (sr:span-safe-p axis sr:span))
      (progn (princ "\nRoad boundaries overlap or fold. Move the section break or reduce the inside width.") (exit)))
    (if (cadddr sr:span)
      (sr:transition axis (car sr:span) (cadr sr:span) (caddr sr:span) (cadddr sr:span))
      (sr:section axis (car sr:span) (cadr sr:span) (caddr sr:span))))
)

(defun sr:render (axis model)
  (sr:render-spans axis (sr:spans axis model))
)

(defun sr:span-delta (before after)
  (list (vl-remove-if '(lambda (span) (member span after)) before)
        (vl-remove-if '(lambda (span) (member span before)) after))
)

(defun sr:span-edge-count (span / first second)
  (setq first (caddr span) second (cadddr span))
  (if second
    (+ (length (sr:pair-bands (car first) (car second) 1.0))
       (length (sr:pair-bands (cadr first) (cadr second) -1.0)))
    (+ (length (car first)) (length (cadr first))))
)

(defun sr:edge-data (entity)
  (cdadr (assoc -3 (entget entity '("SR-CORRIDOR-EDGE"))))
)

(defun sr:span-inventory (axis spans edges / signature entity data extent span groups entry valid)
  (setq signature (sr:axis-signature axis) valid T)
  (foreach entity edges
    (setq data (sr:edge-data entity)
          extent (mapcar 'cdr (vl-remove-if-not '(lambda (item) (= (car item) 1040)) data))
          span nil)
    (if (and (equal signature (cdr (assoc 1000 data))) (= (length extent) 2))
      (setq span (vl-some '(lambda (piece)
                             (if (equal extent (list (car piece) (cadr piece)) 1e-8) piece))
                           spans)))
    (if span
      (if (setq entry (assoc span groups))
        (setq groups (subst (cons span (cons entity (cdr entry))) entry groups))
        (setq groups (cons (list span entity) groups)))
      (setq valid nil)))
  (if (and valid
           (vl-every '(lambda (piece)
                        (= (sr:span-edge-count piece) (length (cdr (assoc piece groups)))))
                     spans))
    groups)
)

(defun sr:model (axis / entry)
  (if (setq entry (assoc -3 (entget axis '("SR-CORRIDOR"))))
    (vl-catch-all-apply 'read
      (list (apply 'strcat (mapcar 'cdr (cdadr entry)))) ))
)

(defun sr:model-valid-p (model / cursor)
  (setq cursor 0.0)
  (and (listp model) (= 2 (length model)) (sr:profile-valid-p (car model))
       (listp (cadr model))
       (vl-every
         '(lambda (section)
            (and (listp section) (= 4 (length section))
                 (numberp (car section)) (numberp (cadr section))
                 (<= cursor (car section)) (< (car section) (cadr section))
                 (sr:profile-valid-p (caddr section))
                 (numberp (cadddr section)) (>= (cadddr section) 0.0)
                 (setq cursor (cadr section))))
         (cadr model)))
)

(defun sr:save-model (axis model / text values)
  (setq text (vl-prin1-to-string model))
  (while (> (strlen text) 0)
    (setq values (cons (cons 1000 (substr text 1 240)) values)
          text (substr text 241)))
  (entmod (append (entget axis) (list (list -3 (cons "SR-CORRIDOR" (reverse values))))))
)

(defun sr:old-edges (owner / ss i entity data result)
  (if (setq ss (ssget "_X" '((410 . "Model") (-3 ("SR-CORRIDOR-EDGE")))))
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq entity (ssname ss i) i (1+ i) data (sr:edge-data entity))
        (if (equal owner (cdr (assoc 1005 data))) (setq result (cons entity result))))))
  result
)

(defun sr:writable-p (entity / layer)
  (setq layer (tblsearch "LAYER" (cdr (assoc 8 (entget entity)))))
  (= 0 (logand 4 (cdr (assoc 70 layer))))
)

(defun sr:accept-junction-rebuild (edges / choice)
  (if (vl-some '(lambda (edge) (assoc -3 (entget edge '("SR-JUNCTION-EDIT")))) edges)
    (progn
      (initget "Yes No")
      (setq choice (getkword "\nRebuilding will replace manual junction trims on affected spans. Continue [Yes/No] <No>: "))
      (equal choice "Yes"))
    T)
)

(defun sr:apply-spans (axis model spans old / *error* sr:new sr:owner sr:previewing doc undo erased entity prior written appearance scheme paintable)
  (defun *error* (msg)
    (foreach entity sr:new (if (entget entity) (entdel entity)))
    (foreach entity erased (if (not (entget entity)) (entdel entity)))
    (if written (entmod prior))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ)
  )
  (setq sr:owner (cdr (assoc 5 (entget axis)))
        appearance (sr:style-owned sr:owner))
  (cond
    ((> (strlen (vl-prin1-to-string model)) 10000)
     (princ "\nThis alignment has too much profile data. Use a separate alignment.") nil)
    ((not (vl-every 'sr:writable-p (cons axis (append old appearance))))
     (princ "\nUnlock the alignment and affected corridor layers before building.") nil)
    ((/= 0 (logand 2 (getvar "UNDOCTL")))
     (princ "\nSet UNDO Control to All to group this corridor update.") nil)
    ((not (sr:accept-junction-rebuild old)) nil)
    (T
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object))
           sr:owner (cdr (assoc 5 (entget axis)))
           prior (entget axis '("SR-CORRIDOR")))
     (if (not (assoc -3 prior))
       (setq prior (append prior (list (list -3 (list "SR-CORRIDOR"))))))
     (if (= 0 (logand 8 (getvar "UNDOCTL")))
       (progn (vla-StartUndoMark doc) (setq undo T)))
     (regapp "SR-CORRIDOR") (regapp "SR-CORRIDOR-EDGE")
     (sr:render-spans axis spans)
     (foreach entity old
       (if (entdel entity) (setq erased (cons entity erased)) (exit)))
     (if appearance
       (progn
         (setq scheme (sr:style-scheme (car appearance)))
         (if (and scheme (setq paintable (sr:paintable-spans axis model sr:owner)))
           (sr:style-render axis paintable sr:owner scheme)
           (princ "\nAppearance cleared because some boundaries could not be verified."))
         (foreach entity appearance
           (if (entdel entity) (setq erased (cons entity erased)) (exit)))))
     (if (not (sr:save-model axis model)) (exit))
     (setq written T)
     (if undo (vla-EndUndoMark doc))
     (setq undo nil written nil sr:new nil erased nil)
     T))
)

(defun sr:build (axis model)
  (if (sr:apply-spans axis model (sr:spans axis model)
                      (sr:old-edges (cdr (assoc 5 (entget axis)))))
    (progn
      (princ "\nCorridor built. Select the original alignment with SREDIT to modify it.")
      T))
)

(defun sr:modelspace-p ()
  (cond
    ((= 1 (getvar "CVPORT")) (princ "\nSwitch to Model Space first.") nil)
    ((= 0 (logand 1 (getvar "UNDOCTL"))) (princ "\nEnable UNDO (All) first.") nil)
    (T T))
)

(defun sr:axis (/ *error* action before entity plinetype)
  (defun *error* (msg)
    (if plinetype (setvar "PLINETYPE" plinetype))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))) (princ msg))
    (princ))
  (initget "Draw Select")
  (setq action (getkword "\nAlignment [Draw/Select] <Select>: "))
  (if (equal action "Draw")
    (progn
      (setq before (entlast) plinetype (getvar "PLINETYPE"))
      (setvar "PLINETYPE" 2)
      (command "_.PLINE")
      (while (= 1 (logand 1 (getvar "CMDACTIVE"))) (command pause))
      (setvar "PLINETYPE" plinetype)
      (setq plinetype nil entity (entlast))
      (if (and entity (not (eq before entity))
               (= "LWPOLYLINE" (cdr (assoc 0 (entget entity))))
               (not (vlax-curve-isClosed entity))
               (droad:planar-center-p entity))
        entity
        (progn (princ "\nUse an open lightweight polyline in the WCS XY plane.") nil)))
    (sr:pick-axis))
)

(defun sr:create (group / profile axis)
  (if (and (setq profile (sr:configure-profile (sr:preset group))) (setq axis (sr:axis)))
    (cond
      ((sr:model axis) (princ "\nThis alignment already has a corridor. Use SREDIT."))
      ((not (sr:quick-eligible-p axis)) (princ "\nChoose an editable source alignment."))
      ((and (sr:preview axis (list profile nil)) (sr:build axis (list profile nil)))
       (sr:remember-quick-profile profile))))
)

(defun sr:range (axis / first last length)
  (setq length (sr:length axis))
  (princ (strcat "\nAlignment length: " (rtos length 2 4)))
  (initget 4)
  (setq first (getreal "\nStart distance from alignment start: "))
  (if first
    (progn
      (initget 6)
      (setq last (getreal "\nEnd distance from alignment start: "))
      (if (and last (< first last) (<= last length))
        (list first last)
        (progn (princ "\nEnter an increasing range within the alignment.") nil))))
)

(defun sr:preview (axis model / *error* sr:previewing answer)
  (defun *error* (msg) (redraw) (princ))
  (initget "Preview Build Cancel")
  (setq answer (getkword "\n[Preview/Build/Cancel] <Build>: "))
  (if (equal answer "Preview")
    (progn
      (setq sr:previewing T)
      (sr:render axis model)
      (initget "Build Cancel")
      (setq answer (getkword "\nPreview [Build/Cancel] <Cancel>: "))
      (redraw)
      (equal answer "Build"))
    (not (equal answer "Cancel")))
)

(defun sr:region-index (regions / index region)
  (setq index 0)
  (foreach region regions
    (setq index (1+ index))
    (princ (strcat "\n" (itoa index) ". "
                   (rtos (car region) 2 3) " to " (rtos (cadr region) 2 3))))
  (if regions
    (progn
      (initget 6)
      (setq index (getint "\nSection number: "))
      (if (and index (<= index (length regions))) (1- index)))
    (progn
      (princ "\nThere are no custom sections on this alignment.")
      nil))
)

(defun sr:set-region (axis regions profile / range taper)
  (cond
    ((null (setq range (sr:range axis))) nil)
    ((vl-some '(lambda (section)
                 (and (< (car section) (cadr range)) (< (car range) (cadr section))))
              regions)
     (princ "\nThe range overlaps an existing section.")
     nil)
    ((setq profile (sr:configure-profile profile))
     (setq taper (sr:number "Width transition length at each end" 0.0 T))
     (vl-sort (cons (append range (list profile taper)) regions)
              '(lambda (a b) (< (car a) (car b)))))
  )
)

(defun sr:edit (/ axis model mode profile regions index selected remaining)
  (if (and (sr:modelspace-p) (setq axis (sr:pick-axis)))
    (progn
      (setq model (sr:model axis))
      (if (sr:model-valid-p model)
        (progn
          (initget "Base Section Modify Remove Rebuild")
          (setq mode (getkword "\nEdit [Base/Section/Modify/Remove/Rebuild] <Rebuild>: ")
                regions (cadr model))
          (cond
            ((equal mode "Base")
             (setq profile (sr:configure-profile (car model))
                   model (if profile (list profile regions))))
            ((equal mode "Section")
             (setq regions (sr:set-region axis regions (car model))
                   model (if regions (list (car model) regions))))
            ((member mode '("Modify" "Remove"))
             (if (setq index (sr:region-index regions))
               (progn
                 (setq selected (nth index regions)
                       remaining (append
                                   (reverse (sr:drop (- (length regions) index) (reverse regions)))
                                   (sr:drop (1+ index) regions)))
                 (if (equal mode "Remove")
                   (setq model (list (car model) remaining))
                   (progn
                     (setq regions (sr:set-region axis remaining (caddr selected))
                           model (if regions (list (car model) regions))))))
               (setq model nil))))
          (if (and model (sr:preview axis model)) (sr:build axis model)))
        (princ "\nThis alignment has no editable Smart Road corridor."))))
  (princ)
)

;; Plan features use a user-picked local XY frame, with dimensions in drawing units.
(defun sr:local (origin direction x y)
  (list (+ (car origin) (* x (car direction)) (* y (- (cadr direction))))
        (+ (cadr origin) (* x (cadr direction)) (* y (car direction)))
        (caddr origin))
)

(defun sr:shape (origin direction plan closed layer)
  (sr:poly (mapcar '(lambda (pt) (sr:local origin direction (car pt) (cadr pt))) plan)
           closed layer)
)

(defun sr:box (origin direction x1 y1 x2 y2 layer)
  (sr:shape origin direction (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)) T layer)
)

(defun sr:plan (kind / origin point direction length width count step x y corner approach radius stem
                      opening angle theta index intersection layer half quadrant fill scale)
  (if (and (setq origin (getpoint "\nFeature insertion point: "))
           (setq point (getpoint origin "\nPick feature direction: ")))
    (progn
      (setq origin (trans origin 1 0) point (trans point 1 0)
            direction (list (- (car point) (car origin)) (- (cadr point) (cadr origin)) 0.0))
      (if (> (distance '(0.0 0.0 0.0) direction) 1e-8)
        (progn
          (setq direction (droad:unit-vec direction))
          (cond
          ((equal kind "Crosswalk")
           (sr:require-style-layer "SR-MARK-CROSSWALK")
           (if (= 0 (getvar "FILLMODE"))
             (progn (princ "\nSet FILLMODE to 1 and REGEN before painting.") (exit)))
           (setq scale (sr:unit-scale)
                 width (sr:number "Crossing span across the road" (* scale 7.0) nil)
                 length (sr:number "Crossing depth along the road" (* scale 3.0) nil)
                 count (max 1 (min 200 (fix (/ width (* scale 0.75)))))
                 step (/ width count)
                 layer (sr:layer "SR-CROSSWALK" 7)
                 fill (sr:style-layer "SR-MARK-CROSSWALK" '(248 247 240))
                 x 0.0)
           (repeat count
             (sr:box origin direction x (- (/ length 2.0)) (+ x (* step 0.55)) (/ length 2.0) layer)
             (sr:style-solid
               (list (sr:local origin direction x (- (/ length 2.0)))
                     (sr:local origin direction (+ x (* step 0.55)) (- (/ length 2.0)))
                     (sr:local origin direction (+ x (* step 0.55)) (/ length 2.0))
                     (sr:local origin direction x (/ length 2.0)))
               fill)
             (setq x (+ x step))))
          ((member kind '("Island" "Ramp" "Stop" "Platform"))
           (setq length (sr:number "Feature length"
                           (cond ((equal kind "Platform") 30.0) ((equal kind "Stop") 12.0) (T 2.0)) nil)
                 width (sr:number "Feature width" (if (equal kind "Platform") 3.0 1.5) nil)
                 half (/ width 2.0)
                 layer (sr:layer (strcat "SR-" (strcase kind))
                           (if (member kind '("Stop" "Platform")) 5 3)))
           (cond
             ((equal kind "Island")
              (setq corner (min half (* length 0.25)))
              (sr:shape origin direction
                (list '(0.0 0.0) (list corner (- half)) (list (- length corner) (- half))
                      (list length 0.0) (list (- length corner) half) (list corner half))
                T layer))
             (T
              (sr:box origin direction 0.0 (- half) length half layer)
              (if (equal kind "Ramp")
                (progn
                  (sr:shape origin direction (list (list 0.0 (- half)) (list length half)) nil layer)
                  (sr:shape origin direction (list (list 0.0 half) (list length (- half))) nil layer)))
              (if (equal kind "Stop")
                (sr:circle (sr:local origin direction 0.0 (+ half 0.45)) 0.25 layer)))))
          ((equal kind "TurnLane")
           (setq width (sr:number "Turn lane width" 3.25 nil)
                 length (sr:number "Full width length" 20.0 nil)
                 approach (sr:number "Entry taper length" 15.0 nil)
                 layer (sr:layer "SR-TURN-LANE" 2))
           (initget "Left Right")
           (if (not (equal "Left" (getkword "\nWiden to [Left/Right] <Right>: "))) (setq width (- width)))
           (sr:shape origin direction
             (list '(0.0 0.0) (list approach width) (list (+ approach length) width)) nil layer)
           (sr:shape origin direction (list '(0.0 0.0) (list (+ approach length) 0.0)) nil layer))
          ((member kind '("Tee" "Cross"))
           (setq width (sr:number "Approach width" 7.0 nil)
                 half (/ width 2.0)
                 approach (sr:number "Approach length beyond intersection" 20.0 nil)
                 corner (min approach (sr:number "Corner setback" 2.0 T))
                 stem (+ half approach)
                 x (+ half corner)
                 layer (sr:layer "SR-JUNCTION" 2))
           (foreach quadrant '((1 1) (-1 1) (-1 -1) (1 -1))
             (if (or (equal kind "Cross") (= 1 (cadr quadrant)))
               (sr:shape origin direction
                 (mapcar '(lambda (pt) (mapcar '* pt quadrant))
                   (append (list (list stem half) (list x half))
                           (if (> corner 0.0) (list (list half x)))
                           (list (list half stem))))
                 nil layer)))
           (if (equal kind "Tee")
             (sr:shape origin direction (list (list (- stem) (- half)) (list stem (- half))) nil layer)))
          ((equal kind "Roundabout")
           (setq radius (sr:number "Outer radius" 14.0 nil)
                 width (sr:number "Circulating width" (* radius 0.45) nil)
                 opening (sr:number "Approach width" (min 7.0 radius) nil)
                 approach (sr:number "Approach length beyond circle" 18.0 nil))
           (if (and (< width radius) (< opening (* radius (sqrt 2.0))))
             (progn
               (setq layer (sr:layer "SR-ROUNDABOUT" 2)
                     angle (atan (cadr direction) (car direction))
                     half (* 0.5 opening)
                     intersection (sqrt (- (* radius radius) (* half half)))
                     theta (atan half intersection)
                     index 0)
               (sr:circle origin (- radius width) layer)
               (repeat 4
                 (setq stem (+ angle (* index (/ pi 2.0))))
                 (sr:arc origin radius (rem (+ stem theta (* 2.0 pi)) (* 2.0 pi))
                         (rem (+ stem (/ pi 2.0) (- theta) (* 2.0 pi)) (* 2.0 pi)) layer)
                 (foreach y (list half (- half))
                   (sr:shape origin (list (cos stem) (sin stem) 0.0)
                     (list (list intersection y) (list (+ radius approach) y)) nil layer))
                 (setq index (1+ index))))
             (princ "\nUse a circulating width smaller than the radius and narrower approaches.")))))
        (princ "\nPick two distinct plan points."))))
  (princ)
)

(defun sr:feature (kind / *error* sr:new sr:owner sr:previewing doc undo item)
  (defun *error* (msg)
    (foreach item sr:new (if (entget item) (entdel item)))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ)
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq undo T)
  (sr:plan kind)
  (setq sr:new nil)
  (vla-EndUndoMark doc)
  (setq undo nil)
  (princ)
)

(defun c:SRROAD ()
  (if (sr:modelspace-p) (sr:create "Road"))
  (princ)
)

(defun c:SRWALK (/ option)
  (if (sr:modelspace-p)
    (progn
      (initget "Walkway Crosswalk Island Ramp")
      (setq option (getkword "\nPedestrian [Walkway/Crosswalk/Island/Ramp] <Walkway>: "))
      (if (member option '("Crosswalk" "Island" "Ramp"))
        (sr:feature option)
        (sr:create "Walkway"))))
  (princ)
)

(defun c:SRTRANSIT (/ option)
  (if (sr:modelspace-p)
    (progn
      (initget "Corridor Stop Platform")
      (setq option (getkword "\nTransit [Corridor/Stop/Platform] <Corridor>: "))
      (if (member option '("Stop" "Platform"))
        (sr:feature option)
        (sr:create "Transit"))))
  (princ)
)

(defun c:SRJUNCTION (/ option)
  (if (sr:modelspace-p)
    (progn
      (initget "Tee Cross Roundabout TurnLane")
      (setq option (getkword "\nJunction [Tee/Cross/Roundabout/TurnLane] <Cross>: "))
      (sr:feature (if option option "Cross"))))
  (princ)
)

(defun c:SREDIT () (sr:edit))

(defun sr:profile-template-key (name)
  (setq name (strcase name))
  (if (and (> (strlen name) 0) (<= (strlen name) 40)
           (not (wcmatch name "*[~A-Z0-9_]*")))
    (strcat "SmartRoad_Profile_" name))
)

(defun sr:write-profile-dialog (file / line)
  (foreach line
    '("sr_profile : dialog {"
      "  label = \"Smart Road - Cross Section\";"
      "  : text { label = \"Bands are listed from the alignment outward. Widths use drawing units.\"; }"
      "  : row {"
      "    : boxed_column { label = \"Left of alignment\";"
      "      : list_box { key = \"left\"; width = 32; height = 10; tabs = \"19\"; }"
      "    }"
      "    : boxed_column { label = \"Right of alignment\";"
      "      : list_box { key = \"right\"; width = 32; height = 10; tabs = \"19\"; }"
      "    }"
      "  }"
      "  : row {"
      "    : popup_list { key = \"kind\"; label = \"Band type\"; width = 28; }"
      "    : edit_box { key = \"width\"; label = \"Width\"; edit_width = 12; edit_limit = 24; }"
      "  }"
      "  : row {"
      "    : button { key = \"add_left\"; label = \"Add Left\"; }"
      "    : button { key = \"add_right\"; label = \"Add Right\"; }"
      "    : button { key = \"insert\"; label = \"Insert Before\"; }"
      "    : button { key = \"update\"; label = \"Update Selected\"; }"
      "    : button { key = \"remove\"; label = \"Remove\"; }"
      "  }"
      "  : boxed_row { label = \"Template\";"
      "    : edit_box { key = \"template\"; label = \"Name\"; edit_width = 19; edit_limit = 40; }"
      "    : button { key = \"load\"; label = \"Load\"; }"
      "    : button { key = \"save\"; label = \"Save\"; }"
      "    : toggle { key = \"replace\"; label = \"Replace existing\"; }"
      "  }"
      "  : text { key = \"status\"; width = 68; }"
      "  ok_cancel;"
      "}")
    (write-line line file))
)

(defun sr:profile-ui-list (key bands / band)
  (start_list key)
  (foreach band bands
    (add_list (strcat (car band) "\t" (rtos (cadr band) 2 8))))
  (end_list)
)

(defun sr:profile-ui-selection (side index / band)
  (setq sr:ui-side side
        sr:ui-index index
        band (if index (nth index (nth side sr:ui-profile))))
  (set_tile "left" (if (and band (= side 0)) (itoa index) ""))
  (set_tile "right" (if (and band (= side 1)) (itoa index) ""))
  (mode_tile "update" (if band 0 1))
  (mode_tile "remove" (if band 0 1))
  (mode_tile "insert" (if (and band (< (length (nth side sr:ui-profile)) 16)) 0 1))
  (mode_tile "add_left" (if (< (length (car sr:ui-profile)) 16) 0 1))
  (mode_tile "add_right" (if (< (length (cadr sr:ui-profile)) 16) 0 1))
  (if band
    (progn
      (set_tile "kind" (itoa (- (length sr:bands) (length (member (assoc (car band) sr:bands) sr:bands)))))
      (set_tile "width" (rtos (cadr band) 2 8))))
)

(defun sr:profile-ui-refresh (side index)
  (sr:profile-ui-list "left" (car sr:ui-profile))
  (sr:profile-ui-list "right" (cadr sr:ui-profile))
  (sr:profile-ui-selection side index)
)

(defun sr:profile-ui-pick (side value)
  (if (> (strlen value) 0)
    (progn
      (sr:profile-ui-selection side (atoi value))
      (set_tile "status" "")))
)

(defun sr:profile-splice (bands index skip insertion / prefix)
  (repeat index
    (setq prefix (cons (car bands) prefix)
          bands (cdr bands)))
  (append (reverse prefix) insertion (if skip (cdr bands) bands))
)

(defun sr:profile-ui-edit (action side / bands index kind old-band raw width updated)
  (setq bands (nth side sr:ui-profile)
        index (if (equal action "add") (length bands) sr:ui-index))
  (if (not (equal action "remove"))
    (progn
      (setq kind (car (nth (atoi (get_tile "kind")) sr:bands))
            old-band (if (equal action "update") (nth index bands))
            raw (vl-string-trim " " (get_tile "width"))
            width (if (and old-band (equal raw (rtos (cadr old-band) 2 8)))
                    (cadr old-band)
                    (distof raw 2)))))
  (cond
    ((null index) nil)
    ((and (member action '("add" "insert")) (>= (length bands) 16))
     (set_tile "status" "A side can contain up to 16 bands."))
    ((and (not (equal action "remove")) (or (null width) (<= width 0.0)))
     (set_tile "status" "Enter a positive width in drawing units.")
     (mode_tile "width" 2)
     (mode_tile "width" 3))
    (T
     (setq updated (sr:profile-splice bands index
                     (member action '("update" "remove"))
                     (if (not (equal action "remove")) (list (list kind width))))
           sr:ui-profile (if (= side 0)
                           (list updated (cadr sr:ui-profile))
                           (list (car sr:ui-profile) updated)))
     (sr:profile-ui-refresh side (if updated (min index (1- (length updated)))))
     (set_tile "status" "")))
)

(defun sr:pack-profile (profile)
  (vl-prin1-to-string (list "SR-PROFILE-1" (getvar "INSUNITS") (sr:unit-scale) profile))
)

(defun sr:unpack-profile (value legacy / record)
  (if value
    (progn
      (setq record (read value))
      (cond
        ((and (listp record) (equal (car record) "SR-PROFILE-1")
              (= (length record) 4) (numberp (caddr record)) (> (caddr record) 0.0)
              (sr:profile-valid-p (cadddr record))
              (or (= (cadr record) (getvar "INSUNITS"))
                  (and (> (cadr record) 0) (> (getvar "INSUNITS") 0))))
         (sr:scale-profile (cadddr record) (/ (sr:unit-scale) (caddr record))))
        ((and legacy (sr:profile-valid-p record)) record))))
)

(defun sr:profile-ui-read-template (value)
  (sr:unpack-profile value T)
)

(defun sr:profile-ui-template (action / key value profile)
  (setq key (sr:profile-template-key (vl-string-trim " " (get_tile "template"))))
  (cond
    ((null key)
     (set_tile "status" "Use letters, digits, and underscores for the template name.")
     (mode_tile "template" 2))
    ((equal action "load")
     (setq value (getenv key)
           profile (vl-catch-all-apply 'sr:profile-ui-read-template (list value)))
     (if (and profile (not (vl-catch-all-error-p profile)))
       (progn
         (setq sr:ui-profile profile)
         (sr:profile-ui-refresh (if (car profile) 0 1) 0)
         (set_tile "status" "Template loaded."))
       (set_tile "status" "No valid template with that name.")))
    ((not (sr:profile-valid-p sr:ui-profile))
     (set_tile "status" "Add at least one band before saving."))
    ((and (getenv key) (not (equal (get_tile "replace") "1")))
     (set_tile "status" "Enable Replace existing to overwrite this template."))
    ((setenv key (sr:pack-profile sr:ui-profile))
     (set_tile "replace" "0")
     (set_tile "status" "Template saved."))
    (T (set_tile "status" "Could not save the template.")))
)

(defun sr:profile-ui-accept ()
  (if (sr:profile-valid-p sr:ui-profile)
    (done_dialog 1)
    (set_tile "status" "Add at least one band to use this cross section."))
)

(defun sr:run-profile-dialog (path sr:ui-profile / sr:ui-side sr:ui-index band)
  (setq sr:ui-file (open path "w"))
  (if sr:ui-file
    (progn
      (sr:write-profile-dialog sr:ui-file)
      (close sr:ui-file)
      (setq sr:ui-file nil sr:ui-dcl-id (load_dialog path))))
  (if (and sr:ui-dcl-id (> sr:ui-dcl-id 0) (new_dialog "sr_profile" sr:ui-dcl-id))
    (progn
      (start_list "kind")
      (foreach band sr:bands (add_list (car band)))
      (end_list)
      (set_tile "kind" "0")
      (set_tile "width" (rtos (* (sr:unit-scale) (cadar sr:bands)) 2 8))
      (sr:profile-ui-refresh (if (car sr:ui-profile) 0 1)
                            (if (or (car sr:ui-profile) (cadr sr:ui-profile)) 0))
      (action_tile "kind" "(set_tile \"width\" (rtos (* (sr:unit-scale) (cadr (nth (atoi $value) sr:bands))) 2 8))")
      (action_tile "left" "(sr:profile-ui-pick 0 $value)")
      (action_tile "right" "(sr:profile-ui-pick 1 $value)")
      (action_tile "add_left" "(sr:profile-ui-edit \"add\" 0)")
      (action_tile "add_right" "(sr:profile-ui-edit \"add\" 1)")
      (action_tile "insert" "(sr:profile-ui-edit \"insert\" sr:ui-side)")
      (action_tile "update" "(sr:profile-ui-edit \"update\" sr:ui-side)")
      (action_tile "remove" "(sr:profile-ui-edit \"remove\" sr:ui-side)")
      (action_tile "load" "(sr:profile-ui-template \"load\")")
      (action_tile "save" "(sr:profile-ui-template \"save\")")
      (action_tile "accept" "(sr:profile-ui-accept)")
      (action_tile "cancel" "(done_dialog 0)")
      (if (= (start_dialog) 1) sr:ui-profile))
    'unavailable)
)

(defun sr:configure-dialog (profile / path sr:ui-file sr:ui-dcl-id result)
  (setq path (vl-filename-mktemp "smart-road-profile-" nil ".dcl"))
  (if path
    (progn
      (setq result (vl-catch-all-apply 'sr:run-profile-dialog (list path profile)))
      (if sr:ui-file (close sr:ui-file))
      (if (and sr:ui-dcl-id (> sr:ui-dcl-id 0)) (unload_dialog sr:ui-dcl-id))
      (if (findfile path) (vl-file-delete path))
      (if (vl-catch-all-error-p result) 'unavailable result))
    'unavailable)
)

;;; Reuse the most recently accepted corridor profile across selected alignments.

(defun sr:remember-quick-profile (profile)
  (if (sr:profile-valid-p profile)
    (vl-catch-all-apply 'setenv
      (list "SmartRoad_QuickProfile" (sr:pack-profile profile))))
)

(defun sr:quick-profile (/ profile)
  (setq profile (vl-catch-all-apply 'sr:unpack-profile
                  (list (getenv "SmartRoad_QuickProfile") nil)))
  (if (not (vl-catch-all-error-p profile)) profile)
)

(defun sr:quick-eligible-p (axis / data)
  (setq data (entget axis '("SR-CORRIDOR" "SR-CORRIDOR-EDGE" "SR-ROAD-STYLE" "DROAD_APP")))
  (and (equal "Model" (cdr (assoc 410 data)))
       (member (cdr (assoc 0 data)) '("LINE" "ARC" "LWPOLYLINE"))
       (not (assoc -3 data))
       (not (equal "ROAD-EDGE" (strcase (cdr (assoc 8 data)))))
       (sr:writable-p axis)
       (not (vlax-curve-isClosed axis))
       (droad:planar-center-p axis))
)

(defun sr:quick-axes (selection / index axis axes)
  (setq index 0.0)
  (repeat (sslength selection)
    (setq axis (ssname selection index)
          index (1+ index))
    (if (eq T (vl-catch-all-apply 'sr:quick-eligible-p (list axis)))
      (setq axes (cons axis axes))))
  (reverse axes)
)

(defun sr:quick-locked-layer (profile / record)
  (vl-some
    '(lambda (edge)
       (if (and (setq record (tblsearch "LAYER" (cadr edge)))
                (/= 0 (logand 4 (cdr (assoc 70 record)))))
         (cadr edge)))
    (sr:profile-offsets profile))
)

(defun sr:quick-clear-model (axis)
  (or (not (assoc -3 (entget axis '("SR-CORRIDOR"))))
      (entmod (append (entget axis) '((-3 ("SR-CORRIDOR"))))))
)

(defun sr:quick-build (axes profile
                       / *error* sr:new sr:owner sr:previewing
                         doc undo-open writing axis writes item result undo-failed rollback-failed locked)
  (defun *error* (msg)
    (foreach item writes
      (setq result (vl-catch-all-apply 'sr:quick-clear-model (list item)))
      (if (or (not result) (vl-catch-all-error-p result)) (setq rollback-failed T)))
    (foreach item sr:new
      (if (entget item)
        (progn
          (setq result (vl-catch-all-apply 'entdel (list item)))
          (if (or (not result) (vl-catch-all-error-p result)) (setq rollback-failed T)))))
    (if undo-open
      (setq undo-failed
        (vl-catch-all-error-p (vl-catch-all-apply 'vla-EndUndoMark (list doc)))))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "
Smart Road: " msg)))
    (if (or writing undo-open)
      (princ (if (or rollback-failed undo-failed)
               "
Quick Build could not finish cleanup. Use U to undo the batch."
               "
Quick Build stopped. No corridors from this batch were kept.")))
    (princ)
  )
  (cond
    ((null axes) nil)
    ((/= 0 (logand 2 (getvar "UNDOCTL")))
     (princ "\nSet UNDO Control to All to undo Quick Build as one operation.")
     nil)
    ((setq locked (sr:quick-locked-layer profile))
     (princ (strcat "
Unlock layer " locked " before using Quick Build."))
     nil)
    (T
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
     (if (= 0 (logand 8 (getvar "UNDOCTL")))
       (progn
         (vla-StartUndoMark doc)
         (setq undo-open T)))
     (setq writing T)
     (regapp "SR-CORRIDOR")
     (regapp "SR-CORRIDOR-EDGE")
     (foreach axis axes
       (setq sr:owner (cdr (assoc 5 (entget axis))))
       (sr:render axis (list profile nil)))
     (foreach axis axes
       (setq writes (cons axis writes))
       (if (not (sr:save-model axis (list profile nil)))
         (progn
           (princ (strcat "
Could not save the corridor data on alignment "
                          (cdr (assoc 5 (entget axis))) "."))
           (exit))))
     (if undo-open (vla-EndUndoMark doc))
     T))
)

(defun c:SRQUICK (/ filter selection axes profile default-profile skipped)
  (if (sr:modelspace-p)
    (progn
      (setq filter '((0 . "LINE,ARC,LWPOLYLINE") (410 . "Model"))
            selection (ssget "_I" filter))
      (if (not selection)
        (progn
          (princ "\nSelect unbuilt open alignments for Quick Build: ")
          (setq selection (ssget filter))))
      (if selection
        (progn
          (setq axes (sr:quick-axes selection)
                skipped (- (sslength selection) (length axes)))
          (if (> skipped 0)
            (princ (strcat "\nSkipped " (itoa skipped)
              " managed, generated, locked or unsuitable alignments.")))
          (if axes
            (progn
              (setq profile (sr:quick-profile))
              (if (not profile)
                (progn
                  (setq profile (sr:scale-default-profile '((("Lane" 3.5)) (("Lane" 3.5))))
                        default-profile T)
                  (princ "\nUsing the default profile: one 3.5-meter lane per side, converted to drawing units.")))
              (if (sr:quick-build axes profile)
                (progn
                  (if default-profile (sr:remember-quick-profile profile))
                  (princ (strcat "\nQuick Build created "
                                 (itoa (length axes)) " corridors.")))))
            (princ "\nNo eligible alignment selected."))))))
  (princ)
)

(defun sr:configure-profile (profile / result)
  (setq result (sr:configure-dialog profile))
  (if (eq result 'unavailable) (sr:configure profile) result)
)

(defun c:SRPROFILE (/ profile saved)
  (setq profile (sr:quick-profile))
  (if (not profile) (setq profile (sr:scale-default-profile '((("Lane" 3.5)) (("Lane" 3.5))))))
  (setq profile (sr:configure-profile profile))
  (if profile
    (progn
      (setq saved (sr:remember-quick-profile profile))
      (princ (if (and saved (not (vl-catch-all-error-p saved)))
               "\nQuick Build profile saved."
               "\nCould not save the Quick Build profile."))))
  (princ)
)

(defun sr:station-at-pick (axis point / projected station)
  (if (and point
           (setq projected (vlax-curve-getClosestPointTo axis (trans point 1 0)))
           (setq station (vlax-curve-getDistAtPoint axis projected)))
    (max 0.0 (min (sr:length axis) station)))
)

(defun sr:pick-range (axis / first-pick second-pick first last)
  (if (and (setq first-pick (getpoint "\nPick the start of the section on or near the alignment: "))
           (setq second-pick (getpoint first-pick "\nPick the end of the section: "))
           (setq first (sr:station-at-pick axis first-pick))
           (setq last (sr:station-at-pick axis second-pick)))
    (if (> (abs (- first last)) 1e-6)
      (list (min first last) (max first last))
      (progn (princ "\nPick two points at different distances along the alignment.") nil)))
)

(defun sr:section-profile (profile / kind side band)
  (initget "Bus Walkway Parking Cycle Profile")
  (setq kind (getkword "\nSection [Bus/Walkway/Parking/Cycle/Profile] <Profile>: "))
  (if (or (not kind) (equal kind "Profile"))
    (sr:configure-profile profile)
    (progn
      (initget "Left Right")
      (setq side (getkword "\nAdd to [Left/Right] <Right>: ")
            band (assoc kind sr:bands)
            band (list (list kind (sr:number "Band width" (* (sr:unit-scale) (cadr band)) nil)))
            profile (if (equal side "Left")
                      (list (append (car profile) band) (cadr profile))
                      (list (car profile) (append (cadr profile) band))))
      (if (sr:profile-valid-p profile)
        profile
        (progn (princ "\nA side can have up to 16 bands. Edit the full profile to replace a band.") nil))))
)

(defun sr:confirm-spans (axis spans / *error* sr:previewing answer)
  (defun *error* (msg)
    (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road preview: " msg)))
    (princ))
  (setq sr:previewing T)
  (sr:render-spans axis spans)
  (initget "Build Cancel")
  (setq answer (getkword "\nSection preview [Build/Cancel] <Build>: "))
  (redraw)
  (not (equal answer "Cancel"))
)

(defun sr:replace-section (axis before after / old-plan new-plan delta owner edges groups remove add span old)
  (setq old-plan (sr:spans axis before)
        new-plan (sr:spans axis after)
        delta (sr:span-delta old-plan new-plan))
  (if (or (car delta) (cadr delta))
    (progn
      (setq owner (cdr (assoc 5 (entget axis)))
            edges (sr:old-edges owner)
            groups (sr:span-inventory axis old-plan edges))
      (if groups
        (progn
          (setq add (cadr delta))
          (foreach span (car delta)
            (setq remove (append (cdr (assoc span groups)) remove))))
        (progn
          (princ "\nThis corridor needs a full rebuild before later edits can keep unchanged spans.")
          (setq add new-plan remove edges)))
      (if (and (sr:confirm-spans axis add) (sr:apply-spans axis after add remove))
        (princ "\nSection updated.")))
    (princ "\nThe selected section already has this geometry."))
  (princ)
)

(defun c:SRSECTION (/ axis model range regions overlapping prior profile taper updated)
  (if (and (sr:modelspace-p) (setq axis (sr:pick-axis)))
    (progn
      (setq model (sr:model axis))
      (cond
        ((not (sr:model-valid-p model))
         (princ "\nSelect the reference alignment of an existing Smart Road corridor."))
        ((setq range (sr:pick-range axis))
         (setq regions (cadr model)
               overlapping (vl-remove-if-not
                 '(lambda (section)
                    (and (< (car section) (cadr range)) (< (car range) (cadr section))))
                 regions))
         (cond
           ((cdr overlapping)
            (princ "\nThe picked range overlaps several sections. Edit them one at a time."))
           ((and overlapping
                 (progn
                   (initget "Yes No")
                   (not (equal "Yes" (getkword "\nReplace the overlapping section? [Yes/No] <No>: ")))))
            (princ "\nExisting section kept."))
           (T
            (setq prior (car overlapping))
            (if (setq profile (sr:section-profile (if prior (caddr prior) (car model))))
              (progn
                (setq taper (sr:number "Transition length at each end" (if prior (cadddr prior) 0.0) T)
                      updated (vl-sort
                                (cons (append range (list profile taper))
                                      (if prior (vl-remove prior regions) regions))
                                '(lambda (a b) (< (car a) (car b)))))
                (sr:replace-section axis model (list (car model) updated))))))))))
  (princ)
)

;; Connect two existing road edges at one junction corner.
(defun srj:dot (a b)
  (apply '+ (mapcar '* a b))
)

(defun srj:scaled (vector factor)
  (mapcar '(lambda (value) (* value factor)) vector)
)

(defun srj:cross (a b)
  (- (* (car a) (cadr b)) (* (cadr a) (car b)))
)

(defun srj:intersection (a b c d / u v denom)
  (setq u (mapcar '- b a)
        v (mapcar '- d c)
        denom (srj:cross u v))
  (if (> (abs denom) (* 1e-10 (distance a b) (distance c d)))
    (mapcar '+ a (srj:scaled u (/ (srj:cross (mapcar '- c a) v) denom))))
)

(defun srj:closest-segment (points pick / previous next delta len2 ratio closest gap best index)
  (setq index 0 previous (car points))
  (foreach next (cdr points)
    (setq delta (mapcar '- next previous)
          len2 (srj:dot delta delta))
    (if (> len2 1e-12)
      (progn
        (setq ratio (max 0.0 (min 1.0 (/ (srj:dot (mapcar '- pick previous) delta) len2)))
              closest (mapcar '+ previous (srj:scaled delta ratio))
              gap (distance pick closest))
        (if (or (null best) (< gap (car best)))
          (setq best (list gap index previous next closest)))))
    (setq previous next index (1+ index)))
  (cdr best)
)

(defun srj:straight-poly-p (data)
  (and (= 0 (logand 1 (cdr (assoc 70 data))))
       (vl-every
         '(lambda (value)
            (or (not (member (car value) '(40 41 42 43)))
                (= 0.0 (cdr value))))
         data))
)

(defun srj:points (data / elevation points value)
  (if (equal (cdr (assoc 0 data)) "LINE")
    (list (cdr (assoc 10 data)) (cdr (assoc 11 data)))
    (progn
      (setq elevation (cdr (assoc 38 data)))
      (foreach value data
        (if (= 10 (car value))
          (setq points (cons (list (cadr value) (caddr value) (if elevation elevation 0.0))
                             points))))
      (reverse points)))
)

(defun srj:pick-edge (prompt / hit entity data type application points segment)
  (if (setq hit (entsel prompt))
    (progn
      (setq entity (car hit)
            data (entget entity '("*"))
            type (cdr (assoc 0 data))
            application (cdr (assoc -3 data)))
      (cond
        ((not (equal (cdr (assoc 410 data)) "Model"))
         (princ "\nSelect an edge in Model Space.") nil)
        ((or (assoc "SR-CORRIDOR" application) (assoc "DROAD_APP" application) (assoc "SR-ROAD-STYLE" application))
         (princ "\nThe selected object is a source alignment. Select a boundary edge.") nil)
        ((not (sr:writable-p entity))
         (princ "\nUnlock this road-edge layer before connecting.") nil)
        ((and (equal type "ARC") (droad:planar-center-p entity))
         (list entity data nil (vlax-curve-getClosestPointTo entity (trans (cadr hit) 1 0))
               (assoc "SR-CORRIDOR-EDGE" application)))
        ((and (or (equal type "LINE")
                  (and (equal type "LWPOLYLINE") (srj:straight-poly-p data)))
              (droad:planar-center-p entity)
              (setq points (srj:points data))
              (setq segment (srj:closest-segment points (trans (cadr hit) 1 0))))
         (list entity data points segment (assoc "SR-CORRIDOR-EDGE" application)))
        (T
         (princ "\nUse a horizontal LINE, ARC or open straight zero-width lightweight polyline.")
         nil))))
)

(defun srj:side (points segment corner / index first second pick ray station side length)
  (setq index (car segment)
        first (cadr segment)
        second (caddr segment)
        pick (cadddr segment)
        ray (droad:unit-vec (mapcar '- second first))
        station (srj:dot (mapcar '- corner first) ray)
        length (distance first second)
        side (- (srj:dot (mapcar '- pick first) ray) station))
  (cond
    ((<= (abs side) 1e-6) nil)
    ((< side 0.0)
     (if (or (<= station (+ length 1e-6)) (= index (- (length points) 2)))
       (list index T (srj:scaled ray -1.0) station (max 0.0 (- station length)))))
    ((or (>= station -1e-6) (= index 0))
     (list index nil ray (- length station) (max 0.0 (- station)))))
)

(defun srj:prefix (items count / result)
  (repeat count
    (setq result (cons (car items) result) items (cdr items)))
  (reverse result)
)

(defun srj:trimmed-points (points side endpoint / retained)
  (if (cadr side)
    (progn
      (setq retained (srj:prefix points (1+ (car side))))
      (if (equal endpoint (nth (1- (length retained)) retained) 1e-6)
        retained
        (append retained (list endpoint))))
    (progn
      (setq retained (sr:drop (1+ (car side)) points))
      (if (equal endpoint (car retained) 1e-6)
        retained
        (cons endpoint retained))))
)

(defun srj:fillet (corner u v radius / dot sine tangent first second center a b)
  (setq dot (srj:dot u v))
  (if (and (> radius 0.0) (< (abs dot) 0.999999999))
    (progn
      (setq sine (sqrt (- 1.0 (* dot dot)))
            tangent (* radius (/ (+ 1.0 dot) sine))
            first (mapcar '+ corner (srj:scaled u tangent))
            second (mapcar '+ corner (srj:scaled v tangent))
            center (mapcar '+ corner (srj:scaled (mapcar '+ u v) (/ radius sine)))
            a (droad:wcs-angle center first)
            b (droad:wcs-angle center second))
      (if (> (droad:ccw-diff a b) pi)
        (list first second center b a)
        (list first second center a b))))
)

(defun srj:radius (first-side second-side / dot maximum radius)
  (setq dot (srj:dot (caddr first-side) (caddr second-side)))
  (if (< (abs dot) 0.999999999)
    (progn
      (setq maximum (* (min (cadddr first-side) (cadddr second-side))
                       (sqrt (/ (- 1.0 dot) (+ 1.0 dot))))
            radius (sr:number "Corner radius, 0 for square" 0.0 T))
      (while (and (> radius 0.0) (>= radius (- maximum 1e-6)))
        (princ (strcat "\nUse a smaller radius than " (rtos maximum 2 4) " for these segments."))
        (setq radius (sr:number "Corner radius, 0 for square" 0.0 T)))
      radius)
    0.0)
)

(defun srj:arc-edge-p (edge)
  (equal "ARC" (cdr (assoc 0 (cadr edge))))
)

(defun srj:pick-point (edge)
  (if (srj:arc-edge-p edge) (nth 3 edge) (cadddr (nth 3 edge)))
)

(defun srj:on-picked-segment (edge corner / segment p q)
  (if (srj:arc-edge-p edge) T
    (progn
      (setq segment (nth 3 edge) p (cadr segment) q (caddr segment))
      (equal (distance p q) (+ (distance p corner) (distance q corner)) 1e-6)))
)

(defun srj:curve-trim (edge corner / axis at pick lo hi)
  (if (srj:arc-edge-p edge)
    (progn
      (setq axis (car edge) at (vlax-curve-getDistAtPoint axis (vlax-curve-getClosestPointTo axis corner))
            pick (vlax-curve-getDistAtPoint axis (nth 3 edge)))
      (if (and at pick (> (abs (- pick at)) 1e-6))
        (progn
          (setq lo (if (< pick at) 0.0 at) hi (if (< pick at) at (sr:length axis)))
          (if (> (- hi lo) 1e-6)
            (mapcar '(lambda (station) (vlax-curve-getPointAtDist axis station))
                    (sr:stations axis lo hi))))))
    (if (setq at (srj:side (caddr edge) (nth 3 edge) corner))
      (srj:trimmed-points (caddr edge) at corner)))
)

(defun srj:curve-plan (first second / coords pt rank best corner p q)
  (setq coords (vlax-invoke (vlax-ename->vla-object (car first)) 'IntersectWith
                            (vlax-ename->vla-object (car second)) 0))
  (while coords
    (setq pt (list (car coords) (cadr coords) (caddr coords)) coords (cdddr coords)
          rank (+ (distance pt (srj:pick-point first)) (distance pt (srj:pick-point second))))
    (if (and (or (null best) (< rank best))
             (srj:on-picked-segment first pt) (srj:on-picked-segment second pt))
      (setq best rank corner pt)))
  (if (and corner (cdr (setq p (srj:curve-trim first corner)))
                  (cdr (setq q (srj:curve-trim second corner))))
    (list p q nil corner 0.0 0.0)
    (progn (princ "\nPick intersecting edges on the portions to retain.") nil))
)

(defun srj:plan (first second / a b corner first-side second-side managed radius fillet p q)
  (setq a (nth 3 first) b (nth 3 second)
        managed (or (nth 4 first) (nth 4 second)))
  (cond
    ((eq (car first) (car second))
     (princ "\nPick two different road edges.") nil)
    ((or (srj:arc-edge-p first) (srj:arc-edge-p second)) (srj:curve-plan first second))
    ((not (equal (caddr (cadr a)) (caddr (cadr b)) 1e-8))
     (princ "\nBoth edges must be at the same elevation.") nil)
    ((not (setq corner (srj:intersection (cadr a) (caddr a) (cadr b) (caddr b))))
     (princ "\nThe picked straight segments are parallel or collinear.") nil)
    ((not (and (setq first-side (srj:side (caddr first) a corner))
               (setq second-side (srj:side (caddr second) b corner))))
     (princ "\nPick a segment that reaches the corner, on the side you want to keep.") nil)
    (T
     (if managed
       (princ "\nManaged corridor edge: square trim. Rebuilding the affected span replaces the trim.")
       (if (> (setq radius (srj:radius first-side second-side)) 0.0)
         (setq fillet (srj:fillet corner (caddr first-side) (caddr second-side) radius))))
     (setq p (srj:trimmed-points (caddr first) first-side (if fillet (car fillet) corner))
           q (srj:trimmed-points (caddr second) second-side (if fillet (cadr fillet) corner)))
     (if (and (cdr p) (cdr q))
       (list p q fillet corner (nth 4 first-side) (nth 4 second-side))
       (progn (princ "\nThe proposed connection would remove an entire edge.") nil))))
)

(defun srj:vertex-ids (data / pair point ids)
  (foreach pair data
    (cond
      ((= 10 (car pair)) (setq point (cdr pair)))
      ((and point (= 91 (car pair))) (setq ids (cons (cons point (cdr pair)) ids)))))
  ids
)

(defun srj:poly-data (data points / value output inserted point coordinate vertices ids)
  (setq ids (srj:vertex-ids data))
  (foreach point points
    (setq coordinate (list (car point) (cadr point))
          vertices (cons (cons 10 coordinate) vertices))
    (if (setq value (assoc coordinate ids))
      (setq vertices (cons (cons 91 (cdr value)) vertices))))
  (foreach value data
    (cond
      ((= 90 (car value)) (setq output (cons (cons 90 (length points)) output)))
      ((= 10 (car value))
       (if (not inserted)
         (setq inserted T output (append vertices output))))
      ((not (member (car value) '(40 41 42 91)))
       (setq output (cons value output)))))
  (reverse output)
)

(defun srj:put-marker (data entry / applications)
  (setq applications (assoc -3 data))
  (if applications
    (subst (append applications (list entry)) applications data)
    (append data (list (list -3 entry))))
)

(defun srj:mark (data)
  (if (assoc "SR-JUNCTION-EDIT" (cdr (assoc -3 data)))
    data
    (srj:put-marker data (list "SR-JUNCTION-EDIT" (cons 1070 1))))
)

(defun srj:restore-edge (edge / data)
  (setq data (cadr edge))
  (if (and (nth 4 edge) (not (assoc "SR-JUNCTION-EDIT" (cdr (assoc -3 data)))))
    (setq data (srj:put-marker data (list "SR-JUNCTION-EDIT"))))
  (entmod data)
  (entupd (car edge))
)

(defun srj:modified-edge (edge points / data)
  (setq data (cadr edge))
  (cond
    ((srj:arc-edge-p edge)
     (setq data (subst (cons 50 (droad:wcs-angle (cdr (assoc 10 data)) (car points))) (assoc 50 data) data)
           data (subst (cons 51 (droad:wcs-angle (cdr (assoc 10 data)) (last points))) (assoc 51 data) data)))
    ((equal (cdr (assoc 0 data)) "LINE")
    (setq data (subst (cons 11 (cadr points)) (assoc 11 data)
                 (subst (cons 10 (car points)) (assoc 10 data) data))))
    (T (setq data (srj:poly-data data points))))
  (if (nth 4 edge) (srj:mark data) data)
)

(defun srj:draw-preview (points)
  (while (cdr points)
    (grdraw (trans (car points) 0 1) (trans (cadr points) 0 1) 3)
    (setq points (cdr points)))
)

(defun srj:arc-points (fillet / center radius start sweep index theta points)
  (setq center (caddr fillet)
        radius (distance center (car fillet))
        start (cadddr fillet)
        sweep (droad:ccw-diff start (nth 4 fillet))
        index 0)
  (repeat 19
    (setq theta (+ start (* sweep (/ index 18.0)))
          points (cons (mapcar '+ center (list (* radius (cos theta)) (* radius (sin theta)) 0.0))
                       points)
          index (1+ index)))
  (reverse points)
)

(defun srj:preview (plan / corner height size)
  (if (> (nth 4 plan) 1e-6)
    (princ (strcat "\nThe corner is " (rtos (nth 4 plan) 2 4) " units beyond the first edge.")))
  (if (> (nth 5 plan) 1e-6)
    (princ (strcat "\nThe corner is " (rtos (nth 5 plan) 2 4) " units beyond the second edge.")))
  (srj:draw-preview (car plan))
  (srj:draw-preview (cadr plan))
  (if (caddr plan) (srj:draw-preview (srj:arc-points (caddr plan))))
  (setq corner (trans (cadddr plan) 0 1)
        height (getvar "VIEWSIZE")
        size (* 0.008 height))
  (grdraw (mapcar '- corner (list size 0.0 0.0))
          (mapcar '+ corner (list size 0.0 0.0)) 2)
  (grdraw (mapcar '- corner (list 0.0 size 0.0))
          (mapcar '+ corner (list 0.0 size 0.0)) 2)
)

(defun sr:extent (data)
  (mapcar 'cdr (vl-remove-if-not '(lambda (item) (= (car item) 1040)) data))
)

(defun srj:affected-appearance (owner first second / spans tag)
  (foreach tag (list (nth 4 first) (nth 4 second))
    (if (and tag (equal owner (cdr (assoc 1005 tag))))
      (setq spans (cons (sr:extent tag) spans))))
  (vl-remove-if '(lambda (entity / extent)
    (setq extent (sr:extent (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))))
    (and extent (not (member extent spans)))) (sr:style-owned owner))
)

(defun srj:apply (first second plan / *error* doc undo first-changed second-changed arc fillet appearance erased entity owner-a owner-b sr:new legacy owner axis spans tag entry)
  (defun *error* (msg)
    (if arc (entdel arc))
    (foreach entity sr:new (if (entget entity) (entdel entity)))
    (foreach entity erased (if (not (entget entity)) (entdel entity)))
    (if first-changed (srj:restore-edge first))
    (if second-changed (srj:restore-edge second))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ))
  (setq owner-a (cdr (assoc 1005 (nth 4 first)))
        owner-b (cdr (assoc 1005 (nth 4 second)))
        appearance (append (if owner-a (srj:affected-appearance owner-a first second))
                           (if (and owner-b (not (equal owner-a owner-b))) (srj:affected-appearance owner-b first second))))
  (if (not (vl-every 'sr:writable-p appearance))
    (progn (princ "\nUnlock the road fill and marking layers first.") (exit)))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= 0 (logand 8 (getvar "UNDOCTL")))
    (progn (vla-StartUndoMark doc) (setq undo T)))
  (if (or (nth 4 first) (nth 4 second))
    (if (and (not (tblsearch "APPID" "SR-JUNCTION-EDIT")) (not (regapp "SR-JUNCTION-EDIT")))
      (progn (princ "\nUnable to register the corridor junction marker.") (exit))))
  (if (not (setq first-changed (entmod (srj:modified-edge first (car plan)))))
    (progn (princ "\nCould not modify the first edge.") (exit)))
  (if (not (setq second-changed (entmod (srj:modified-edge second (cadr plan)))))
    (progn (princ "\nCould not modify the second edge.") (exit)))
  (if (setq fillet (caddr plan))
    (if (entmake (list '(0 . "ARC") '(100 . "AcDbEntity") (assoc 8 (cadr first))
                       '(100 . "AcDbCircle") (cons 10 (caddr fillet))
                       (cons 40 (distance (caddr fillet) (car fillet)))
                       '(100 . "AcDbArc") (cons 50 (cadddr fillet)) (cons 51 (nth 4 fillet))))
      (setq arc (entlast))
      (progn (princ "\nCould not create the corner arc.") (exit))))
  (foreach entity appearance
    (setq tag (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))
          owner (cdr (assoc 1005 tag)))
    (if (and (not (sr:extent tag)) (not (assoc owner legacy)))
      (setq legacy (cons (cons owner (cdr (assoc 1000 tag))) legacy))))
  (foreach entry legacy
    (setq owner (car entry) axis (handent owner))
    (if (and axis (setq spans (sr:paintable-spans axis (sr:model axis) owner)))
      (sr:style-render axis spans owner (cdr entry))))
  (foreach entity appearance
    (if (entdel entity) (setq erased (cons entity erased)) (exit)))
  (if undo (vla-EndUndoMark doc))
  (setq undo nil first-changed nil second-changed nil arc nil erased nil)
  (if appearance (princ "\nCleared appearance on trimmed spans."))
  (entupd (car first))
  (entupd (car second))
  (princ "\nRoad edges connected.")
  (princ)
)

(defun c:SRCONNECT (/ *error* first second plan choice)
  (defun *error* (msg)
    (redraw)
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ))
  (cond
    ((not (sr:modelspace-p)) nil)
    ((/= 0 (logand 2 (getvar "UNDOCTL")))
     (princ "\nEnable UNDO (All) first."))
    ((and (setq first (srj:pick-edge "\nPick the first road edge on the side you want to keep: "))
          (setq second (srj:pick-edge "\nPick the second road edge on the side you want to keep: "))
          (setq plan (srj:plan first second)))
     (srj:preview plan)
     (initget "Apply Cancel")
     (setq choice (getkword "\nGreen shows retained geometry, yellow marks the corner [Apply/Cancel] <Cancel>: "))
     (redraw)
     (if (equal choice "Apply") (srj:apply first second plan))))
  (princ)
)

;;; Measured Smart Road corridor quantities in drawing units.

(defun sr:quantity-axis-p (entity / data)
  (and entity
       (setq data (entget entity '("SR-CORRIDOR")))
       (equal "Model" (cdr (assoc 410 data)))
       (member (cdr (assoc 0 data)) '("LINE" "ARC" "LWPOLYLINE"))
       (assoc -3 data))
)

(defun sr:quantity-select-axes (selection / index entity handle axis axes)
  (setq index 0.0)
  (if selection
    (repeat (sslength selection)
      (setq entity (ssname selection index) index (1+ index) axis nil)
      (cond
        ((sr:quantity-axis-p entity) (setq axis entity))
        ((and (setq handle (cdr (assoc 1005 (sr:edge-data entity))))
              (setq entity (handent handle))
              (sr:quantity-axis-p entity))
         (setq axis entity)))
      (if (and axis (not (member axis axes)))
        (setq axes (cons axis axes)))))
  (reverse axes)
)

(defun sr:quantity-owner-edges (axes / groups axis entry selection index edge handle)
  (foreach axis axes
    (setq groups (cons (list (strcase (cdr (assoc 5 (entget axis))))) groups)))
  (if (and groups (setq selection (ssget "_X" '((410 . "Model") (-3 ("SR-CORRIDOR-EDGE"))))))
    (progn
      (setq index 0.0)
      (repeat (sslength selection)
        (setq edge (ssname selection index) index (1+ index))
        (if (and (setq handle (cdr (assoc 1005 (sr:edge-data edge))))
                 (setq entry (assoc (strcase handle) groups)))
          (setq groups (subst (cons (car entry) (cons edge (cdr entry))) entry groups))))))
  groups
)

(defun sr:quantity-length (entity / value)
  (setq value (vl-catch-all-apply 'sr:length (list entity)))
  (if (and (numberp value) (> value 0.0)) value)
)

(defun sr:quantity-band (layer)
  (if layer
    (progn
      (setq layer (strcase layer))
      (vl-some '(lambda (spec)
                  (if (equal layer (strcat "SR-" (strcase (car spec)))) (car spec)))
               sr:bands)))
)

(defun sr:quantity-vertices (entity / data elevation normal item points valid)
  (setq data (entget entity '("SR-JUNCTION-EDIT"))
        elevation (cdr (assoc 38 data))
        normal (cdr (assoc 210 data)))
  (if (and (equal "LWPOLYLINE" (cdr (assoc 0 data)))
           (= 0 (logand 1 (cdr (assoc 70 data))))
           (not (assoc -3 data))
           (or (not normal) (equal normal '(0.0 0.0 1.0) 1e-8)))
    (progn
      (if (not elevation) (setq elevation 0.0))
      (setq valid T)
      (foreach item data
        (cond
          ((= (car item) 10)
           (setq points (cons (list (cadr item) (caddr item) elevation) points)))
          ((and (= (car item) 42) (> (abs (cdr item)) 1e-12))
           (setq valid nil))))
      (if (and valid (cdr points)) (reverse points))))
)

(defun sr:quantity-cross (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a))))
)

(defun sr:quantity-cell-area (a b c d sign / x y)
  (setq x (* sign (sr:quantity-cross a b c))
        y (* sign (sr:quantity-cross a c d)))
  (if (or (< x -1e-8) (< y -1e-8))
    (setq x (* sign (sr:quantity-cross a b d))
          y (* sign (sr:quantity-cross b c d))))
  (if (and (>= x -1e-8) (>= y -1e-8))
    (* 0.5 (max 0.0 (+ x y))))
)

(defun sr:outline-simple-p (points / first previous edges i a b x y pending other end ok)
  ;; Sort by the left edge of each segment so distant pairs are skipped.
  (setq first (car points) previous first i 0 ok T)
  (foreach b (append (cdr points) (list first))
    (if (not (equal previous b 1e-8))
      (setq edges (cons (list (min (car previous) (car b)) i previous b) edges)
            i (1+ i)))
    (setq previous b))
  (setq edges (vl-sort edges '(lambda (a b)
                (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b))))))
  (while (and edges ok)
    (setq a (caddar edges) b (nth 3 (car edges)) end (max (car a) (car b))
          pending (cdr edges))
    (while (and pending ok (<= (caar pending) end))
      (setq other (car pending) x (caddr other) y (cadddr other))
      (if (and (not (member (abs (- (cadar edges) (cadr other))) (list 1 (1- i))))
               (<= (min (cadr a) (cadr b)) (max (cadr x) (cadr y)))
               (<= (min (cadr x) (cadr y)) (max (cadr a) (cadr b)))
               (or (inters a b x y T)
                   (and (equal 0.0 (sr:quantity-cross a b x) 1e-10)
                        (equal 0.0 (sr:quantity-cross a b y) 1e-10))))
        (setq ok nil))
      (setq pending (cdr pending)))
    (setq edges (cdr edges)))
  ok
)

(defun sr:quantity-strip-area (inner outer sign / area cell valid)
  (setq area 0.0 valid (and (cdr inner) (= (length inner) (length outer))
                            (sr:outline-simple-p (append inner (reverse outer)))))
  (while (and valid (cdr inner))
    (setq cell (sr:quantity-cell-area (car inner) (cadr inner) (cadr outer) (car outer) sign))
    (if (numberp cell)
      (setq area (+ area cell) inner (cdr inner) outer (cdr outer))
      (setq valid nil)))
  (if (and valid (> area 1e-12)) area)
)

(defun sr:quantity-offset-vertices (axis stations start finish start-offset finish-offset / ratio)
  (mapcar
    '(lambda (station)
       (setq ratio (/ (- station start) (- finish start)))
       (sr:offset-at axis station (+ (* (- 1.0 ratio) start-offset) (* ratio finish-offset))))
    stations)
)

(defun sr:quantity-points-match-p (expected actual)
  (and (= (length expected) (length actual))
       (vl-every '(lambda (a b) (equal a b 1e-6)) expected actual))
)

(defun sr:legacy-stations (axis start finish / kind param distance step stations)
  (setq kind (cdr (assoc 0 (entget axis)))
        step (max 0.5 (/ (- finish start) 600.0))
        stations (list start finish))
  (if (not (equal kind "LINE"))
    (progn
      (setq distance (+ start step))
      (while (< distance finish)
        (setq stations (cons distance stations) distance (+ distance step)))))
  (if (equal kind "LWPOLYLINE")
    (progn
      (setq param 1)
      (while (< param (vlax-curve-getEndParam axis))
        (setq distance (vlax-curve-getDistAtParam axis param))
        (if (< start distance finish) (setq stations (cons distance stations)))
        (setq param (1+ param)))))
  (vl-sort stations '<)
)

(defun sr:linear-corners (points / corners pt a b direction len along)
  (foreach pt points
    (setq b (car corners) a (cadr corners))
    (if (and a (> (setq len (distance a pt)) 1e-9))
      (progn
        (setq direction (mapcar '- pt a)
              along (/ (srj:dot direction (mapcar '- b a)) (* len len)))
        (if (and (<= 0.0 along 1.0)
                 (equal b (sr:interpolate a pt along) 1e-7))
          (setq corners (cdr corners)))))
    (setq corners (cons pt corners)))
  (reverse corners)
)

(defun sr:quantity-measured-edge (kind expected candidates)
  (vl-some
    '(lambda (entry)
       (if (and (equal kind (cadr entry)) (not (equal "ARC" (cdr (assoc 0 (entget (car entry)))))))
         (cond
           ((sr:quantity-points-match-p expected (caddr entry)) entry)
           ((sr:quantity-points-match-p expected (reverse (caddr entry)))
            (list (car entry) (cadr entry) (reverse (caddr entry))))
           ((sr:quantity-points-match-p (sr:linear-corners expected)
                                        (sr:linear-corners (caddr entry))) entry)
           ((sr:quantity-points-match-p (sr:linear-corners expected)
                                        (reverse (sr:linear-corners (caddr entry)))) entry))))
    candidates)
)

(defun sr:quantity-arc-match (axis start finish pair expected candidates / center radius length)
  (setq center (cdr (assoc 10 (entget axis))) radius (cdr (assoc 40 (entget axis))))
  (if (and radius (equal (car pair) (cadr pair) 1e-10))
    (progn
      (setq length (* (- finish start) (/ (- radius (car pair)) radius)))
      (vl-some '(lambda (entry / data entity)
        (setq entity (car entry) data (entget entity))
        (if (and (equal (cadr entry) (caddr pair)) (equal "ARC" (cdr (assoc 0 data)))
                 (equal center (cdr (assoc 10 data)) 1e-6)
                 (equal (- radius (car pair)) (cdr (assoc 40 data)) 1e-6)
                 (or (not (assoc 210 data)) (equal '(0.0 0.0 1.0) (cdr (assoc 210 data)) 1e-8))
                 (equal length (sr:length entity) 1e-6)
                 (equal (car expected) (car (caddr entry)) 1e-6)
                 (equal (last expected) (last (caddr entry)) 1e-6))
          entry)) candidates)))
)

(defun sr:old-offset-points (axis stations start finish pair legacy)
  (mapcar '(lambda (station / ratio offset)
    (setq ratio (/ (- station start) (- finish start))
          offset (+ (* (- 1.0 ratio) (car pair)) (* ratio (cadr pair))))
    (if legacy (sr:sampled-offset-at axis station offset) (sr:offset-at axis station offset)))
    stations)
)

(defun sr:quantity-span-areas (axis span edges
                               / start finish from to stations axis-points edge data points candidates
                                 side sign inner pair expected match area areas valid sr:envelope legacy prior sr-radius inner-offset)
  (setq start (car span) finish (cadr span)
        from (caddr span) to (if (cadddr span) (cadddr span) from)
        sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to))
        sr-radius (if (equal "ARC" (cdr (assoc 0 (entget axis)))) (cdr (assoc 40 (entget axis))))
        stations (sr:stations axis start finish)
        axis-points (sr:quantity-offset-vertices axis stations start finish 0.0 0.0)
        valid T)
  (foreach edge edges
    (if (and (setq points (if (and (equal "ARC" (cdr (assoc 0 (entget edge))))
                                         (not (assoc -3 (entget edge '("SR-JUNCTION-EDIT")))))
                                  (list (vlax-curve-getStartPoint edge) (vlax-curve-getEndPoint edge))
                                  (sr:quantity-vertices edge)))
             (setq data (sr:quantity-band (cdr (assoc 8 (entget edge))))))
      (setq candidates (cons (list edge data points) candidates))
      (setq valid nil)))
  (foreach side '(0 1)
    (setq sign (if (= side 0) 1.0 -1.0) inner axis-points inner-offset 0.0)
    (foreach pair (sr:pair-bands (nth side from) (nth side to) sign)
      (if valid
        (progn
          (setq expected (sr:quantity-offset-vertices axis stations start finish (car pair) (cadr pair))
                match (sr:quantity-measured-edge (caddr pair) expected candidates))
          (if (and sr-radius (not match))
            (setq match (sr:quantity-arc-match axis start finish pair expected candidates)))
          (if (not match)
            (progn
              (if (not prior)
                (progn (setq sr:envelope nil prior (sr:stations axis start finish))
                       (setq sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to)))))
              (setq match (sr:quantity-measured-edge (caddr pair)
                            (sr:old-offset-points axis prior start finish pair nil) candidates))))
          (if (not match)
            (progn
              (if (not legacy) (setq legacy (sr:legacy-stations axis start finish)))
              (setq match (sr:quantity-measured-edge (caddr pair)
                            (sr:old-offset-points axis legacy start finish pair T) candidates))))
          (if (and match (setq area (sr:quantity-strip-area inner expected sign)))
            (setq areas (cons (cons (caddr pair)
                          (if (and sr-radius (not (cadddr span))
                                   (equal "ARC" (cdr (assoc 0 (entget (car match))))))
                            (* (- finish start) (abs (- (car pair) inner-offset))
                               (- 1.0 (/ (+ (car pair) inner-offset) (* 2.0 sr-radius))))
                            area)) areas)
                  inner expected inner-offset (car pair)
                  candidates (vl-remove-if '(lambda (record) (eq (car record) (car match))) candidates))
            (setq valid nil))))))
  (if (and valid (not candidates)) areas)
)

(defun sr:quantity-axis-areas (axis model edges / spans inventory span pieces areas valid)
  (setq spans (sr:spans axis model)
        inventory (sr:span-inventory axis spans edges)
        valid (and spans inventory))
  (foreach span spans
    (if valid
      (if (setq pieces (sr:quantity-span-areas axis span (cdr (assoc span inventory))))
        (setq areas (append pieces areas))
        (setq valid nil))))
  (if valid areas)
)

(defun sr:quantity-add (rows kind count length area / record)
  (if (setq record (assoc kind rows))
    (subst (list kind (+ count (cadr record)) (+ length (caddr record)) (+ area (cadddr record)))
           record rows)
    (cons (list kind count length area) rows))
)

(defun sr:quantity-profile-kinds (model / profile side band kinds)
  (foreach profile (cons (car model) (mapcar 'caddr (cadr model)))
    (foreach side profile
      (foreach band side
        (if (not (member (car band) kinds))
          (setq kinds (cons (car band) kinds))))))
  kinds
)

(defun sr:quantity-report (axes / axis seen groups handle edges edge kind measured
                                model model-valid areas area rows ordered spec count incomplete
                                valid edge-kinds unknown-kinds unknown-all)
  (setq count 0 incomplete 0)
  (foreach axis axes
    (if (and (sr:quantity-axis-p axis) (not (member axis seen)))
      (setq seen (cons axis seen))))
  (setq groups (sr:quantity-owner-edges seen))
  (foreach axis seen
    (setq count (1+ count)
          handle (strcase (cdr (assoc 5 (entget axis))))
          edges (cdr (assoc handle groups))
          measured (sr:quantity-length axis)
          valid (numberp measured)
          edge-kinds nil)
    (if valid (setq rows (sr:quantity-add rows "Alignment" 1 measured 0.0)))
    (foreach edge edges
      (setq kind (sr:quantity-band (cdr (assoc 8 (entget edge))))
            measured (sr:quantity-length edge))
      (if (and kind (not (member kind edge-kinds)))
        (setq edge-kinds (cons kind edge-kinds)))
      (if (and kind measured)
        (setq rows (sr:quantity-add rows kind 1 measured 0.0))
        (setq valid nil)))
    (setq model (vl-catch-all-apply 'sr:model (list axis))
          model-valid (eq T (vl-catch-all-apply 'sr:model-valid-p (list model)))
          areas nil)
    (if (and valid model-valid)
      (setq areas (vl-catch-all-apply 'sr:quantity-axis-areas (list axis model edges))))
    (if (and (listp areas) areas)
      (foreach area areas
        (setq rows (sr:quantity-add rows (car area) 0 0.0 (cdr area))))
      (progn
        (setq incomplete (1+ incomplete))
        (if model-valid
          (foreach kind (append (sr:quantity-profile-kinds model) edge-kinds)
            (if (not (member kind unknown-kinds))
              (setq unknown-kinds (cons kind unknown-kinds))))
          (setq unknown-all T)))))
  (foreach spec (cons '("Alignment") sr:bands)
    (if (setq area (assoc (car spec) rows))
      (setq ordered (cons
        (list (car area) (cadr area) (caddr area)
              (if (and (not unknown-all)
                       (not (member (car area) (cons "Alignment" unknown-kinds))))
                (cadddr area)))
        ordered))))
  (list count incomplete (reverse ordered))
)

;; Measure selected Model Space curves by layer, independent of corridor ownership.
(defun sr:quantity-existing-item (entity / data kind normal end length closed area)
  (setq data (entget entity)
        kind (cdr (assoc 0 data))
        normal (cdr (assoc 210 data)))
  (if (and (equal (cdr (assoc 410 data)) "Model")
           (member kind '("LINE" "ARC" "LWPOLYLINE" "CIRCLE" "ELLIPSE"))
           (if (equal kind "LINE")
             (equal (caddr (cdr (assoc 10 data))) (caddr (cdr (assoc 11 data))) 1e-8)
             (or (not normal) (equal normal '(0.0 0.0 1.0) 1e-8)
                 (equal normal '(0.0 0.0 -1.0) 1e-8)))
           (setq end (vlax-curve-getEndParam entity))
           (setq length (vlax-curve-getDistAtParam entity end))
           (> length 1e-6))
    (progn
      (setq closed (vlax-curve-isClosed entity))
      (if closed
        (progn
          (setq area (vl-catch-all-apply 'vla-get-Area (list (vlax-ename->vla-object entity))))
          (if (or (not (numberp area)) (<= area 0.0)) (setq area nil))))
      (list (strcat (cdr (assoc 8 data)) (if closed " / Closed" " / Open"))
            length area closed)))
)

(defun sr:quantity-existing-report (selection / index entity measure rows row key area count incomplete)
  (setq index 0.0 count 0 incomplete 0)
  (repeat (sslength selection)
    (setq entity (ssname selection index) index (1+ index)
          measure (vl-catch-all-apply 'sr:quantity-existing-item (list entity)))
    (if (and measure (not (vl-catch-all-error-p measure)))
      (progn
        (setq key (car measure) area (caddr measure) row (assoc key rows)
              count (1+ count))
        (if (and (cadddr measure) (not area)) (setq incomplete (1+ incomplete)))
        (if row
          (setq rows
            (subst
              (list key (1+ (cadr row)) (+ (caddr row) (cadr measure))
                    (if (and (cadddr row) area) (+ (cadddr row) area)))
              row rows))
          (setq rows (cons (list key 1 (cadr measure) area) rows))))
      (setq incomplete (1+ incomplete))))
  (list count incomplete (vl-sort rows '(lambda (a b) (< (strcase (car a)) (strcase (car b))))))
)

(defun sr:quantity-unit ()
  (cdr (assoc (getvar "INSUNITS")
    '((0 . "du") (1 . "in") (2 . "ft") (3 . "mi") (4 . "mm") (5 . "cm")
      (6 . "m") (7 . "km") (8 . "microin") (9 . "mil") (10 . "yd")
      (11 . "angstrom") (12 . "nm") (13 . "um") (14 . "dm") (15 . "dam")
      (16 . "hm") (17 . "Gm") (18 . "AU") (19 . "ly") (20 . "pc")
      (21 . "us-ft") (22 . "us-in") (23 . "us-yd") (24 . "us-mi"))))
)

(defun sr:quantity-columns (scope unit)
  (list (if (equal scope "Existing") "Layer / item" "Item")
        "Entities"
        (strcat (if (equal scope "Existing") "Length / perimeter (" "Length (") unit ")")
        (strcat "Gross plan area (" unit "^2)"))
)

(defun sr:quantity-text (value)
  (cond
    ((null value) "")
    ((eq 'INT (type value)) (itoa value))
    ((numberp value) (rtos value 2 8))
    (T value))
)

(defun sr:quantity-note (scope report)
  (strcat (itoa (car report))
          (if (equal scope "Existing") " selected curve(s)" " corridor(s)")
          ", " (itoa (cadr report)) " incomplete. Blank area means unavailable or not applicable.")
)

(defun sr:quantity-print (scope unit report / row)
  (princ (strcat "\nSmart Road Quantities - " scope "\n"
                 (sr:quantity-note scope report)))
  (princ (strcat "\nLength: " unit " | Gross plan area: " unit "^2"))
  (if (equal unit "du") (princ " (du = drawing units)"))
  (princ (if (equal scope "Existing")
           "\nClosed curve length is its perimeter. Gross area sums closed curves."
           "\nEdge lengths follow the measured curves. Gross area sums complete bands."))
  (foreach row (caddr report)
    (princ (strcat "\n  " (car row) "  |  " (itoa (cadr row))
                   "  |  " (sr:quantity-text (caddr row))
                   "  |  " (if (cadddr row) (sr:quantity-text (cadddr row)) "-"))))
)

(defun sr:quantity-csv-field (value / trimmed first quote-index)
  (cond
    ((null value) "")
    ((numberp value) (vl-string-subst "." "," (sr:quantity-text value)))
    (T
     (setq trimmed (vl-string-left-trim " \t\r\n" value)
           first (substr trimmed 1 1))
     (if (or (member first '("=" "+" "-" "@"))
             (and (> (strlen value) 0) (< (ascii value) 32))
             (and (> (strlen trimmed) 0) (< (ascii trimmed) 32)))
       (setq value (strcat "'" value)))
     (setq quote-index (vl-string-search "\"" value))
     (while quote-index
       (setq value (vl-string-subst "\"\"" "\"" value quote-index)
             quote-index (vl-string-search "\"" value (+ quote-index 2))))
     (strcat "\"" value "\"")))
)

(defun sr:quantity-csv-line (values / text value)
  (setq text (sr:quantity-csv-field (car values)))
  (foreach value (cdr values)
    (setq text (strcat text "," (sr:quantity-csv-field value))))
  text
)

(defun sr:quantity-write-csv (temp scope unit report / row)
  (if (setq sr:qty-stream (open temp "w"))
    (progn
      (write-line (sr:quantity-csv-line
                    (append (list "Scope") (sr:quantity-columns scope unit)
                            (list "Incomplete sources"))) sr:qty-stream)
      (foreach row (caddr report)
        (write-line (sr:quantity-csv-line
                      (append (list scope) row (list (cadr report)))) sr:qty-stream))
      (close sr:qty-stream)
      (setq sr:qty-stream nil)
      T))
)

(defun sr:quantity-export (path scope unit report / temp sr:qty-stream result)
  (setq temp (vl-filename-mktemp "sr-quantities-" (vl-filename-directory path) ".tmp"))
  (if temp
    (progn
      (setq result (vl-catch-all-apply 'sr:quantity-write-csv (list temp scope unit report)))
      (if sr:qty-stream (vl-catch-all-apply 'close (list sr:qty-stream)))
      (if (eq result T)
        (setq result (vl-catch-all-apply 'vl-file-rename (list temp path)))
        (setq result nil))
      (if (findfile temp) (vl-catch-all-apply 'vl-file-delete (list temp)))
      (eq result T)))
)

(defun sr:quantity-path (/ path searching)
  (setq searching T)
  (while searching
    (setq path (getfiled "Export quantities to a new CSV file" "Smart Road Quantities.csv" "csv" 129))
    (if (and path (findfile path))
      (princ "\nThis file already exists. Choose a new filename for the export.")
      (setq searching nil)))
  path
)

(defun sr:quantity-table-row (table index values / column value)
  (setq column 0)
  (foreach value values
    (vla-SetText table index column (sr:quantity-text value))
    (setq column (1+ column)))
)

(defun sr:quantity-table (doc point scope unit report / row index height note-row suppress)
  (setq height (max 1e-6 (getvar "TEXTSIZE"))
        note-row (+ 2 (length (caddr report)))
        sr:qty-table (vla-AddTable (vla-get-ModelSpace doc) (vlax-3d-point (trans point 1 0))
                                  (1+ note-row) 4 (* height 2.2) (* height 20.0))
        suppress (vlax-property-available-p sr:qty-table "RegenerateTableSuppressed" T))
  (if suppress (vla-put-RegenerateTableSuppressed sr:qty-table :vlax-true))
  (vla-put-TitleSuppressed sr:qty-table :vlax-false)
  (vla-put-HeaderSuppressed sr:qty-table :vlax-false)
  (vla-SetText sr:qty-table 0 0 (strcat "Smart Road Quantities - " scope))
  (sr:quantity-table-row sr:qty-table 1 (sr:quantity-columns scope unit))
  (setq index 2)
  (foreach row (caddr report)
    (sr:quantity-table-row sr:qty-table index row)
    (setq index (1+ index)))
  (vla-MergeCells sr:qty-table note-row note-row 0 3)
  (vla-SetText sr:qty-table note-row 0 (sr:quantity-note scope report))
  (if suppress (vla-put-RegenerateTableSuppressed sr:qty-table :vlax-false))
  (vla-Update sr:qty-table)
)

(defun sr:quantity-output (point path scope unit report / *error* doc undo sr:qty-table)
  (defun *error* (msg)
    (if (and sr:qty-table
             (vl-catch-all-error-p (vl-catch-all-apply 'vla-Delete (list sr:qty-table))))
      (princ "\nCould not remove the unfinished table. Use U to undo its insertion."))
    (if undo (vl-catch-all-apply 'vla-EndUndoMark (list doc)))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road quantities: " msg)))
    (princ))
  (if point
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (if (= 0 (logand 8 (getvar "UNDOCTL")))
        (progn
          (vla-StartUndoMark doc)
          (setq undo T)))
      (sr:quantity-table doc point scope unit report)))
  (if (or (null path) (sr:quantity-export path scope unit report))
    (progn
      (if undo (vla-EndUndoMark doc))
      (if point (princ "\nQuantity table inserted."))
      (if path (princ (strcat "\nCSV saved: " path))))
    (*error* "CSV export failed. Check the destination and try again."))
  (princ)
)

(defun sr:quantity-table-ready-p ()
  (if (sr:modelspace-p)
    (if (= 0 (logand 2 (getvar "UNDOCTL")))
      T
      (progn
        (princ "\nSet UNDO Control to All to insert a quantity table. CSV can be exported separately.")
        nil)))
)

(defun c:SRQUANTITY (/ selection scope report unit choice point path)
  (cond
    ((= 1 (getvar "CVPORT")) (princ "\nSwitch to Model Space or enter a model viewport."))
    (T
     (setq selection (ssget "_I"))
     (initget "Corridors Existing")
     (setq scope (getkword "\nQuantities for [Corridors/Existing] <Corridors>: "))
     (if (null scope) (setq scope "Corridors"))
     (if (null selection)
       (progn
         (princ (if (equal scope "Existing")
                  "\nSelect existing curves for quantities."
                  "\nSelect alignments or generated edges. Each linked edge selects its entire corridor."))
         (setq selection (ssget '((0 . "LINE,ARC,LWPOLYLINE,CIRCLE,ELLIPSE") (410 . "Model"))))))
     (if selection
       (progn
         (setq report (if (equal scope "Existing")
                        (sr:quantity-existing-report selection)
                        (sr:quantity-report (sr:quantity-select-axes selection))))
         (if (caddr report)
           (progn
             (setq unit (sr:quantity-unit))
             (if (null unit) (setq unit "du"))
             (sr:quantity-print scope unit report)
             (initget "Table CSV Both Exit")
             (setq choice (getkword "\nOutput [Table/CSV/Both/Exit] <Exit>: "))
             (cond
               ((equal choice "CSV")
                (if (setq path (sr:quantity-path))
                  (sr:quantity-output nil path scope unit report)))
               ((and (member choice '("Table" "Both")) (sr:quantity-table-ready-p)
                     (setq point (getpoint "\nPlace quantity table: ")))
                (if (or (equal choice "Table") (setq path (sr:quantity-path)))
                  (sr:quantity-output point path scope unit report)))))
           (princ "\nNo measurable curves in that selection."))))))
  (princ)
)

;; Presentation objects use their own XData so quantity scans still see only corridor edges.
(defun sr:require-style-layer (name / existing)
  (if (and (setq existing (tblsearch "LAYER" name))
           (or (< (cdr (assoc 62 existing)) 0) (/= 0 (logand 5 (cdr (assoc 70 existing))))))
    (progn (princ (strcat "\nTurn on, thaw and unlock " name " before painting.")) (exit)))
)

(defun sr:style-layer (name rgb / existing true-color)
  (sr:require-style-layer name)
  (setq existing (tblsearch "LAYER" name)
        true-color (+ (* (car rgb) 65536) (* (cadr rgb) 256) (caddr rgb)))
  (cond
    ((not existing)
     (if (not (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                             '(100 . "AcDbLayerTableRecord") (cons 2 name)
                             '(70 . 0) '(62 . 7) (cons 420 true-color) '(6 . "Continuous"))))
       (exit))))
  name
)

(defun sr:style-track (/ entity tag)
  (setq entity (entlast) sr:new (cons entity sr:new))
  (if sr:appearance-owner
    (progn
      (setq tag (list "SR-ROAD-STYLE" (cons 1005 sr:appearance-owner)
                                     (cons 1000 sr:appearance-scheme)
                                     (cons 1040 (car sr:appearance-span))
                                     (cons 1040 (cadr sr:appearance-span))))
      (if (not (entmod (append (entget entity) (list (list -3 tag))))) (exit))))
  entity
)

(defun sr:style-solid (points layer / point vertices unique)
  (foreach point points
    (if (not (equal point (car unique) 1e-8)) (setq unique (cons point unique))))
  (if (equal (car unique) (car points) 1e-8) (setq unique (cdr unique)))
  (setq points (reverse unique))
  (if (>= (length points) 3)
    (progn
      (foreach point points
        (setq vertices (cons (cons 10 (list (car point) (cadr point))) vertices)))
      (if (entmake
            (append
              (list '(0 . "HATCH") '(100 . "AcDbEntity") (cons 8 layer)
                    '(100 . "AcDbHatch") (cons 10 (list 0.0 0.0 (caddr (car points))))
                    '(210 0.0 0.0 1.0) '(2 . "SOLID") '(70 . 1) '(71 . 0) '(91 . 1)
                    '(92 . 3) '(72 . 0) '(73 . 1) (cons 93 (length points)))
              (reverse vertices) '((97 . 0) (75 . 0) (76 . 1) (98 . 0))))
        (progn
          (setq point (sr:style-track))
          (if sr:appearance-hatches (ssadd point sr:appearance-hatches))
          point)
        (progn (princ "\nAutoCAD could not create a solid hatch.") (exit)))))
)

(defun sr:style-line (points width layer / point vertices)
  (foreach point points
    (setq vertices (cons (cons 10 (list (car point) (cadr point))) vertices)))
  (if (entmake (append
                 (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                       '(100 . "AcDbPolyline") (cons 90 (length points)) '(70 . 0)
                       (cons 43 width) (cons 38 (caddr (car points))))
                 (reverse vertices)))
    (progn (setq point (sr:style-track))
           (if sr:appearance-lines (ssadd point sr:appearance-lines)) point)
    (exit))
)

(defun sr:style-material (kind)
  (cond ((member kind '("Lane" "Parking" "Shoulder")) '("ASPHALT" 57 61 65))
        ((member kind '("Walkway" "Curb" "Platform")) '("CONCRETE" 188 182 171))
        ((equal kind "Median") '("MEDIAN" 121 138 101))
        ((equal kind "Cycle") '("CYCLE" 73 134 100))
        ((member kind '("Bus" "BRT")) '("TRANSIT" 149 79 63)))
)

(defun sr:style-dash-ranges (start finish dash gap / period base first last ranges)
  (setq period (+ dash gap) base (* period (fix (/ start period))))
  (if (> (/ (- finish base) period) 10000.0)
    (progn (princ "\nToo many dashes for one span. Verify INSUNITS or split the alignment.") (exit)))
  (while (< base finish)
    (setq first (max start base) last (min finish (+ base dash)))
    (if (> (- last first) 1e-6) (setq ranges (cons (list first last) ranges)))
    (setq base (+ base period)))
  (reverse ranges)
)

(defun sr:interpolate (a b ratio)
  (mapcar '(lambda (x y) (+ x (* ratio (- y x)))) a b)
)

;; Split the actual painted path. Keep the finishing phase for a matching
;; boundary at the next span, including across a widening or taper.
(defun sr:style-stripe (axis stations start finish first last dashed yellow
                        / scale layer width points lengths total pieces ranges a b len
                          base lo hi path previous key radius factor interval remaining)
  (setq scale (sr:unit-scale)
        layer (if yellow (sr:style-layer "SR-MARK-YELLOW" '(244 192 54))
                         (sr:style-layer "SR-MARK-WHITE" '(248 247 240)))
        width (* 0.12 scale)
        points (sr:quantity-offset-vertices axis stations start finish first last))
  (if (not dashed)
    (sr:style-line points width layer)
    (progn
      (setq key (list start first yellow) previous (assoc key sr:dash-phases)
            base (if previous (cdr previous) 0.0) total base)
      (if (and (equal first last 1e-10) (equal "ARC" (cdr (assoc 0 (entget axis)))))
        (setq radius (cdr (assoc 40 (entget axis))) factor (/ (- radius first) radius)))
      (setq lengths stations)
      (while (cdr points)
        (setq a (car points) b (cadr points)
              len (if factor (* factor (- (cadr lengths) (car lengths))) (distance a b)))
        (if (> len 1e-10)
          (setq pieces (cons (list total (+ total len) a b) pieces) total (+ total len)))
        (setq points (cdr points) lengths (cdr lengths)))
      (setq sr:dash-phases (cons (cons (list finish last yellow) total) sr:dash-phases)
            pieces (reverse pieces)
            ranges (sr:style-dash-ranges base total (* 3.0 scale) (* 6.0 scale)))
      (foreach interval ranges
        (while (and pieces (<= (cadar pieces) (car interval))) (setq pieces (cdr pieces)))
        (setq path nil)
        (while (and pieces (< (caar pieces) (cadr interval)))
          (setq a (caddar pieces) b (nth 3 (car pieces)) base (caar pieces)
                len (- (cadar pieces) base)
                lo (max base (car interval)) hi (min (cadar pieces) (cadr interval)))
          (if (not path) (setq path (list (sr:interpolate a b (/ (- lo base) len)))))
          (setq path (cons (sr:interpolate a b (/ (- hi base) len)) path))
          (if (<= (cadar pieces) (cadr interval)) (setq pieces (cdr pieces))
            (setq remaining pieces pieces nil)))
        (if (cdr path) (sr:style-line (reverse path) width layer))
        (if remaining (setq pieces remaining remaining nil)))))
)
(defun sr:style-road-band-p (kind)
  (member kind '("Lane" "Bus" "BRT"))
)

(defun sr:style-spans (axis spans / span start finish from to stations index sign points pairs edge material
                                  current next inner outer center-offset sr:dash-phases sr:envelope previous
                                  inner-first inner-last opposite sr:appearance-span)
  (foreach span spans
    (setq start (car span) finish (cadr span) from (caddr span)
          to (if (cadddr span) (cadddr span) from)
          sr:appearance-span (list start finish)
          sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to))
          stations (sr:stations axis start finish)
          points (sr:quantity-offset-vertices axis stations start finish 0.0 0.0))
    (foreach index '(0 1)
      (setq sign (if (= index 0) 1.0 -1.0) inner points previous nil inner-first 0.0 inner-last 0.0
            opposite (sr:pair-bands (nth (- 1 index) from) (nth (- 1 index) to) (- sign))
            pairs (sr:pair-bands (nth index from) (nth index to) sign))
      (while pairs
        (setq edge (car pairs) current (caddr edge) next (caddr (cadr pairs))
              outer (sr:quantity-offset-vertices axis stations start finish (car edge) (cadr edge))
              material (sr:style-material current))
        (if (and material (sr:quantity-strip-area inner outer sign))
          (sr:style-solid (append inner (reverse outer))
            (sr:style-layer (strcat "SR-FILL-" (car material)) (cdr material))))
        (if (and (sr:style-road-band-p current) (not (sr:style-road-band-p previous))
                 (or previous (not (sr:style-road-band-p (caddar opposite)))))
          (sr:style-stripe axis stations start finish inner-first inner-last nil nil))
        (if (sr:style-road-band-p current)
          (sr:style-stripe axis stations start finish (car edge) (cadr edge)
                           (sr:style-road-band-p next) nil))
        (setq inner outer previous current inner-first (car edge) inner-last (cadr edge) pairs (cdr pairs))))
    (if (and (not (equal sr:appearance-scheme "NoCenter"))
             (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) from)
             (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) to))
      (if (equal sr:appearance-scheme "DoubleYellow")
        (progn
          (setq center-offset (* 0.12 (sr:unit-scale)))
          (sr:style-stripe axis stations start finish center-offset center-offset nil T)
          (sr:style-stripe axis stations start finish (- center-offset) (- center-offset) nil T))
        (sr:style-stripe axis stations start finish 0.0 0.0 T
                         (not (equal sr:appearance-scheme "WhiteDash"))))))
)

(defun sr:style-owned (owner / set index entity data result)
  (if (setq set (ssget "_X" '((410 . "Model") (-3 ("SR-ROAD-STYLE")))))
    (progn
      (setq index 0.0)
      (repeat (sslength set)
        (setq entity (ssname set index) index (1+ index)
              data (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE")))))
        (if (equal owner (cdr (assoc 1005 data))) (setq result (cons entity result))))))
  result
)

(defun sr:paintable-spans (axis model owner / span spans groups edges good invalid)
  (setq spans (sr:spans axis model)
        groups (sr:span-inventory axis spans (sr:old-edges owner)))
  (if groups
    (foreach span spans
      (setq edges (cdr (assoc span groups)))
      (cond
        ((vl-some '(lambda (edge) (assoc -3 (entget edge '("SR-JUNCTION-EDIT")))) edges)
         (princ "\nSkipping manually trimmed span."))
        ((sr:quantity-span-areas axis span edges) (setq good (cons span good)))
        (T (setq invalid T)))))
  (if (not invalid) (reverse good))
)

(defun sr:check-paint-layers (spans scheme / span profile side band material)
  (if (= (getvar "FILLMODE") 0)
    (progn (princ "\nSet FILLMODE to 1 and REGEN before painting.") (exit)))
  (foreach span spans
    (foreach profile (list (caddr span) (cadddr span))
      (foreach side profile
        (foreach band side
          (if (setq material (sr:style-material (car band)))
            (sr:require-style-layer (strcat "SR-FILL-" (car material))))
          (if (sr:style-road-band-p (car band)) (sr:require-style-layer "SR-MARK-WHITE")))))
    (if (and (member scheme '("YellowDash" "DoubleYellow"))
             (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) (caddr span))
             (or (not (cadddr span))
                 (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) (cadddr span))))
      (sr:require-style-layer "SR-MARK-YELLOW")))
)

(defun sr:style-render (axis spans owner scheme / sr:appearance-owner sr:appearance-scheme sr:appearance-hatches sr:appearance-lines)
  (setq sr:appearance-owner owner sr:appearance-scheme scheme sr:appearance-hatches (ssadd) sr:appearance-lines (ssadd))
  (sr:check-paint-layers spans scheme)
  (regapp "SR-ROAD-STYLE")
  (sr:style-spans axis spans)
  (if (> (sslength sr:appearance-hatches) 0)
    (vl-cmdf "_.DRAWORDER" sr:appearance-hatches "" "_Front"))
  (if (> (sslength sr:appearance-lines) 0)
    (vl-cmdf "_.DRAWORDER" sr:appearance-lines "" "_Front"))
)

(defun sr:style-scheme (entity)
  (cdr (assoc 1000 (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))))
)

(defun sr:style-change (axis model scheme / *error* doc undo sr:new old erased entity owner paintable)
  (defun *error* (msg)
    (foreach entity sr:new (if (entget entity) (entdel entity)))
    (foreach entity erased (if (not (entget entity)) (entdel entity)))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*"))) (princ (strcat "\nSmart Road: " msg)))
    (princ))
  (setq owner (cdr (assoc 5 (entget axis))) old (sr:style-owned owner))
  (cond
    ((not (vl-every 'sr:writable-p old))
     (princ "\nUnlock the Smart Road fill and marking layers first."))
    ((and scheme (not (setq paintable (sr:paintable-spans axis model owner))))
     (princ "\nAppearance needs complete corridor boundaries. Rebuild or repair the affected edges first."))
    (T
     (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
     (if (= 0 (logand 8 (getvar "UNDOCTL")))
       (progn (vla-StartUndoMark doc) (setq undo T)))
     (if scheme (sr:style-render axis paintable owner scheme))
     (foreach entity old
       (if (entdel entity) (setq erased (cons entity erased)) (exit)))
     (if undo (vla-EndUndoMark doc))
     (setq undo nil sr:new nil erased nil)
     (princ (if scheme "\nRoad surfaces and markings updated." "\nRoad appearance cleared."))))
  (princ)
)

(defun sr:style-pick (/ hit data owner)
  (if (setq hit (entsel "\nSelect a corridor alignment, edge or painted surface: "))
    (progn
      (setq data (cdr (assoc -3 (entget (car hit) '("SR-CORRIDOR-EDGE" "SR-ROAD-STYLE"))))
            owner (cdr (assoc 1005 (cdar data))))
      (if owner (handent owner) (car hit))))
)

(defun c:SRSTYLE (/ axis model mode)
  (if (and (sr:modelspace-p)
           (or (= 0 (logand 2 (getvar "UNDOCTL")))
               (progn (princ "\nEnable UNDO (All) first.") nil))
           (setq axis (sr:style-pick)))
    (if (sr:model-valid-p (setq model (sr:model axis)))
      (progn
        (initget "YellowDash DoubleYellow WhiteDash NoCenter Clear")
        (setq mode (getkword "\nAppearance [YellowDash/DoubleYellow/WhiteDash/NoCenter/Clear] <YellowDash>: "))
        (sr:style-change axis model (cond ((equal mode "Clear") nil) (mode mode) (T "YellowDash"))))
      (princ "\nSelect the source alignment of a Smart Road corridor.")))
  (princ)
)

(vl-catch-all-apply 'droad:toolbar (list nil))
(princ "\nSmart Road loaded. Commands: SMARTROADTOOLBAR, SMARTROAD, DROAD, RNET.")
(princ "\nCorridor tools: SRROAD, SRWALK, SRTRANSIT, SRJUNCTION, SREDIT.")
(princ "\nQuick tools: SRQUICK, SRPROFILE, SRSECTION. Road appearance: SRSTYLE.")
(princ "\nJunction and reporting: SRCONNECT, SRQUANTITY.")
(princ)
