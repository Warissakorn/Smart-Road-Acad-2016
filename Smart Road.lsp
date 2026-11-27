;;; ==========================================================================
;;; AutoLISP: Smart Road & Auto-Junction Generator (ASCII Safe Version)
;;; Commands:
;;;   SMARTROADTOOLBAR - Show the Smart Road toolbar
;;;   SMARTROAD - Open the road settings dialog
;;;   DROAD - Draw new road (Line & Arc) with auto-junction & fillet
;;;   RNET  - Convert multiple centerlines (Line/Arc/Polyline) to road network
;;;   SRROAD, SRWALK, SRTRANSIT, SRJUNCTION, SREDIT - 2D corridor and plan tools
;;;   SRQUICK, SRPROFILE, SRSECTION - Quick corridor build, profile and section edit
;;;   SRDRAWROAD, SRBUILDROAD - Sketch or build roads from existing alignments
;;;   SRCONNECT, SRSTYLE - Join existing edges and finish road surfaces
;;; ==========================================================================

(vl-load-com)

(defun droad:sync-units (/ scale ratio)
  (setq scale (sr:unit-scale))
  (cond
    ((null *droad-unit-scale*)
     (setq *droad-width* (sr:default 'road-width) *droad-fillet* (sr:default 'junction-radius)))
    ((/= scale *droad-unit-scale*)
     (setq ratio (/ scale *droad-unit-scale*)
           *droad-width* (* *droad-width* ratio)
           *droad-fillet* (* *droad-fillet* ratio))))
  (setq *droad-unit-scale* scale)
)

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

;; Size the CENTER pattern as 50.8 mm on paper (the acadiso.lin length) at the
;; default plot scale, whichever .lin defined it and whatever LTSCALE is.
(defun droad:center-ltscale (/ pattern)
  (setq pattern (cdr (assoc 40 (tblsearch "LTYPE" "CENTER"))))
  (if (and pattern (> pattern 0.0) (> (getvar "LTSCALE") 0.0))
    (/ (* 50.8 0.001 (sr:default-m 'plot-scale) (sr:unit-scale)) pattern (getvar "LTSCALE"))
    1.0)
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
      (if (< d (- half-w (sr:tol 1e-3)))
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
        (if (<= (distance pt pt-on) (sr:tol 1e-3))
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
          (if (not (vl-some '(lambda (c) (<= (distance p-s (vlax-curve-getClosestPointTo c p-s)) (sr:tol 1e-3))) old-cls))
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
                      (if (> (distance p-s p-hit) (sr:tol 1e-3))
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
          (if (not (vl-some '(lambda (c) (<= (distance p-e (vlax-curve-getClosestPointTo c p-e)) (sr:tol 1e-3))) old-cls))
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
                      (if (> (distance p-e p-hit) (sr:tol 1e-3))
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
  (vla-put-LinetypeScale vla-cl (droad:center-ltscale))
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
      (setq all-int-pts (droad:dedupe-pts all-int-pts (sr:tol 1e-3)))

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
      (if (equal p q (sr:tol 1e-3)) (setq match p))
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
                   (< tangent (- (min (distance corner other-a) (distance corner other-b)) (sr:tol 1e-3))))
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
  (if (equal start pt (sr:tol 1e-3)) (vlax-curve-getEndPoint en) start)
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
     (droad:sync-units)
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
           (if (and (not (assoc -3 (entget en '("SR-CORRIDOR" "SR-CORRIDOR-EDGE" "SR-ROAD-STYLE" "SR-TRANSIT-ANCHOR"))))
                    (droad:planar-center-p en)
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
      (droad:sync-units)
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
  '(("Draw Road" "Sketch an alignment with a live road preview" "^C^CSRDRAWROAD " "SmartRoadDraw")
    ("Build from Alignments" "Build roads from selected lines, arcs and polylines" "SRBUILDROAD " "SmartRoadSelect")
    ("Cross Sections" "Set the active cross section or save and load named templates" "^C^CSRPROFILE " "SmartRoadCorridor")
    ("Edit Corridor" "Change the base section, edit stations or rebuild after an alignment edit" "^C^CSREDIT " "SmartRoadEdit")
    ("Edit by Points" "Pick two points to add or change a section of a corridor" "^C^CSRSECTION " "SmartRoadSection")
    ("Junction Layout" "Lay out a tee, cross junction, roundabout or turn lane" "^C^CSRJUNCTION " "SmartRoadJunction")
    ("Join Road Edges" "Preview and join two edges at one junction corner" "^C^CSRCONNECT " "SmartRoadConnect")
    ("Pedestrian Layout" "Build walkways or place a crossing, refuge or ramp symbol" "^C^CSRWALK " "SmartRoadWalk")
    ("Transit Layout" "Build bus or rail corridors and place stops or platforms" "^C^CSRTRANSIT " "SmartRoadTransit")
    ("Surface & Markings" "Apply or clear pavement fills and lane markings" "^C^CSRSTYLE " "SmartRoadStyle")))

(defun droad:toolbar-buttons (toolbar folder index / spec button base)
  (foreach spec sr:toolbar-spec
    (setq button (vla-AddToolbarButton toolbar index (car spec) (cadr spec)
                                       (caddr spec) :vlax-false)
          base (strcat folder "\\" (cadddr spec)))
    (vla-SetBitmaps button (strcat base "16.bmp") (strcat base "32.bmp"))
    (setq index (1+ index)))
)

(defun droad:toolbar-current-p (toolbar / index matches spec)
  (setq index 0 matches (= (vla-get-Count toolbar) (length sr:toolbar-spec)))
  (foreach spec sr:toolbar-spec
    (if matches
      (setq matches (equal (vla-get-Name (vla-Item toolbar index)) (car spec))
            index (1+ index))))
  matches
)

(defun droad:toolbar (show / groups toolbars toolbar folder icon result create start index)
  (setq groups (vla-get-MenuGroups (vlax-get-acad-object))
        toolbars (vla-get-Toolbars (vla-Item groups 0))
        toolbar (vl-catch-all-apply 'vla-Item (list toolbars "Smart Road"))
        create (vl-catch-all-error-p toolbar))
  (cond
    ((and create (not show)) nil)
    ((and (not create) (droad:toolbar-current-p toolbar))
     (if show (vla-put-Visible toolbar :vlax-true)))
    (T
     (setq folder (droad:toolbar-icons))
     (if (and show (not folder))
       (if (setq icon (getfiled "Locate SmartRoadStyle16.bmp in the Icons folder"
                              "SmartRoadStyle16.bmp" "bmp" 0))
         (setq folder (vl-filename-directory icon))))
     (if folder
       (progn
         (if create (setq toolbar (vla-Add toolbars "Smart Road")))
         (setq start (vla-get-Count toolbar)
               result (vl-catch-all-apply 'droad:toolbar-buttons (list toolbar folder start)))
         (if (vl-catch-all-error-p result)
           (progn
             (if create
               (vla-Delete toolbar)
               (while (> (vla-get-Count toolbar) start)
                 (vla-Delete (vla-Item toolbar (1- (vla-get-Count toolbar))))))
             (princ (strcat "\nSmart Road toolbar: " (vl-catch-all-error-message result))))
           (progn
             (setq index start)
             (repeat start
               (setq index (1- index))
               (vla-Delete (vla-Item toolbar index)))
             (if create (vla-Dock toolbar acToolbarDockTop))
             (vla-put-Visible toolbar (if show :vlax-true :vlax-false))))))))
  (princ)
)

(defun droad:hide-toolbar (/ group toolbar)
  (vlax-for group (vla-get-MenuGroups (vlax-get-acad-object))
    (setq toolbar (vl-catch-all-apply 'vla-Item (list (vla-get-Toolbars group) "Smart Road")))
    (if (not (vl-catch-all-error-p toolbar))
      (vla-put-Visible toolbar :vlax-false)))
  (princ)
)

(defun c:SMARTROADTOOLBAR (/ result)
  (setq result (vl-catch-all-apply 'droad:toolbar (list T)))
  (if (vl-catch-all-error-p result)
    (princ (strcat "\nSmart Road toolbar: " (vl-catch-all-error-message result)))
  )
  (princ)
)

;; Default dimensions in meters. Read them with sr:default (drawing units)
;; or sr:default-m (meters) so every command converts through INSUNITS.
(setq sr:defaults
  '((road-width . 7.0) (junction-radius . 2.0) (lane-width . 3.5)
    (crossing-span . 7.0) (crossing-depth . 3.0) (crossing-pitch . 0.75)
    (island-length . 2.0) (island-width . 1.5)
    (stop-length . 12.0) (stop-width . 1.5)
    (platform-length . 30.0) (platform-width . 3.0)
    (marker-offset . 0.45) (marker-radius . 0.25)
    (turn-width . 3.25) (turn-length . 20.0) (turn-taper . 15.0)
    (approach-width . 7.0) (approach-length . 20.0)
    (roundabout-radius . 14.0) (roundabout-approach . 18.0)
    (centerline-tolerance . 0.05) (chord-tolerance . 0.005) (sample-step . 0.001)
    (plot-scale . 1000.0)
    ;; Stripe width, dash length, gap, clear gap between double stripes.
    (marking 0.12 3.0 6.0 0.12)))

(defun sr:default-m (key / item)
  (if (setq item (assoc key sr:defaults))
    (cdr item)
    (progn (princ (strcat "\nSmart Road: no default named " (vl-princ-to-string key) ".")) (exit)))
)

(defun sr:default (key)
  (* (sr:unit-scale) (sr:default-m key))
)

;; Tolerances are given in meters; the values match the old literals so
;; metric drawings behave as before while other units scale with them.
(defun sr:tol (meters)
  (* meters (sr:unit-scale))
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

(defun sr:default-profile (/ lane)
  (setq lane (list (list "Lane" (sr:default-m 'lane-width))))
  (sr:scale-default-profile (list lane lane))
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

(defun sr:configure (profile / action key saved value record)
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
       (if (and (setq key (sr:template-key)) (setq value (getenv key))
                (not (vl-catch-all-error-p
                       (setq saved (vl-catch-all-apply 'sr:profile-ui-read-template (list value)))))
                (sr:profile-valid-p saved))
         (progn
           (setq profile saved record (vl-catch-all-apply 'read (list value)))
           (if (sr:profile-valid-p record)
             (princ "\nOld template has no units. Check widths before saving or building.")))
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
        tolerance (sr:default 'chord-tolerance))
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

(defun sr:miter-direction (a b clamp / na nb miter divisor)
  (if (not a) (setq a b))
  (if (not b) (setq b a))
  (setq na (list (- (cadr a)) (car a) 0.0)
        nb (list (- (cadr b)) (car b) 0.0)
        miter (droad:unit-vec (mapcar '+ na nb))
        divisor (apply '+ (mapcar '* miter na)))
  (if (or clamp (>= divisor 0.2))
    (mapcar '(lambda (component) (/ component (max 0.2 divisor))) miter))
)

(defun sr:offset-direction (a b)
  (sr:miter-direction a b nil)
)

(defun sr:clamped-offset-direction (a b)
  (sr:miter-direction a b T)
)

(defun sr:sampled-offset-at (axis station offset / length pt before after a b direction)
  (setq length (sr:length axis)
        pt (vlax-curve-getPointAtDist axis station)
        before (vlax-curve-getPointAtDist axis (max 0.0 (- station (sr:default 'sample-step))))
        after (vlax-curve-getPointAtDist axis (min length (+ station (sr:default 'sample-step))))
        a (if (equal before pt 1e-10) nil (droad:unit-vec (mapcar '- pt before)))
        b (if (equal after pt 1e-10) nil (droad:unit-vec (mapcar '- after pt))))
  (cond
    ((equal offset 0.0 1e-10) pt)
    ((setq direction (sr:offset-direction a b)) (mapcar '+ pt (srj:scaled direction offset)))
    ;; Existing edges from earlier versions used a clamped miter; match them when checking.
    ((not sr:strict-bends)
     (mapcar '+ pt (srj:scaled (sr:clamped-offset-direction a b) offset)))
    (T (princ (strcat "\nCannot maintain the section width at the sharp bend near station "
                      (rtos station 2 4) ". Round the alignment before building."))
       (exit)))
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

(defun sr:profile-at-fraction (from to ratio / side sign pair inner offset bands profile)
  (if (or (null to) (<= ratio 0.0)) from
    (if (>= ratio 1.0) to
      (progn
        (foreach sign '(1.0 -1.0)
          (setq side (if (> sign 0.0) 0 1) inner 0.0 bands nil)
          (foreach pair (sr:pair-bands (nth side from) (nth side to) sign)
            (setq offset (+ (* (- 1.0 ratio) (car pair)) (* ratio (cadr pair))))
            (if (> (abs (- offset inner)) 1e-10)
              (setq bands (cons (list (caddr pair) (abs (- offset inner))) bands)))
            (setq inner offset))
          (setq profile (cons (reverse bands) profile)))
        (reverse profile))))
)

(defun sr:profile-at-station (span station)
  (sr:profile-at-fraction (caddr span) (cadddr span)
    (/ (- station (car span)) (float (- (cadr span) (car span)))))
)

(defun sr:ranges-overlap-p (a b)
  (and (< (car a) (cadr b)) (< (car b) (cadr a)))
)

(defun sr:merge-ranges (ranges / range head merged)
  (foreach range (vl-sort ranges '(lambda (a b)
                   (if (= (car a) (car b)) (< (cadr a) (cadr b)) (< (car a) (car b)))))
    (setq head (car merged))
    (if (and head (<= (car range) (cadr head)))
      (setq merged (cons (list (car head) (max (cadr head) (cadr range))) (cdr merged)))
      (setq merged (cons range merged))))
  (reverse merged)
)

(defun sr:span-without-ranges (span ranges / start finish range end piece result)
  (setq start (car span) finish (cadr span))
  (foreach range (append (sr:merge-ranges ranges) (list (list finish finish)))
    (setq end (min finish (car range)))
    (if (> (- end start) 1e-6)
      (progn
        (setq piece (list start end (sr:profile-at-station span start)
                         (if (cadddr span) (sr:profile-at-station span end))))
        (if (> start (+ (car span) 1e-6))
          (setq piece (append piece (list (car span)))))
        (setq result (cons piece result))))
    (setq start (max start (cadr range))))
  (reverse result)
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
        spine (sr:offset-vertices axis stations start finish 0.0 0.0)
        good T)
  (foreach side '(0 1)
    (setq sign (if (= side 0) 1.0 -1.0) inner spine)
    (foreach pair (sr:pair-bands (nth side from) (nth side to) sign)
      (if good
        (setq outer (sr:offset-vertices axis stations start finish (car pair) (cadr pair))
              good (sr:strip-area inner outer sign) inner outer))))
  good
)

(defun sr:render-spans (axis spans / sr:span sr:edge-signature sr:strict-bends)
  (setq sr:strict-bends T)
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

(defun sr:apply-spans (axis model spans old / *error* sr:new sr:owner sr:previewing doc undo erased entity prior written appearance scheme paintable
                         transit all-spans stranded)
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
        appearance (sr:style-owned sr:owner)
        transit (sr:transit-owned sr:owner))
  (cond
    ((> (strlen (vl-prin1-to-string model)) 10000)
     (princ "\nThis alignment has too much profile data. Use a separate alignment.") nil)
    ((and transit
          (setq all-spans (sr:spans axis model)
                stranded (vl-some '(lambda (item / values)
                                     (setq values (sr:extent (sr:transit-data item)))
                                     (if (not (sr:transit-frame axis all-spans values))
                                       (if (numberp (car values)) (car values) -1.0)))
                                  transit)))
     (princ (strcat "\nA linked transit feature at station " (rtos stranded 2 4)
                    " is outside the rebuilt corridor. Reposition or erase it before rebuilding.")) nil)
    ((not (vl-every 'sr:writable-p (cons axis (append old appearance transit))))
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
     (if transit
       (progn
         (foreach entity transit
           (sr:write-transit axis all-spans (sr:transit-data entity) (cdr (assoc 8 (entget entity)))))))
     (foreach entity (append old transit)
       (if (entdel entity) (setq erased (cons entity erased)) (exit)))
     (if appearance
       (progn
         (setq scheme (sr:style-scheme (car appearance)))
         (if (and scheme (setq paintable (sr:paintable-spans axis model sr:owner)))
           (sr:style-render axis paintable sr:owner scheme (sr:style-dimensions (car appearance)))
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

(defun sr:confirm-station-limits (axis model / length section warning)
  (setq length (sr:length axis))
  (foreach section (cadr model)
    (if (> (cadr section) (+ length 1e-6))
      (progn
        (setq warning T)
        (princ (strcat "\nSection " (rtos (car section) 2 4) " to " (rtos (cadr section) 2 4)
          (if (>= (car section) length) " will be omitted." (strcat " will end at " (rtos length 2 4) ".")))))))
  (if warning
    (progn
      (initget "Yes No")
      (equal "Yes" (getkword "\nContinue with the shortened alignment [Yes/No] <No>: ")))
    T)
)

(defun sr:edit (/ axis model mode profile regions index selected remaining)
  (if (and (sr:modelspace-p) (setq axis (sr:pick-corridor)))
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
          (if (and model (sr:confirm-station-limits axis model) (sr:preview axis model)) (sr:build axis model)))
        (princ "\nThis alignment has no editable Smart Road corridor."))))
  (princ)
)

;; Plan features use a user-picked local XY frame, with dimensions in drawing units.
(defun sr:local (origin direction x y)
  (list (+ (car origin) (* x (car direction)) (* y (- (cadr direction))))
        (+ (cadr origin) (* x (cadr direction)) (* y (car direction)))
        (caddr origin))
)

(defun sr:plan-geometry (command args / center radius start sweep points index angle)
  (if sr:collect-plan
    (progn
      (setq sr:plan-items (cons (cons command args) sr:plan-items))
      (if (eq command 'sr:poly)
        (srj:draw-preview (car args))
        (progn
          (setq center (car args) radius (cadr args)
                start (if (eq command 'sr:arc) (caddr args) 0.0)
                sweep (if (eq command 'sr:arc)
                        (droad:ccw-diff start (cadddr args)) (* 2.0 pi))
                index 0)
          (repeat 33
            (setq angle (+ start (* sweep (/ index 32.0)))
                  points (cons (mapcar '+ center (list (* radius (cos angle)) (* radius (sin angle)) 0.0)) points)
                  index (1+ index)))
          (srj:draw-preview points))))
    (apply command args))
)

(defun sr:shape (origin direction plan closed layer)
  (sr:plan-geometry 'sr:poly
    (list (mapcar '(lambda (pt) (sr:local origin direction (car pt) (cadr pt))) plan)
          closed layer))
)

(defun sr:box (origin direction x1 y1 x2 y2 layer)
  (sr:shape origin direction (list (list x1 y1) (list x2 y1) (list x2 y2) (list x1 y2)) T layer)
)

(defun sr:plan (kind / origin point direction length width count step x y corner approach radius stem
                      opening angle theta index intersection layer half quadrant fill scale fillet u v)
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
                 width (sr:number "Crossing span across the road" (sr:default 'crossing-span) nil)
                 length (sr:number "Crossing depth along the road" (sr:default 'crossing-depth) nil)
                 count (fix (/ width (sr:default 'crossing-pitch))))
           (if (or (= 0 (getvar "INSUNITS")) (< count 1) (> count 200))
             (progn
               (princ (strcat "\nCheck crossing dimensions and drawing units. Span "
                              (rtos (/ width scale) 2 3) " m, depth " (rtos (/ length scale) 2 3) " m."))
               (initget "Yes No")
               (if (not (equal "Yes" (getkword "\nKeep these dimensions [Yes/No] <No>: "))) (exit))))
           (setq count (max 1 (min 200 count))
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
           (setq scale (sr:unit-scale)
                 length (sr:number "Feature length"
                           (sr:default (cond ((equal kind "Platform") 'platform-length)
                                             ((equal kind "Stop") 'stop-length)
                                             (T 'island-length))) nil)
                 width (sr:number "Feature width"
                         (sr:default (cond ((equal kind "Platform") 'platform-width)
                                           ((equal kind "Stop") 'stop-width)
                                           (T 'island-width))) nil)
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
                (sr:circle (sr:local origin direction 0.0 (+ half (sr:default 'marker-offset))) (sr:default 'marker-radius) layer)))))
          ((equal kind "TurnLane")
           (setq scale (sr:unit-scale)
                 width (sr:number "Turn lane width" (sr:default 'turn-width) nil)
                 length (sr:number "Full width length" (sr:default 'turn-length) nil)
                 approach (sr:number "Entry taper length" (sr:default 'turn-taper) nil)
                 layer (sr:layer "SR-TURN-LANE" 2))
           (initget "Left Right")
           (if (not (equal "Left" (getkword "\nWiden to [Left/Right] <Right>: "))) (setq width (- width)))
           (sr:shape origin direction
             (list '(0.0 0.0) (list approach width) (list (+ approach length) width)) nil layer)
           (sr:shape origin direction (list '(0.0 0.0) (list (+ approach length) 0.0)) nil layer))
          ((member kind '("Tee" "Cross"))
           (setq scale (sr:unit-scale)
                 width (sr:number "Approach width" (sr:default 'approach-width) nil)
                 half (/ width 2.0)
                 approach (sr:number "Approach length beyond intersection" (sr:default 'approach-length) nil)
                 corner (sr:number "Corner radius" (sr:default 'junction-radius) T)
                 stem (+ half approach)
                 layer (sr:layer "SR-JUNCTION" 2))
           (if (>= corner approach)
             (progn (princ "\nCorner radius must be less than the approach length.") (exit)))
           (foreach quadrant '((1 1) (-1 1) (-1 -1) (1 -1))
             (if (or (equal kind "Cross") (= 1 (cadr quadrant)))
               (if (> corner 0.0)
                 (progn
                   (setq u (sr:local '(0.0 0.0 0.0) direction (car quadrant) 0.0)
                         v (sr:local '(0.0 0.0 0.0) direction 0.0 (cadr quadrant))
                         fillet (srj:fillet
                           (sr:local origin direction (* half (car quadrant)) (* half (cadr quadrant)))
                           u v corner))
                   (sr:plan-geometry 'sr:poly
                     (list (list (sr:local origin direction (* stem (car quadrant)) (* half (cadr quadrant)))
                                 (car fillet)) nil layer))
                   (sr:plan-geometry 'sr:poly
                     (list (list (cadr fillet)
                                 (sr:local origin direction (* half (car quadrant)) (* stem (cadr quadrant))))
                           nil layer))
                   (sr:plan-geometry 'sr:arc
                     (list (caddr fillet) corner (cadddr fillet) (nth 4 fillet) layer)))
                 (sr:shape origin direction
                   (mapcar '(lambda (pt) (mapcar '* pt quadrant))
                     (list (list stem half) (list half half) (list half stem)))
                   nil layer))))
           (if (equal kind "Tee")
             (sr:shape origin direction (list (list (- stem) (- half)) (list stem (- half))) nil layer)))
          ((equal kind "Roundabout")
           (setq scale (sr:unit-scale)
                 radius (sr:number "Outer radius" (sr:default 'roundabout-radius) nil)
                 width (sr:number "Circulating width" (* radius 0.45) nil)
                 opening (sr:number "Approach width" (min (sr:default 'approach-width) radius) nil)
                 approach (sr:number "Approach length beyond circle" (sr:default 'roundabout-approach) nil))
           (if (and (< width radius) (< opening (* radius (sqrt 2.0))))
             (progn
               (setq layer (sr:layer "SR-ROUNDABOUT" 2)
                     angle (atan (cadr direction) (car direction))
                     half (* 0.5 opening)
                     intersection (sqrt (- (* radius radius) (* half half)))
                     theta (atan half intersection)
                     index 0)
               (sr:plan-geometry 'sr:circle (list origin (- radius width) layer))
               (repeat 4
                 (setq stem (+ angle (* index (/ pi 2.0))))
                 (sr:plan-geometry 'sr:arc (list origin radius (rem (+ stem theta (* 2.0 pi)) (* 2.0 pi))
                         (rem (+ stem (/ pi 2.0) (- theta) (* 2.0 pi)) (* 2.0 pi)) layer))
                 (foreach y (list half (- half))
                   (sr:shape origin (list (cos stem) (sin stem) 0.0)
                     (list (list intersection y) (list (+ radius approach) y)) nil layer))
                 (setq index (1+ index))))
             (princ "\nUse a circulating width smaller than the radius and narrower approaches.")))))
        (princ "\nPick two distinct plan points."))))
  (princ)
)

(defun sr:feature (kind / *error* sr:new sr:owner sr:previewing sr:collect-plan sr:plan-items
                        doc undo item layer)
  (defun *error* (msg)
    (redraw)
    (foreach item sr:new (if (entget item) (entdel item)))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ)
  )
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)
  (setq undo T)
  (if (member kind '("FromRoads" "Tee" "Cross" "Roundabout" "TurnLane"))
    (progn
      (setq sr:collect-plan T sr:previewing T)
      (if (equal kind "FromRoads") (srj:layout-from-roads) (sr:plan kind))
      (setq sr:collect-plan nil sr:previewing nil)
      (if sr:plan-items
        (progn
          (initget "Apply Cancel")
          (if (equal "Apply" (getkword "\nJunction preview [Apply/Cancel] <Cancel>: "))
            (progn
              (setq layer (cond ((equal kind "Roundabout") "SR-ROUNDABOUT")
                                ((equal kind "TurnLane") "SR-TURN-LANE")
                                (T "SR-JUNCTION")))
              (sr:layer layer 2)
              (foreach item (reverse sr:plan-items) (apply (car item) (cdr item)))))))
      (redraw))
    (sr:plan kind))
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

(defun sr:span-at-station (spans station)
  (vl-some '(lambda (span)
              (if (<= (car span) station (cadr span)) span)) (reverse spans))
)

(defun sr:alignment-direction (axis station / step before after)
  (setq step (sr:default 'sample-step)
        before (vlax-curve-getPointAtDist axis (max 0.0 (- station step)))
        after (vlax-curve-getPointAtDist axis (min (sr:length axis) (+ station step))))
  (droad:unit-vec (mapcar '- after before))
)

(defun sr:transit-data (entity)
  (cdadr (assoc -3 (entget entity '("SR-TRANSIT-ANCHOR"))))
)

(defun sr:transit-owned (owner / selection index entity result)
  (if (setq selection (ssget "_X" '((410 . "Model") (-3 ("SR-TRANSIT-ANCHOR")))))
    (progn
      (setq index 0.0)
      (repeat (sslength selection)
        (setq entity (ssname selection index) index (1+ index))
        (if (equal owner (cdr (assoc 1005 (sr:transit-data entity))))
          (setq result (cons entity result))))))
  result
)

(defun sr:transit-frame (axis spans values / station sign clearance width span profile offset center)
  (setq station (car values) sign (cadr values)
        clearance (caddr values) width (nth 4 values))
  (if (and (numberp station) (setq span (sr:span-at-station spans station)))
    (progn
      (setq profile (sr:profile-at-station span station)
            offset (* sign (+ (sr:side-width profile (if (> sign 0.0) 0 1)) clearance (* 0.5 width)))
            center (sr:offset-at axis station offset))
      (list center (sr:alignment-direction axis station))))
)

(defun sr:write-transit (axis spans data layer / sr:owner kind values frame center direction entity half)
  (setq kind (cdr (assoc 1000 data)) values (sr:extent data))
  (if (not (setq frame (sr:transit-frame axis spans values)))
    (progn (princ "\nLinked transit station is outside the current alignment.") (exit)))
  (setq center (car frame) direction (cadr frame)
        half (* 0.5 (nth 4 values))
        entity (if (equal kind "Pole")
                 (sr:circle (sr:local center direction (- (* 0.5 (cadddr values)))
                              (* (cadr values) (+ half (sr:default 'marker-offset))))
                            (sr:default 'marker-radius) layer)
                 (sr:box center direction (- (* 0.5 (cadddr values))) (- half)
                         (* 0.5 (cadddr values)) half layer)))
  (if (not (and entity (entmod (append (entget entity) (list (list -3 (cons "SR-TRANSIT-ANCHOR" data)))))))
    (progn (princ "\nCould not save the transit attachment.") (exit)))
  entity
)

(defun sr:linked-transit (kind / *error* sr:new axis model pick station side sign gap length width values
                                data spans frame doc undo layer entity)
  (defun *error* (msg)
    (redraw)
    (foreach entity sr:new (if (entget entity) (entdel entity)))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nSmart Road: " msg)))
    (princ))
  (if (/= 0 (logand 2 (getvar "UNDOCTL")))
    (princ "\nEnable UNDO (All) first.")
    (if (and (setq axis (sr:pick-corridor))
             (sr:model-valid-p (setq model (sr:model axis)))
             (setq pick (getpoint "\nPick the center station of the transit feature: "))
             (setq station (sr:station-at-pick axis pick)))
      (progn
        (initget "Left Right")
        (setq side (getkword "\nSide of alignment [Left/Right] <Right>: ")
              sign (if (equal side "Left") 1.0 -1.0)
              gap (sr:number "Clearance from corridor edge (drawing units)" 0.0 T)
              length (sr:number "Feature length" (sr:default (if (equal kind "Stop") 'stop-length 'platform-length)) nil)
              width (sr:number "Feature width" (sr:default (if (equal kind "Stop") 'stop-width 'platform-width)) nil)
              values (list station sign gap length width)
              data (append (list (cons 1005 (cdr (assoc 5 (entget axis)))) (cons 1000 kind))
                           (mapcar '(lambda (value) (cons 1040 (float value))) values))
              spans (sr:spans axis model)
              frame (sr:transit-frame axis spans values))
        (if frame
          (progn
            (setq entity (car frame))
            (sr:preview-path (list (vlax-curve-getPointAtDist axis station) entity) 6)
            (sr:preview-path (mapcar '(lambda (pt)
              (sr:local entity (cadr frame) (* (car pt) length) (* (cadr pt) width)))
              '((-0.5 -0.5) (0.5 -0.5) (0.5 0.5) (-0.5 0.5) (-0.5 -0.5))) 3)
            (initget "Build Cancel")
            (if (equal "Build" (getkword "\nLinked feature preview [Build/Cancel] <Cancel>: "))
              (progn
                (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
                (if (= 0 (logand 8 (getvar "UNDOCTL")))
                  (progn (vla-StartUndoMark doc) (setq undo T)))
                (regapp "SR-TRANSIT-ANCHOR")
                (setq layer (sr:layer (strcat "SR-" (strcase kind)) 5))
                (sr:write-transit axis spans data layer)
                (if (equal kind "Stop")
                  (sr:write-transit axis spans (subst '(1000 . "Pole") (assoc 1000 data) data) layer))
                (if undo (vla-EndUndoMark doc))
                (setq undo nil sr:new nil)
                (princ "\nTransit feature linked; Edit Corridor rebuilds its position.")))
            (redraw))))
      (if (and axis (not (sr:model-valid-p model)))
        (princ "\nChoose an editable Smart Road Corridor."))))
  (princ)
)

(defun c:SRTRANSIT (/ option)
  (if (sr:modelspace-p)
    (progn
      (initget "Corridor LinkedStop LinkedPlatform Stop Platform")
      (setq option (getkword "\nTransit [Corridor/LinkedStop/LinkedPlatform/Stop/Platform] <Corridor>: "))
      (cond
        ((equal option "LinkedStop") (sr:linked-transit "Stop"))
        ((equal option "LinkedPlatform") (sr:linked-transit "Platform"))
        ((member option '("Stop" "Platform")) (sr:feature option))
        (T (sr:create "Transit")))))

  (princ)
)

(defun sr:side-width (profile side)
  (apply '+ (cons 0.0 (mapcar 'cadr (nth side profile))))
)

(defun srj:road-arm (origin tolerance / hit axis at pick position direction model span profile a b width forward
                                       step ahead reach)
  (if (and (setq hit (entsel "\nPick an alignment on the side that leaves the junction: "))
           (setq axis (sr:corridor-source (car hit)))
           (member (cdr (assoc 0 (entget axis))) '("LINE" "ARC" "LWPOLYLINE"))
           (droad:planar-center-p axis) (not (vlax-curve-isClosed axis))
           (setq at (sr:station-at-wcs axis origin) pick (sr:station-at-pick axis (cadr hit)))
           (setq position (vlax-curve-getPointAtDist axis at))
           (equal (caddr position) (caddr origin) 1e-8)
           (<= (distance position origin) tolerance)
           (> (abs (- pick at)) 1e-6))
    (progn
      ;; Sample only the picked side so a vertex at the junction gives that segment's direction.
      (setq forward (> pick at)
            reach (if forward (- (sr:length axis) at) at)
            step (min (sr:default 'sample-step) reach)
            ahead (vlax-curve-getPointAtDist axis (if forward (+ at step) (- at step)))
            direction (droad:unit-vec (mapcar '- ahead position))
            model (sr:model axis))
      (if (and (sr:model-valid-p model) (setq span (sr:span-at-station (sr:spans axis model) at)))
        (setq profile (sr:profile-at-station span at)
              a (sr:side-width profile 0)
              b (sr:side-width profile 1))
        (progn
          (setq width (sr:number "Arm width (drawing units)"
                         (droad:get-width-xdata axis (sr:default 'road-width)) nil)
                a (* 0.5 width) b a)))
      ;; Left and right widths follow the station direction; swap them for an arm running backwards.
      (if (not forward)
        (setq width a a b b width))
      ;; Reach limits where the corner may fall; the pick distance sets how far the arm is drawn.
      (list (droad:ccw-diff 0.0 (atan (cadr direction) (car direction))) direction a b reach (abs (- pick at))))
    (if hit
      (princ "\nChoose an open horizontal alignment passing through the junction on the same level.")))
)

(defun srj:arm-corner (origin first second radius / gap u v a b far-a far-b corner fillet p q)
  (setq gap (droad:ccw-diff (car first) (car second))
        u (cadr first) v (cadr second)
        a (sr:local origin u 0.0 (caddr first))
        b (sr:local origin v 0.0 (- (cadddr second)))
        far-a (sr:local a u (nth 5 first) 0.0)
        far-b (sr:local b v (nth 5 second) 0.0))
  (cond
    ((<= gap 1e-6) nil)
    ((>= gap (- pi 1e-7))
     (list (if (equal a b (sr:tol 1e-8)) (list far-a a far-b) (list far-a a b far-b)) nil nil))
    ((setq corner (srj:intersection a far-a b far-b))
     (if (> radius 0.0)
       (setq fillet (srj:fillet corner u v radius)))
     (setq p (if fillet (car fillet) corner) q (if fillet (cadr fillet) corner))
     (if (and (or (<= radius 0.0) fillet)
              (< (srj:dot (mapcar '- p a) u) (- (nth 4 first) 1e-6))
              (< (srj:dot (mapcar '- q b) v) (- (nth 4 second) 1e-6)))
       (progn
         ;; Draw each arm at least past its tangent point, even when the pick was closer.
         (setq far-a (sr:local a u (max (nth 5 first) (srj:dot (mapcar '- p a) u)) 0.0)
               far-b (sr:local b v (max (nth 5 second) (srj:dot (mapcar '- q b) v)) 0.0))
       (if fillet
         (list (list far-a p) (list q far-b) fillet)
         (list (list far-a corner far-b) nil nil))))))
)

(defun srj:layout-outline (plan / result item path arc)
  (foreach item plan
    (setq path (car item))
    (if (caddr item)
      (progn
        (setq arc (srj:arc-points (caddr item)))
        (if (> (distance (last path) (car arc)) (distance (last path) (last arc)))
          (setq arc (reverse arc)))
        (setq path (append path (cdr arc) (cdr (cadr item))))))
    (setq result (append result path)))
  result
)

(defun srj:road-layout (origin arms radius / ordered rest pair plan valid)
  (setq ordered (vl-sort arms '(lambda (a b) (< (car a) (car b)))))
  (if (= (length ordered) (length arms))
    (progn
      (setq rest (append ordered (list (car ordered))) valid T)
      (while (and valid (cdr rest))
        (setq pair (srj:arm-corner origin (car rest) (cadr rest) radius))
        (if pair (setq plan (cons pair plan)) (setq valid nil))
        (setq rest (cdr rest)))
      (if (and valid (sr:outline-simple-p (srj:layout-outline (reverse plan))))
        (reverse plan))))
)

(defun srj:layout-from-roads (/ origin count tolerance arms arm radius plan item fillet)
  (if (setq origin (getpoint "\nPick the junction center on the alignments: "))
    (progn
      (setq origin (trans origin 1 0))
      (initget "Three Four")
      (setq count (if (equal "Four" (getkword "\nNumber of arms [Three/Four] <Three>: ")) 4 3)
            tolerance (sr:number "Centerline tolerance (drawing units)" (sr:default 'centerline-tolerance) T))
      (repeat count
        (if (setq arm (srj:road-arm origin tolerance))
          (setq arms (cons arm arms))
          (exit)))
      (setq radius (sr:number "Corner radius (drawing units)" (sr:default 'junction-radius) T))
      (if (setq plan (srj:road-layout origin arms radius))
        (foreach item plan
          (sr:plan-geometry 'sr:poly (list (car item) nil "SR-JUNCTION"))
          (if (cadr item) (sr:plan-geometry 'sr:poly (list (cadr item) nil "SR-JUNCTION")))
          (if (setq fillet (caddr item))
            (sr:plan-geometry 'sr:arc (list (caddr fillet) radius (cadddr fillet) (nth 4 fillet) "SR-JUNCTION"))))
        (princ "\nThe arms overlap, coincide or are too short for this radius. Check the picks and widths."))))
)
(defun c:SRJUNCTION (/ option)
  (if (sr:modelspace-p)
    (progn
      (initget "FromRoads Tee Cross Roundabout TurnLane")
      (setq option (getkword "\nJunction [FromRoads/Tee/Cross/Roundabout/TurnLane] <Cross>: "))
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

(defun sr:profile-ui-template (action / key value profile record)
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
         (setq record (vl-catch-all-apply 'read (list value)))
         (set_tile "status"
           (if (and (listp record) (equal (car record) "SR-PROFILE-1"))
             "Template loaded in current drawing units."
             "Old template has no units. Check widths before saving or building.")))
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
  (setq data (entget axis '("SR-CORRIDOR" "SR-CORRIDOR-EDGE" "SR-ROAD-STYLE" "SR-TRANSIT-ANCHOR" "DROAD_APP")))
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

(defun sr:quick-build (axes profile scheme dimensions
                       / *error* sr:new sr:owner sr:previewing
                         doc undo-open writing axis writes item result undo-failed rollback-failed locked paintable)
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
       (sr:render axis (list profile nil))
       (if scheme
         (if (setq paintable (sr:paintable-spans axis (list profile nil) sr:owner))
           (sr:style-render axis paintable sr:owner scheme dimensions)
           (progn (princ "\nAppearance needs complete corridor boundaries.") (exit)))))
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
                  (setq profile (sr:default-profile)
                        default-profile T)
                  (princ (strcat "\nUsing the default profile: one " (rtos (sr:default-m 'lane-width) 2 2)
                                 "-meter lane per side, converted to drawing units."))))
              (if (sr:quick-build axes profile nil nil)
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
  (if (not profile) (setq profile (sr:default-profile)))
  (setq profile (sr:configure-profile profile))
  (if profile
    (progn
      (setq saved (sr:remember-quick-profile profile))
      (princ (if (and saved (not (vl-catch-all-error-p saved)))
               "\nQuick Build profile saved."
               "\nCould not save the Quick Build profile."))))
  (princ)
)

(defun sr:station-at-wcs (axis point / projected station)
  (if (and point
           (setq projected (vlax-curve-getClosestPointTo axis point))
           (setq station (vlax-curve-getDistAtPoint axis projected)))
    (max 0.0 (min (sr:length axis) station)))
)

(defun sr:station-at-pick (axis point)
  (if point (sr:station-at-wcs axis (trans point 1 0)))
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

(defun sr:keep-section-sides (sections range / section result)
  (foreach section sections
    (cond
      ((or (<= (cadr section) (car range)) (>= (car section) (cadr range)))
       (setq result (cons section result)))
      (T
       (if (< (car section) (car range))
         (setq result (cons (list (car section) (car range) (caddr section) (cadddr section)) result)))
       (if (> (cadr section) (cadr range))
         (setq result (cons (list (cadr range) (cadr section) (caddr section) (cadddr section)) result))))))
  (reverse result)
)

(defun c:SRSECTION (/ axis model range regions overlapping prior profile taper updated choice retained)
  (if (and (sr:modelspace-p) (setq axis (sr:pick-corridor)))
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
                   (initget "Keep Replace Cancel")
                   (setq choice (getkword "\nKeep outside stations or replace the whole override [Keep/Replace/Cancel] <Cancel>: "))
                   (not (member choice '("Keep" "Replace")))))
            (princ "\nExisting section kept."))
           (T
            (setq prior (car overlapping)
                  retained (cond ((equal choice "Keep") (sr:keep-section-sides regions range))
                                 (prior (vl-remove prior regions))
                                 (T regions)))
            (if (equal choice "Keep")
              (princ "\nOutside stations are kept; taper lengths are limited by each remaining section."))
            (if (setq profile (sr:section-profile (if prior (caddr prior) (car model))))
              (progn
                (setq taper (sr:number "Transition length at each end" (if prior (cadddr prior) 0.0) T)
                      updated (vl-sort
                                (cons (append range (list profile taper))
                                      retained)
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
        ((or (assoc "SR-CORRIDOR" application) (assoc "DROAD_APP" application) (assoc "SR-ROAD-STYLE" application) (assoc "SR-TRANSIT-ANCHOR" application))
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

(defun srj:read-ranges (marker extent / values ranges)
  (if (equal (cdr (assoc 1070 marker)) 2)
    (progn
      (setq values (sr:extent marker))
      (while (cadr values)
        (setq ranges (cons (list (car values) (cadr values)) ranges) values (cddr values)))
      ranges)
    ;; Older drawings only record that the full span was edited.
    (list extent))
)

(defun srj:mark (data ranges / applications marker entry)
  (setq applications (cdr (assoc -3 data))
        marker (assoc "SR-JUNCTION-EDIT" applications)
        entry (append '("SR-JUNCTION-EDIT" (1070 . 2))
                 (if ranges
                   (apply 'append
                     (mapcar '(lambda (range)
                       (list (cons 1040 (float (car range))) (cons 1040 (float (cadr range))))) ranges)))))
  (if marker
    (subst (cons -3 (subst entry marker applications)) (assoc -3 data) data)
    (srj:put-marker data entry))
)

(defun srj:edge-trim-ranges (edge points / source extent data prior start-point end-point cut-start cut-end result)
  (if (and (nth 4 edge) (= 2 (length (setq extent (sr:extent (nth 4 edge))))))
    (progn
      (setq data (cadr edge)
            prior (assoc "SR-JUNCTION-EDIT" (cdr (assoc -3 data)))
            source (handent (cdr (assoc 1005 (nth 4 edge))))
            start-point (if (srj:arc-edge-p edge) (vlax-curve-getStartPoint (car edge)) (car (caddr edge)))
            end-point (if (srj:arc-edge-p edge) (vlax-curve-getEndPoint (car edge)) (last (caddr edge))))
      (if prior (setq result (srj:read-ranges (cdr prior) extent)))
      (if (and source (= 2 (length extent)))
        (progn
          (if (not (equal start-point (car points) 1e-6))
            (if (setq cut-start (sr:station-at-wcs source (car points)))
              (if (> (min cut-start (cadr extent)) (+ (car extent) 1e-6))
                (setq result (cons (list (car extent) (min cut-start (cadr extent))) result)))
              (setq result (list extent))))
          (if (not (equal end-point (last points) 1e-6))
            (if (setq cut-end (sr:station-at-wcs source (last points)))
              (if (< (max cut-end (car extent)) (- (cadr extent) 1e-6))
                (setq result (cons (list (max cut-end (car extent)) (cadr extent)) result)))
              (setq result (list extent))))
          (sr:merge-ranges result))
        (list extent))))
)

(defun srj:restore-edge (edge / data)
  (setq data (cadr edge))
  (if (and (nth 4 edge) (not (assoc "SR-JUNCTION-EDIT" (cdr (assoc -3 data)))))
    (setq data (srj:put-marker data (list "SR-JUNCTION-EDIT"))))
  (entmod data)
  (entupd (car edge))
)

(defun srj:modified-edge (edge points ranges / data)
  (setq data (cadr edge))
  (cond
    ((srj:arc-edge-p edge)
     (setq data (subst (cons 50 (droad:wcs-angle (cdr (assoc 10 data)) (car points))) (assoc 50 data) data)
           data (subst (cons 51 (droad:wcs-angle (cdr (assoc 10 data)) (last points))) (assoc 51 data) data)))
    ((equal (cdr (assoc 0 data)) "LINE")
    (setq data (subst (cons 11 (cadr points)) (assoc 11 data)
                 (subst (cons 10 (car points)) (assoc 10 data) data))))
    (T (setq data (srj:poly-data data points))))
  (if (nth 4 edge) (srj:mark data ranges) data)
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

(defun srj:owner-extents (owner first second / tag ranges)
  (foreach tag (list (nth 4 first) (nth 4 second))
    (if (and tag (equal owner (cdr (assoc 1005 tag))))
      (setq ranges (cons (sr:extent tag) ranges))))
  ranges
)

(defun srj:affected-appearance (owner extents / entity data extent result)
  (foreach entity (sr:style-owned owner)
    (setq data (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))
          extent (sr:extent data))
    (if (or (null extent) (member nil extents)
            (vl-some '(lambda (range) (sr:ranges-overlap-p range extent)) extents))
      (setq result (cons entity result))))
  result
)

;; Returns T when the affected spans were repainted, or were checked and need no paint.
(defun srj:repaint-owner (owner extents appearance / axis model paintable spans entity scheme legacy)
  (if (and appearance (setq axis (handent owner)) (sr:model-valid-p (setq model (sr:model axis))))
    (progn
      (setq entity (car appearance) scheme (sr:style-scheme entity)
            legacy (vl-some '(lambda (obj)
              (not (sr:extent (cdadr (assoc -3 (entget obj '("SR-ROAD-STYLE"))))))) appearance)
            paintable (sr:paintable-spans axis model owner)
            spans paintable)
      (if (and spans (not legacy) (not (member nil extents)))
        (setq spans (vl-remove-if-not
          '(lambda (span) (vl-some '(lambda (range) (sr:ranges-overlap-p range span)) extents))
          spans)))
      (cond
        ((not (and scheme paintable)) nil)
        (spans (sr:style-render axis spans owner scheme (sr:style-dimensions entity)) T)
        (T T))))
)

(defun srj:apply (first second plan ranges-a ranges-b / *error* doc undo first-changed second-changed
                  arc fillet appearance erased entity owner-a owner-b sr:new extents-a extents-b old-a old-b
                  repainted)
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
        owner-b (cdr (assoc 1005 (nth 4 second))))
  (if owner-a
    (setq extents-a (srj:owner-extents owner-a first second)
          old-a (srj:affected-appearance owner-a extents-a)))
  (if (and owner-b (not (equal owner-a owner-b)))
    (setq extents-b (srj:owner-extents owner-b first second)
          old-b (srj:affected-appearance owner-b extents-b)))
  (setq appearance (append old-a old-b))
  (if (not (vl-every 'sr:writable-p appearance))
    (progn (princ "\nUnlock the road fill and marking layers first.") (exit)))
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (if (= 0 (logand 8 (getvar "UNDOCTL")))
    (progn (vla-StartUndoMark doc) (setq undo T)))
  (if (or (nth 4 first) (nth 4 second))
    (if (and (not (tblsearch "APPID" "SR-JUNCTION-EDIT")) (not (regapp "SR-JUNCTION-EDIT")))
      (progn (princ "\nUnable to register the corridor junction marker.") (exit))))
  (if (not (setq first-changed (entmod (srj:modified-edge first (car plan) ranges-a))))
    (progn (princ "\nCould not modify the first edge.") (exit)))
  (if (not (setq second-changed (entmod (srj:modified-edge second (cadr plan) ranges-b))))
    (progn (princ "\nCould not modify the second edge.") (exit)))
  (if (setq fillet (caddr plan))
    (if (entmake (list '(0 . "ARC") '(100 . "AcDbEntity") (assoc 8 (cadr first))
                       '(100 . "AcDbCircle") (cons 10 (caddr fillet))
                       (cons 40 (distance (caddr fillet) (car fillet)))
                       '(100 . "AcDbArc") (cons 50 (cadddr fillet)) (cons 51 (nth 4 fillet))))
      (setq arc (entlast))
      (progn (princ "\nCould not create the corner arc.") (exit))))
  ;; Repaint both owners even when the first one fails.
  (setq repainted (or (null old-a) (srj:repaint-owner owner-a extents-a old-a)))
  (if (and old-b (not (srj:repaint-owner owner-b extents-b old-b)))
    (setq repainted nil))
  (foreach entity appearance
    (if (entdel entity) (setq erased (cons entity erased)) (exit)))
  (if undo (vla-EndUndoMark doc))
  (setq undo nil first-changed nil second-changed nil arc nil erased nil)
  (if appearance
    (princ (if repainted
             "\nRefreshed appearance around the trimmed corner."
             "\nCleared appearance around the trimmed corner. Corridor edges could not be checked; fix them and run Surface & Markings to repaint.")))
  (entupd (car first))
  (entupd (car second))
  (princ "\nRoad edges connected.")
  (princ)
)

(defun srj:preview-trim-ranges (edge ranges / axis range)
  (if (and ranges (setq axis (handent (cdr (assoc 1005 (nth 4 edge))))))
    (foreach range ranges
      (princ (strcat "\nFinish excludes stations " (rtos (car range) 2 4)
                     " to " (rtos (cadr range) 2 4) " on this alignment."))
      (sr:preview-path
        (mapcar '(lambda (station) (vlax-curve-getPointAtDist axis station))
          (sr:stations axis (car range) (cadr range))) 6)))
)

(defun c:SRCONNECT (/ *error* first second plan choice ranges-a ranges-b)
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
     (setq ranges-a (srj:edge-trim-ranges first (car plan))
           ranges-b (srj:edge-trim-ranges second (cadr plan)))
     (srj:preview-trim-ranges first ranges-a)
     (srj:preview-trim-ranges second ranges-b)
     (initget "Apply Cancel")
     (setq choice (getkword "\nGreen shows retained geometry, yellow marks the corner [Apply/Cancel] <Cancel>: "))
     (redraw)
     (if (equal choice "Apply") (srj:apply first second plan ranges-a ranges-b))))
  (princ)
)

;;; Corridor boundary checks used when building and painting.

(defun sr:edge-band (layer)
  (if layer
    (progn
      (setq layer (strcase layer))
      (vl-some '(lambda (spec)
                  (if (equal layer (strcat "SR-" (strcase (car spec)))) (car spec)))
               sr:bands)))
)

(defun sr:edge-vertices (entity / data elevation normal item points valid)
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

(defun sr:cross-product (a b c)
  (- (* (- (car b) (car a)) (- (cadr c) (cadr a)))
     (* (- (cadr b) (cadr a)) (- (car c) (car a))))
)

(defun sr:strip-cell-area (a b c d sign / x y)
  (setq x (* sign (sr:cross-product a b c))
        y (* sign (sr:cross-product a c d)))
  (if (or (< x -1e-8) (< y -1e-8))
    (setq x (* sign (sr:cross-product a b d))
          y (* sign (sr:cross-product b c d))))
  (if (and (>= x -1e-8) (>= y -1e-8))
    (* 0.5 (max 0.0 (+ x y))))
)

(defun sr:outline-simple-p (points / first previous edges i a b x y pending other end ok tolerance)
  ;; Sort by the left edge of each segment so distant pairs are skipped.
  (setq first (car points) previous first i 0 ok T tolerance (sr:tol 1e-8))
  (foreach b (append (cdr points) (list first))
    (if (not (equal previous b tolerance))
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
                   (and (equal 0.0 (sr:cross-product a b x) 1e-10)
                        (equal 0.0 (sr:cross-product a b y) 1e-10))))
        (setq ok nil))
      (setq pending (cdr pending)))
    (setq edges (cdr edges)))
  ok
)

(defun sr:strip-area (inner outer sign / area cell valid)
  (setq area 0.0 valid (and (cdr inner) (= (length inner) (length outer))
                            (sr:outline-simple-p (append inner (reverse outer)))))
  (while (and valid (cdr inner))
    (setq cell (sr:strip-cell-area (car inner) (cadr inner) (cadr outer) (car outer) sign))
    (if (numberp cell)
      (setq area (+ area cell) inner (cdr inner) outer (cdr outer))
      (setq valid nil)))
  (if (and valid (> area 1e-12)) area)
)

(defun sr:offset-vertices (axis stations start finish start-offset finish-offset / ratio)
  (mapcar
    '(lambda (station)
       (setq ratio (/ (- station start) (- finish start)))
       (sr:offset-at axis station (+ (* (- 1.0 ratio) start-offset) (* ratio finish-offset))))
    stations)
)

(defun sr:points-match-p (expected actual / tolerance)
  (setq tolerance (sr:tol 1e-6))
  (and (= (length expected) (length actual))
       (vl-every '(lambda (a b) (equal a b tolerance)) expected actual))
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

(defun sr:matching-edge (kind expected candidates)
  (vl-some
    '(lambda (entry)
       (if (and (equal kind (cadr entry)) (not (equal "ARC" (cdr (assoc 0 (entget (car entry)))))))
         (cond
           ((sr:points-match-p expected (caddr entry)) entry)
           ((sr:points-match-p expected (reverse (caddr entry)))
            (list (car entry) (cadr entry) (reverse (caddr entry))))
           ((sr:points-match-p (sr:linear-corners expected)
                                        (sr:linear-corners (caddr entry))) entry)
           ((sr:points-match-p (sr:linear-corners expected)
                                        (reverse (sr:linear-corners (caddr entry)))) entry))))
    candidates)
)

(defun sr:matching-arc (axis start finish pair expected candidates / center radius length)
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

(defun sr:span-boundaries-valid-p (axis span edges
                               / start finish from to stations axis-points edge data points candidates
                                 side sign inner pair expected match valid sr:envelope legacy prior sr-radius allowed marked)
  (setq start (car span) finish (cadr span)
        from (caddr span) to (if (cadddr span) (cadddr span) from)
        sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to))
        sr-radius (if (equal "ARC" (cdr (assoc 0 (entget axis)))) (cdr (assoc 40 (entget axis))))
        stations (sr:stations axis start finish)
        axis-points (sr:offset-vertices axis stations start finish 0.0 0.0)
        valid T)
  (foreach edge edges
    (setq data (sr:edge-band (cdr (assoc 8 (entget edge))))
          marked (assoc -3 (entget edge '("SR-JUNCTION-EDIT"))))
    (cond
      ((and sr:trim-allowed data marked)
       (setq allowed (cons (list edge data) allowed)))
      ((and data (not marked)
            (setq points (if (equal "ARC" (cdr (assoc 0 (entget edge))))
                           (list (vlax-curve-getStartPoint edge) (vlax-curve-getEndPoint edge))
                           (sr:edge-vertices edge))))
       (setq candidates (cons (list edge data points) candidates)))
      (T (setq valid nil))))
  (foreach side '(0 1)
    (setq sign (if (= side 0) 1.0 -1.0) inner axis-points)
    (foreach pair (sr:pair-bands (nth side from) (nth side to) sign)
      (if valid
        (progn
          (setq expected (sr:offset-vertices axis stations start finish (car pair) (cadr pair))
                match (sr:matching-edge (caddr pair) expected candidates))
          (if (and sr-radius (not match))
            (setq match (sr:matching-arc axis start finish pair expected candidates)))
          (if (not match)
            (progn
              (if (not prior)
                (progn (setq sr:envelope nil prior (sr:stations axis start finish))
                       (setq sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to)))))
              (setq match (sr:matching-edge (caddr pair)
                            (sr:old-offset-points axis prior start finish pair nil) candidates))))
          (if (not match)
            (progn
              (if (not legacy) (setq legacy (sr:legacy-stations axis start finish)))
              (setq match (sr:matching-edge (caddr pair)
                            (sr:old-offset-points axis legacy start finish pair T) candidates))))
          (if (not match)
            (if (setq match (vl-some '(lambda (record)
                                      (if (equal (cadr record) (caddr pair)) record)) allowed))
              (setq allowed (vl-remove match allowed))))
          (if (and match (sr:strip-area inner expected sign))
            (setq inner expected
                  candidates (vl-remove-if '(lambda (record) (eq (car record) (car match))) candidates))
            (setq valid nil))))))
  (and valid (null candidates) (null allowed))
)

;; Presentation objects use separate ownership tags from corridor edges.
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
      (setq tag (append tag (mapcar '(lambda (value) (cons 1040 value)) sr:appearance-dimensions)))
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

(defun sr:preview-path (points color / previous point vectors)
  (setq previous (trans (car points) 0 1))
  (foreach point (cdr points)
    (setq point (trans point 0 1)
          vectors (cons point (cons previous vectors))
          previous point))
  (if vectors (grvecs (cons color (reverse vectors))))
)

(defun sr:draft-offset-path (frames offset)
  (mapcar '(lambda (frame) (mapcar '+ (car frame) (srj:scaled (cadr frame) offset))) frames)
)

;; A sketch has one constant section. Built corridors may also have station-dependent offsets.
(defun sr:style-vertices (axis stations start finish first last)
  (if axis
    (sr:offset-vertices axis stations start finish first last)
    (sr:draft-offset-path sr:preview-frames first))
)

(defun sr:style-line (points width layer / point vertices)
  (if sr:appearance-preview
    (sr:preview-path points layer)
    (progn
      (foreach point points
        (setq vertices (cons (cons 10 (list (car point) (cadr point))) vertices)))
      (if (entmake (append
                     (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 layer)
                           '(100 . "AcDbPolyline") (cons 90 (length points)) '(70 . 0)
                           (cons 43 width) (cons 38 (caddr (car points))))
                     (reverse vertices)))
        (progn
          (setq point (sr:style-track))
          (if sr:appearance-lines (ssadd point sr:appearance-lines))
          point)
        (exit))))
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
(defun sr:style-dash-prelude (axis start finish first last yellow / origin offset previous data points total)
  (setq origin sr:appearance-original-start total 0.0)
  (if (and axis origin (> start origin))
    (progn
      (setq offset (+ first (* (- last first) (/ (- origin start) (float (- finish start)))))
            previous (assoc (list origin offset yellow) sr:dash-phases)
            total (if previous (cdr previous) 0.0)
            data (entget axis))
      (if (and (equal "ARC" (cdr (assoc 0 data))) (equal first last 1e-10))
        (setq data (cdr (assoc 40 data))
              total (+ total (* (- start origin) (/ (- data first) data))))
        (progn
          (setq points (sr:offset-vertices axis (sr:stations axis origin start) origin start offset first))
          (while (cdr points)
            (setq total (+ total (distance (car points) (cadr points))) points (cdr points)))))))
  total
)

(defun sr:style-stripe (axis stations start finish first last dashed yellow
                        / scale layer width points lengths total pieces ranges dash gap a b len
                          base lo hi path previous key radius factor interval remaining)
  (setq scale (sr:unit-scale)
        layer (cond (sr:appearance-preview (if yellow 2 7))
                    (yellow (sr:style-layer "SR-MARK-YELLOW" '(244 192 54)))
                    (T (sr:style-layer "SR-MARK-WHITE" '(248 247 240))))
        width (* (car sr:appearance-dimensions) scale)
        points (sr:style-vertices axis stations start finish first last))
  (if (not dashed)
    (sr:style-line points width layer)
    (progn
      (setq key (list start first yellow) previous (assoc key sr:dash-phases)
            base (if previous (cdr previous) (sr:style-dash-prelude axis start finish first last yellow))
            total base)
      (if (and axis (equal first last 1e-10) (equal "ARC" (cdr (assoc 0 (entget axis)))))
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
            dash (* (cadr sr:appearance-dimensions) scale)
            gap (* (caddr sr:appearance-dimensions) scale)
            ;; Keep cursor tracking responsive on long alignments.
            ranges (if (or (not sr:appearance-preview) (<= (- total base) (* 500 (+ dash gap))))
                     (sr:style-dash-ranges base total dash gap)))
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

(defun sr:road-group-end (pairs / boundary)
  (while (and pairs (sr:style-road-band-p (caddar pairs)))
    (setq boundary (car pairs) pairs (cdr pairs)))
  boundary
)

;; Both sides of a taper are linear in station. Testing both ends bounds the stripe throughout.
(defun sr:stripe-fits-p (offset inner outer half)
  (and (>= (- offset half) (- (min inner outer) 1e-8))
       (<= (+ offset half) (+ (max inner outer) 1e-8)))
)

(defun sr:bounded-stripe (axis stations start finish first last dashed yellow bounds-from bounds-to half)
  (if (and (sr:stripe-fits-p first (car bounds-from) (cadr bounds-from) half)
           (sr:stripe-fits-p last (car bounds-to) (cadr bounds-to) half))
    (sr:style-stripe axis stations start finish first last dashed yellow)
    (setq sr:style-skipped T))
)

(defun sr:style-center (axis stations span from to half / start finish left right bounds-from bounds-to shift)
  (if (and (not (equal sr:appearance-scheme "NoCenter"))
           (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) from)
           (vl-every '(lambda (side) (sr:style-road-band-p (caar side))) to))
    (progn
      (setq start (car span) finish (cadr span)
            left (sr:road-group-end (sr:pair-bands (car from) (car to) 1.0))
            right (sr:road-group-end (sr:pair-bands (cadr from) (cadr to) -1.0))
            bounds-from (list (car right) (car left))
            bounds-to (list (cadr right) (cadr left)))
      (if (equal sr:appearance-scheme "DoubleYellow")
        (progn
          (setq shift (+ half (* 0.5 (cadddr sr:appearance-dimensions) (sr:unit-scale))))
          (if (and (sr:stripe-fits-p shift 0.0 (cadr bounds-from) half)
                   (sr:stripe-fits-p shift 0.0 (cadr bounds-to) half)
                   (sr:stripe-fits-p (- shift) (car bounds-from) 0.0 half)
                   (sr:stripe-fits-p (- shift) (car bounds-to) 0.0 half))
            (progn
              (sr:style-stripe axis stations start finish shift shift nil T)
              (sr:style-stripe axis stations start finish (- shift) (- shift) nil T))
            (setq sr:style-skipped T)))
        (sr:bounded-stripe axis stations start finish 0.0 0.0 T
                          (not (equal sr:appearance-scheme "WhiteDash")) bounds-from bounds-to half))))
)

(defun sr:style-spans (axis spans / span start finish from to stations index sign points pairs edge material
                                  current next inner outer sr:dash-phases sr:envelope previous
                                  inner-first inner-last opposite sr:appearance-span sr:appearance-original-start sr:style-skipped
                                  half inset group-end bounds-from bounds-to shift)
  (setq half (* 0.5 (car sr:appearance-dimensions) (sr:unit-scale)))
  (foreach span spans
    (setq start (car span) finish (cadr span) from (caddr span)
          to (if (cadddr span) (cadddr span) from)
          sr:appearance-span (list start finish)
          sr:appearance-original-start (nth 4 span)
          sr:style-skipped nil
          sr:envelope (max (sr:profile-envelope from) (sr:profile-envelope to))
          stations (if axis (sr:stations axis start finish))
          points (sr:style-vertices axis stations start finish 0.0 0.0))
    (foreach index '(0 1)
      (setq sign (if (= index 0) 1.0 -1.0) inner points previous nil inner-first 0.0 inner-last 0.0
            opposite (sr:pair-bands (nth (- 1 index) from) (nth (- 1 index) to) (- sign))
            pairs (sr:pair-bands (nth index from) (nth index to) sign))
      (while pairs
        (setq edge (car pairs) current (caddr edge) next (caddr (cadr pairs))
              outer (sr:style-vertices axis stations start finish (car edge) (cadr edge))
              material (sr:style-material current))
        (if material
          (cond
            (sr:appearance-preview
             (sr:preview-path (append inner (reverse outer) (list (car inner))) 9))
            ((sr:strip-area inner outer sign)
             (sr:style-solid (append inner (reverse outer))
               (sr:style-layer (strcat "SR-FILL-" (car material)) (cdr material))))))
        (if (sr:style-road-band-p current)
          (progn
            (setq inset (* sign half))
            (if (not (sr:style-road-band-p previous))
              (progn
                (setq group-end (sr:road-group-end pairs)
                      bounds-from (list inner-first (car group-end))
                      bounds-to (list inner-last (cadr group-end)))
                (if (or previous (not (sr:style-road-band-p (caddar opposite))))
                  (sr:bounded-stripe axis stations start finish (+ inner-first inset) (+ inner-last inset)
                                    nil nil bounds-from bounds-to half))))
            (setq shift (if (sr:style-road-band-p next) 0.0 inset))
            (sr:bounded-stripe axis stations start finish (- (car edge) shift) (- (cadr edge) shift)
                              (sr:style-road-band-p next) nil bounds-from bounds-to half)))
        (setq inner outer previous current inner-first (car edge) inner-last (cadr edge) pairs (cdr pairs))))
    (sr:style-center axis stations span from to half)
    (if (and sr:style-skipped (not sr:appearance-preview))
      (princ (strcat "\nMarking omitted where it does not fit on pavement, stations "
                     (rtos start 2 4) " to " (rtos finish 2 4) "."))))
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

(defun sr:paintable-spans (axis model owner / span spans groups edges good invalid marker ranges edge pieces sr:trim-allowed)
  (setq spans (sr:spans axis model)
        groups (sr:span-inventory axis spans (sr:old-edges owner)))
  (if groups
    (foreach span spans
      (setq edges (cdr (assoc span groups)) ranges nil sr:trim-allowed nil)
      (foreach edge edges
        (if (setq marker (cdadr (assoc -3 (entget edge '("SR-JUNCTION-EDIT")))))
          (setq ranges (append ranges (srj:read-ranges marker (list (car span) (cadr span))))
                sr:trim-allowed T)))
      (setq pieces (if ranges (sr:span-without-ranges span ranges) (list span)))
      (cond
        ((null pieces) (princ "\nSkipping the fully trimmed span."))
        ((sr:span-boundaries-valid-p axis span edges) (setq good (append good pieces)))
        (T (setq invalid T)))))
  (if (not invalid) good)
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

(defun sr:style-render (axis spans owner scheme sr:appearance-dimensions
                        / sr:appearance-owner sr:appearance-scheme sr:appearance-hatches sr:appearance-lines)
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

(defun sr:style-dimensions (entity / dimensions)
  (if entity
    (setq dimensions (cddr (sr:extent (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))))))
  (if (and (= 4 (length dimensions)) (vl-every '(lambda (value) (> value 0.0)) dimensions))
    dimensions
    (sr:default-m 'marking))
)

(defun sr:prompt-marking-dimensions (dimensions)
  (list (sr:style-dimension-input "Stripe width" (car dimensions))
        (sr:style-dimension-input "Dash length" (cadr dimensions))
        (sr:style-dimension-input "Gap between dashes" (caddr dimensions))
        (sr:style-dimension-input "Clear gap between double stripes" (cadddr dimensions)))
)

(defun sr:style-dimension-input (label current / value)
  (initget 6)
  (setq value (getreal (strcat "\n" label " in meters <" (rtos current 2 8) ">: ")))
  (if value value current)
)

(defun sr:style-change (axis model scheme dimensions / *error* doc undo sr:new old erased entity owner paintable)
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
     (if scheme (sr:style-render axis paintable owner scheme dimensions))
     (foreach entity old
       (if (entdel entity) (setq erased (cons entity erased)) (exit)))
     (if undo (vla-EndUndoMark doc))
     (setq undo nil sr:new nil erased nil)
     (princ (if scheme "\nRoad surfaces and markings updated." "\nRoad appearance cleared."))))
  (princ)
)

(defun sr:corridor-source (entity / data owner)
  (setq data (cdr (assoc -3 (entget entity '("SR-CORRIDOR-EDGE" "SR-ROAD-STYLE" "SR-TRANSIT-ANCHOR"))))
        owner (cdr (assoc 1005 (cdar data))))
  (if owner (handent owner) entity)
)

(defun sr:pick-corridor (/ hit)
  (if (setq hit (entsel "\nSelect a corridor alignment, edge or painted surface: "))
    (sr:corridor-source (car hit)))
)

(defun c:SRSTYLE (/ axis model mode current dimensions)
  (if (and (sr:modelspace-p)
           (or (= 0 (logand 2 (getvar "UNDOCTL")))
               (progn (princ "\nEnable UNDO (All) first.") nil))
           (setq axis (sr:pick-corridor)))
    (if (sr:model-valid-p (setq model (sr:model axis)))
      (progn
        (setq current (car (sr:style-owned (cdr (assoc 5 (entget axis))))))
        (initget "YellowDash DoubleYellow WhiteDash NoCenter Clear")
        (setq mode (getkword
          (strcat "\nAppearance [YellowDash/DoubleYellow/WhiteDash/NoCenter/Clear] <"
                  (if current (sr:style-scheme current) "YellowDash") ">: ")))
        (if (equal mode "Clear")
          (sr:style-change axis model nil nil)
          (progn
            (if (null mode) (setq mode (if current (sr:style-scheme current) "YellowDash")))
            (setq dimensions (sr:style-dimensions current))
            (if (= 0 (getvar "INSUNITS"))
              (princ "\nINSUNITS is unitless. Markings assume 1 drawing unit = 1 meter."))
            (initget "Keep Set")
            (if (equal "Set" (getkword "\nMarking dimensions [Keep/Set] <Keep>: "))
              (setq dimensions
                (sr:prompt-marking-dimensions dimensions)))
            (sr:style-change axis model mode dimensions))))
      (princ "\nSelect the source alignment of a Smart Road corridor.")))
  (princ)
)

;; Keep the same corner offsets in the sketch and on the resulting polyline.
(defun sr:draft-frames (points / a b frames direction)
  (while (and (cdr points)
              (setq b (droad:unit-vec (mapcar '- (cadr points) (car points))))
              (setq direction (sr:offset-direction a b)))
    (setq frames (cons (list (car points) direction) frames)
          a b points (cdr points)))
  (if (not (cdr points))
    (reverse (cons (list (car points) (sr:offset-direction a nil)) frames)))
)

(defun sr:road-sketch-preview (points profile scheme dimensions / sr:preview-frames edge)
  (sr:preview-path points 3)
  (if (and (cdr points) (setq sr:preview-frames (sr:draft-frames points)))
    (progn
      (foreach edge (sr:profile-offsets profile)
        (sr:preview-path (sr:draft-offset-path sr:preview-frames (car edge)) 3))
      (if scheme
        (sr:road-style-preview nil (list (list 0.0 0.0 profile nil)) scheme dimensions))
      T))
)

(defun sr:road-style-preview (axis spans sr:appearance-scheme sr:appearance-dimensions
                              / sr:appearance-preview)
  (setq sr:appearance-preview T)
  (sr:style-spans axis spans)
)

(defun sr:road-sketch (profile scheme dimensions / start current pending event done)
  (if (setq start (getpoint "\nStart of alignment: "))
    (progn
      (setq start (trans start 1 0) pending (list start))
      (princ "\nClick to add straight segments. Enter finishes, Backspace removes the last point.")
      (while (not done)
        (setq event (grread T 15 0))
        (cond
          ((member (car event) '(3 5))
           (setq current (trans (cadr event) 1 0)
                 current (list (car current) (cadr current) (caddr start)))
           (redraw)
           (if (> (distance current (car pending)) 1e-6)
             (progn
               (setq current (cons current pending))
               (if (= 3 (car event)) (setq pending current)))
             (setq current pending))
           (sr:road-sketch-preview (reverse current) profile scheme dimensions))
          ((and (= 2 (car event)) (member (cadr event) '(13 32))) (setq done T))
          ((equal event '(2 27)) (setq pending nil done T))
          ((and (equal event '(2 8)) (cdr pending))
           (setq pending (cdr pending))
           (redraw)
           (sr:road-sketch-preview (reverse pending) profile scheme dimensions))))
      (redraw)
      (if (cdr pending) (reverse pending))))
)

(defun sr:create-road-profile (dedicated / active keyword default choice)
  (if (setq active (sr:quick-profile))
    (progn
      (setq keyword (if dedicated "Active" "Last")
            default (if dedicated keyword "Configure"))
      (if dedicated
        (progn
          (princ "\nActive cross section (drawing units):")
          (sr:profile-summary active)))
      (initget (strcat "Configure " keyword))
      (setq choice (getkword (strcat "\nCross section [Configure/" keyword "] <" default ">: ")))
      (if (null choice) (setq choice default))))
  (if (and active (equal choice keyword))
    active
    (sr:configure-profile (sr:preset "Road")))
)

(defun sr:road-appearance-mode (/ mode)
  (initget "Edges YellowDash DoubleYellow WhiteDash NoCenter")
  (setq mode (getkword "\nAppearance [Edges/YellowDash/DoubleYellow/WhiteDash/NoCenter] <Edges>: "))
  (if (not (equal mode "Edges")) mode)
)

(defun sr:create-road (mode / *error* profile filter selection axes skipped points scheme dimensions total
                             axis new-axis doc undo built sr:new sr:owner sr:previewing)
  (defun *error* (msg)
    (redraw)
    (if (and new-axis (not built) (entget new-axis)) (entdel new-axis))
    (if undo (vla-EndUndoMark doc))
    (if (and msg (not (wcmatch (strcase msg) "*CANCEL*,*QUIT*,*BREAK*")))
      (princ (strcat "\nCreate Road: " msg)))
    (princ))
  (if (and (sr:modelspace-p) (= 0 (logand 2 (getvar "UNDOCTL"))))
    (progn
      (if (= 0 (getvar "INSUNITS"))
        (princ "\nUnitless drawing: default dimensions assume one drawing unit is one meter."))
      (setq filter '((0 . "LINE,ARC,LWPOLYLINE") (410 . "Model")))
      (if (not (equal mode "Draw")) (setq selection (ssget "_I" filter)))
      (if (setq profile (sr:create-road-profile mode))
        (progn
          (if (null mode)
            (progn
              (initget "Draw Select")
              (setq mode (getkword "\nAlignment [Draw/Select] <Select>: "))))
          (if (not (equal mode "Draw"))
            (if (or selection (setq selection (ssget filter)))
              (setq axes (sr:quick-axes selection)
                    skipped (- (sslength selection) (length axes)))))
          (if (and skipped (> skipped 0))
            (princ (strcat "\nSkipped " (itoa skipped) " managed, locked or unsuitable alignments.")))
          (if (or (equal mode "Draw") axes)
            (progn
              (setq scheme (sr:road-appearance-mode) dimensions (sr:style-dimensions nil)
                    total (apply '+ (append (mapcar 'cadr (car profile)) (mapcar 'cadr (cadr profile)))))
              (if scheme
                (progn
                  (initget "Default Set")
                  (if (equal "Set" (getkword "\nMarking dimensions [Default/Set] <Default>: "))
                    (setq dimensions
                      (sr:prompt-marking-dimensions dimensions)))))
              (if (equal mode "Draw") (setq points (sr:road-sketch profile scheme dimensions)))
              (if (or points axes)
                (progn
                  (princ (strcat "\n" (itoa (if points 1 (length axes)))
                    " road(s), total width per road " (rtos total 2 4)
                    " drawing units (" (rtos (/ total (sr:unit-scale)) 2 4) " m)."))
                  (if points
                    (if (not (sr:road-sketch-preview points profile scheme dimensions))
                      (progn (princ "\nThe alignment has a sharp bend that cannot retain the section width.") (exit)))
                    (progn
                      (setq sr:previewing T)
                      (foreach axis axes
                        (sr:render axis (list profile nil))
                        (if scheme
                          (sr:road-style-preview axis (sr:spans axis (list profile nil)) scheme dimensions)))
                      (setq sr:previewing nil)))
                  (initget "Build Cancel")
                  (if (equal "Build" (getkword "\n[Build/Cancel] <Cancel>: "))
                    (progn
                      (redraw)
                      (if points
                        (progn
                          (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
                          (if (= 0 (logand 8 (getvar "UNDOCTL")))
                            (progn (vla-StartUndoMark doc) (setq undo T)))
                          (setq new-axis (sr:poly points nil (getvar "CLAYER"))
                                axes (list new-axis))))
                      (setq built (sr:quick-build axes profile scheme dimensions))
                      (if (and new-axis (not built) (entget new-axis)) (entdel new-axis))
                      (if undo (progn (vla-EndUndoMark doc) (setq undo nil)))
                      (if built
                        (progn
                          (sr:remember-quick-profile profile)
                          (princ (strcat "\nCreated " (itoa (length axes)) " road corridor(s)."))))))
                  (redraw)))))))))
  (princ)
)

(defun c:SRDRAWROAD () (sr:create-road "Draw"))
(defun c:SRBUILDROAD () (sr:create-road "Select"))
(defun c:SRCREATEROAD () (sr:create-road nil))

(vl-catch-all-apply 'droad:toolbar (list nil))
(vl-catch-all-apply 'droad:hide-toolbar nil)
(princ "\nSmart Road loaded. Use the Smart Road ribbon or SRDRAWROAD, SRBUILDROAD, SRPROFILE.")
(princ)
