;;; ==========================================================================
;;; AutoLISP: Smart Road & Auto-Junction Generator (ASCII Safe Version)
;;; Commands:
;;;   DROAD - Draw new road (Line & Arc) with auto-junction & fillet
;;;   RNET  - Convert multiple centerlines (Line/Arc/Polyline) to road network
;;; ==========================================================================

(vl-load-com)

(if (not *droad-width*)  (setq *droad-width* 6.0))
(if (not *droad-fillet*) (setq *droad-fillet* 3.0))

;; --------------------------------------------------------------------------
;; 1. Layer Setup (ROAD-CENTER and ROAD-EDGE)
;; --------------------------------------------------------------------------
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
  (setq lyr-c (vla-Add lyrs "ROAD-CENTER"))
  (vla-put-Color lyr-c 1)
  (if (tblsearch "LTYPE" "CENTER")
    (vla-put-Linetype lyr-c "CENTER")
  )
  (setq lyr-e (vla-Add lyrs "ROAD-EDGE"))
  (vla-put-Color lyr-e 3)
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
      (setq ss-cl (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-CENTER") (cons 410 (getvar "CTAB")))))
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
                          (vl-cmdf "_.PEDIT" cl-en "_Join" (vlax-vla-object->ename ext-ln) "" "")
                          (while (> (getvar "CMDACTIVE") 0) (vl-cmdf ""))
                          (if (not was-lw) (setq cl-en (entlast)))
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
                          (vl-cmdf "_.PEDIT" cl-en "_Join" (vlax-vla-object->ename ext-ln) "" "")
                          (while (> (getvar "CMDACTIVE") 0) (vl-cmdf ""))
                          (if (not was-lw) (setq cl-en (entlast)))
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
(defun droad:process-road (cl-en width fillet-rad spc / half-w vla-cl off1 off2 new-off new-simple new-enames o no oo ss-cl ss-ed i en old-cls old-edge-vlas hit-p raw active-old-simple all-int-pts split-new split-old surviving p-mid len1 len2 d1 d2 e1 e2 p1 p2 meet ss-join)
  (setq half-w (* 0.5 width)
        cl-en  (droad:extend-cl-to-junctions cl-en width spc)
        vla-cl (vlax-ename->vla-object cl-en))
  (vla-put-Layer vla-cl "ROAD-CENTER")
  (droad:set-width-xdata cl-en width)

  ;; Offset Left & Right Road Edges
  (setq off1 (vl-catch-all-apply 'vlax-invoke (list vla-cl 'Offset half-w))
        off2 (vl-catch-all-apply 'vlax-invoke (list vla-cl 'Offset (- half-w))))

  (if (or (vl-catch-all-error-p off1) (vl-catch-all-error-p off2))
    (princ "\n[Warning] Curve radius is smaller than half road width. Please use a wider curve radius.")
  )
  (if (not (vl-catch-all-error-p off1)) (setq new-off (append new-off off1)))
  (if (not (vl-catch-all-error-p off2)) (setq new-off (append new-off off2)))

  (foreach o new-off
    (vla-put-Layer o "ROAD-EDGE")
    (setq new-simple (append new-simple (droad:explode-to-simple o)))
  )
  (setq new-enames (mapcar 'vlax-vla-object->ename new-simple))

  ;; Collect Existing Centerlines
  (setq ss-cl (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-CENTER") (cons 410 (getvar "CTAB")))))
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
  (setq ss-ed (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE") '(8 . "ROAD-EDGE") (cons 410 (getvar "CTAB")))))
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

      (foreach o split-old
        (if (and o (not (vlax-erased-
