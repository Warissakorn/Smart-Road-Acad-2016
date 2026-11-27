;;; Load Smart Road.lsp first. Run SRSMOKE only in a new blank test drawing.
;;; Use the fixture with the AutoCAD 2016 checks in README.md.

(defun srsmoke:line (x1 y1 x2 y2 scale layer)
  (if (entmake (list '(0 . "LINE") (cons 8 (sr:layer layer 4))
                     (cons 10 (list (* x1 scale) (* y1 scale) 0.0))
                     (cons 11 (list (* x2 scale) (* y2 scale) 0.0))))
    (entlast))
)

(defun c:SRSMOKE (/ units scale axis branch narrow profile thin suite corner)
  (cond
    ((/= "Model" (getvar "CTAB"))
     (princ "\nRun in the Model tab of a blank test drawing."))
    ((ssget "_X" '((410 . "Model")))
     (princ "\nUse a blank test drawing. SRSMOKE leaves fixture geometry for inspection."))
    ((not (and (sr:modelspace-p) (= 0 (logand 2 (getvar "UNDOCTL")))))
     (princ "\nEnable UNDO (All) before preparing fixtures."))
    (T
     (initget "Meters Millimeters")
     (setq units (getkword "\nFixture units [Meters/Millimeters] <Meters>: "))
     (setvar "INSUNITS" (if (equal units "Millimeters") 4 6))
     (setvar "FILLMODE" 1)
     (setq scale (sr:unit-scale)
           profile (sr:scale-default-profile '((("Lane" 3.5)) (("Lane" 3.5))))
           thin (sr:scale-default-profile '((("Lane" 0.1)) (("Lane" 0.1))))
           axis (srsmoke:line 0.0 0.0 100.0 0.0 scale "SR-TEST-MAIN")
           branch (srsmoke:line 90.0 -35.0 90.0 35.0 scale "SR-TEST-BRANCH")
           narrow (srsmoke:line 0.0 30.0 40.0 30.0 scale "SR-TEST-NARROW")
           corner (* pi (/ 160.0 180.0)))
     (if (and axis branch narrow
              (entmake (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity")
                             (cons 8 (sr:layer "SR-TEST-SHARP" 6))
                             '(100 . "AcDbPolyline") '(90 . 3) '(70 . 0)
                             (cons 10 (list 0.0 (* 80.0 scale)))
                             (cons 10 (list (* 150.0 scale) (* 80.0 scale)))
                             (cons 10 (list (* (+ 150.0 (* 150.0 (cos corner))) scale)
                                            (* (+ 80.0 (* 150.0 (sin corner))) scale))))))
       (progn
         (setq suite (list axis branch))
         (if (and (sr:quick-build suite profile "YellowDash" (sr:style-dimensions nil))
                  (sr:quick-build (list narrow) thin "DoubleYellow" (sr:style-dimensions nil)))
           (progn
             (vl-cmdf "_.ZOOM" "_Extents")
             (princ "\nFixtures ready: MAIN, BRANCH, NARROW and SHARP layers. Follow the README checks."))
           (princ "\nFixture build did not finish. Inspect the AutoCAD command history.")))
       (princ "\nFixture geometry could not be created."))))
  (princ)
)
