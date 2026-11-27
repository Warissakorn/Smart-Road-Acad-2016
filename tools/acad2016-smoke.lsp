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

;;; SRSMOKECHECK runs without prompts and prints PASS/FAIL for the unit and
;;; default helpers in meter, millimeter and foot drawings. It restores
;;; INSUNITS and LTSCALE, and loads the CENTER linetype and road layers.

(defun srsmoke:check (label actual expected)
  (if (equal actual expected 1e-9)
    (setq srsmoke:passed (1+ srsmoke:passed))
    (progn
      (setq srsmoke:failed (1+ srsmoke:failed))
      (princ (strcat "\nFAIL " label ": got " (vl-prin1-to-string actual)
                     ", expected " (vl-prin1-to-string expected)))))
)

(defun srsmoke:units (insunits scale / lane saved)
  (setvar "INSUNITS" insunits)
  (srsmoke:check "unit-scale" (sr:unit-scale) scale)
  (srsmoke:check "road width" (sr:default 'road-width) (* 7.0 scale))
  (srsmoke:check "junction radius" (sr:default 'junction-radius) (* 2.0 scale))
  (srsmoke:check "chord tolerance" (sr:default 'chord-tolerance) (* 0.005 scale))
  (srsmoke:check "1 mm tolerance" (sr:tol 1e-3) (* 1e-3 scale))
  (setq lane (list (list "Lane" (* 3.5 scale))))
  (srsmoke:check "default profile" (sr:default-profile) (list lane lane))
  (srsmoke:check "marking dimensions" (sr:style-dimensions nil) '(0.12 3.0 6.0 0.12))
  (setvar "LTSCALE" 1.0)
  (srsmoke:check "center ltscale"
    (droad:center-ltscale) (/ (* 50.8 scale) (cdr (assoc 40 (tblsearch "LTYPE" "CENTER")))))
  (setvar "LTSCALE" 1000.0)
  (srsmoke:check "center ltscale follows LTSCALE"
    (* 1000.0 (droad:center-ltscale)) (/ (* 50.8 scale) (cdr (assoc 40 (tblsearch "LTYPE" "CENTER")))))
  (srsmoke:check "unitless warning returns T" (sr:warn-unitless) T)
  (srsmoke:check "meters text" (sr:meters-text (* 3.5 scale)) (rtos 3.5 2 8))
  (srsmoke:check "dialog keeps unchanged value"
    (sr:tile-meters (sr:meters-text (/ scale 3.0)) (/ scale 3.0)) (/ scale 3.0))
  (srsmoke:check "dialog reads meters" (sr:tile-meters "2.5" nil) (* 2.5 scale))
  (srsmoke:check "dialog rejects text" (sr:tile-meters "abc" nil) nil)
)

(defun c:SRSMOKECHECK (/ doc insunits ltscale saved)
  (setq srsmoke:passed 0 srsmoke:failed 0
        doc (vla-get-ActiveDocument (vlax-get-acad-object))
        insunits (getvar "INSUNITS") ltscale (getvar "LTSCALE"))
  (droad:ensure-layers doc)
  (if (not (tblsearch "LTYPE" "CENTER"))
    (princ "\nCENTER linetype could not be loaded; linetype checks will fail."))
  (srsmoke:units 6 1.0)
  (srsmoke:units 4 1000.0)
  (srsmoke:units 2 3.280839895)
  ;; Templates saved in meters load scaled in millimeters, and not into a unitless drawing.
  (setvar "INSUNITS" 6)
  (setq saved (sr:pack-profile (sr:default-profile)))
  (setvar "INSUNITS" 4)
  (srsmoke:check "template meters -> millimeters" (sr:unpack-profile saved nil)
    '((("Lane" 3500.0)) (("Lane" 3500.0))))
  (setvar "INSUNITS" 0)
  (srsmoke:check "template refused in unitless drawing" (sr:unpack-profile saved nil) nil)
  ;; An undo group is opened only when none is open.
  (setvar "INSUNITS" insunits)
  (vla-StartUndoMark doc)
  (srsmoke:check "no nested undo group" (sr:begin-undo doc) nil)
  (vla-EndUndoMark doc)
  (setq saved (sr:begin-undo doc))
  (srsmoke:check "opens undo group" saved T)
  (if saved (vla-EndUndoMark doc))
  (setvar "LTSCALE" ltscale)
  (princ (strcat "\nSRSMOKECHECK: " (itoa srsmoke:passed) " passed, " (itoa srsmoke:failed) " failed."))
  (princ)
)

;;; SRSMOKEPLAN draws every plan feature with the default dimensions through
;;; the sr:draw-* builders, without prompts, and checks the entity count per
;;; layer. Run it in a new blank drawing; it leaves the geometry for review.
;;; Expected counts follow from the builders: one entity per shape, box,
;;; circle, arc or solid.

(defun srsmoke:count (layer / found)
  (if (setq found (ssget "_X" (list (cons 8 layer) '(410 . "Model")))) (sslength found) 0)
)

(defun c:SRSMOKEPLAN (/ sr:new sr:collect-plan sr:previewing east at kind radius)
  (cond
    ((/= "Model" (getvar "CTAB")) (princ "\nRun in the Model tab of a blank test drawing."))
    ((ssget "_X" '((410 . "Model"))) (princ "\nUse a blank test drawing."))
    (T
     (setq srsmoke:passed 0 srsmoke:failed 0 east '(1.0 0.0 0.0))
     (setvar "FILLMODE" 1)
     (defun at (column) (list (* column 60.0 (sr:unit-scale)) 0.0 0.0))
     (sr:draw-crosswalk (at 0) east (sr:default 'crossing-span) (sr:default 'crossing-depth)
       (fix (/ (sr:default 'crossing-span) (sr:default 'crossing-pitch))))
     (foreach kind '("Island" "Ramp" "Stop" "Platform")
       (sr:draw-pad kind (at (1+ (vl-position kind '("Island" "Ramp" "Stop" "Platform")))) east
         (sr:default (cdr (assoc kind '(("Island" . island-length) ("Ramp" . island-length)
                                         ("Stop" . stop-length) ("Platform" . platform-length)))))
         (sr:default (cdr (assoc kind '(("Island" . island-width) ("Ramp" . island-width)
                                         ("Stop" . stop-width) ("Platform" . platform-width)))))))
     (sr:draw-turn-lane (at 6) east (- (sr:default 'turn-width)) (sr:default 'turn-length) (sr:default 'turn-taper))
     (sr:draw-junction "Cross" (at 8) east (sr:default 'approach-width) (sr:default 'approach-length)
       (sr:default 'junction-radius))
     (sr:draw-junction "Tee" (at 10) east (sr:default 'approach-width) (sr:default 'approach-length)
       (sr:default 'junction-radius))
     (setq radius (sr:default 'roundabout-radius))
     (srsmoke:check "roundabout fits with defaults"
       (sr:roundabout-fits-p radius (* radius 0.45) (sr:default 'approach-width)) T)
     (sr:draw-roundabout (at 12) east radius (* radius 0.45) (sr:default 'approach-width)
       (sr:default 'roundabout-approach))
     ;; 7.0 / 0.75 gives 9 stripes, each a box and a solid.
     (srsmoke:check "crosswalk boxes" (srsmoke:count "SR-CROSSWALK") 9)
     (srsmoke:check "crosswalk solids" (srsmoke:count "SR-MARK-CROSSWALK") 9)
     (srsmoke:check "island" (srsmoke:count "SR-ISLAND") 1)
     (srsmoke:check "ramp box and diagonals" (srsmoke:count "SR-RAMP") 3)
     (srsmoke:check "stop box and pole" (srsmoke:count "SR-STOP") 2)
     (srsmoke:check "platform" (srsmoke:count "SR-PLATFORM") 1)
     (srsmoke:check "turn lane" (srsmoke:count "SR-TURN-LANE") 2)
     ;; Cross: 4 corners x (2 edges + 1 arc) = 12. Tee: 2 corners x 3 + far edge = 7.
     (srsmoke:check "cross and tee" (srsmoke:count "SR-JUNCTION") 19)
     ;; Island circle + 4 x (arc + 2 approach edges).
     (srsmoke:check "roundabout" (srsmoke:count "SR-ROUNDABOUT") 13)
     (vl-cmdf "_.ZOOM" "_Extents")
     (princ (strcat "\nSRSMOKEPLAN: " (itoa srsmoke:passed) " passed, " (itoa srsmoke:failed) " failed."))))
  (princ)
)
