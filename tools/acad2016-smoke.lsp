;;; Load Smart Road.lsp first. Run each smoke command in a new blank test drawing.
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
     (sr:write-drawing-units (if (equal units "Millimeters") "mm" "m") "m")
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

;;; SRSMOKECHECK verifies that plugin units are independent of INSUNITS.
;;; It writes Smart Road settings into the blank test drawing.
;;; The Windows profile setting is restored when the checks finish.

(defun srsmoke:check (label actual expected)
  (if (equal actual expected 1e-9)
    (progn (setq srsmoke:passed (1+ srsmoke:passed)) T)
    (progn
      (setq srsmoke:failed (1+ srsmoke:failed))
      (princ (strcat "\nFAIL " label ": got " (vl-prin1-to-string actual)
                     ", expected " (vl-prin1-to-string expected)))
      nil))
)

(defun srsmoke:units (geometry scale / lane insunits)
  (sr:write-drawing-units geometry "m")
  (srsmoke:check "drawing units round trip" (sr:read-drawing-units) (list geometry "m"))
  (foreach insunits '(0 2 4 6)
    (setvar "INSUNITS" insunits)
    (srsmoke:check "unit scale independent of INSUNITS" (sr:unit-scale) scale)
    (srsmoke:check "road width independent of INSUNITS" (sr:default 'road-width) (* 7.0 scale)))
  (srsmoke:check "junction radius" (sr:default 'junction-radius) (* 2.0 scale))
  (srsmoke:check "chord tolerance" (sr:default 'chord-tolerance) (* 0.005 scale))
  (srsmoke:check "1 mm tolerance" (sr:tol 1e-3) (* 1e-3 scale))
  (setq lane (list (list "Lane" (* 3.5 scale))))
  (srsmoke:check "default profile" (sr:default-profile) (list lane lane))
  (srsmoke:check "marking dimensions in meters" (sr:style-dimensions nil) '(0.12 3.0 6.0 0.12))
  (setvar "LTSCALE" 1.0)
  (srsmoke:check "center linetype scale"
    (droad:center-ltscale) (/ (* 50.8 scale) (cdr (assoc 40 (tblsearch "LTYPE" "CENTER")))))
  (setvar "LTSCALE" 1000.0)
  (srsmoke:check "center linetype scale compensates for LTSCALE"
    (* 1000.0 (droad:center-ltscale)) (/ (* 50.8 scale) (cdr (assoc 40 (tblsearch "LTYPE" "CENTER")))))
  (srsmoke:check "drawn length displayed in meters" (sr:display-length (* 3.5 scale)) 3.5)
  (sr:write-drawing-units geometry "mm")
  (srsmoke:check "display unit in millimeters" (sr:display-unit) "mm")
  (srsmoke:check "display length in millimeters" (sr:display-length (* 3.5 scale)) 3500.0)
  (srsmoke:check "display to meters" (sr:display-to-m 3500.0) 3.5)
  (srsmoke:check "dialog input in millimeters" (sr:tile-length "3500" nil) (* 3.5 scale))
  (srsmoke:check "unchanged dialog text preserves stored precision"
    (sr:tile-length (sr:display-text (/ scale 3.0)) (/ scale 3.0)) (/ scale 3.0))
  (srsmoke:check "display change preserves drawing width" (sr:default 'road-width) (* 7.0 scale))
)

(defun c:SRSMOKECHECK (/ *error* doc insunits ltscale saved user user-checked undo sr:settings-cache)
  (defun *error* (message)
    (if undo (vla-EndUndoMark doc))
    (if user-checked (setenv "SmartRoad_Settings" (if user user "")))
    (if insunits (setvar "INSUNITS" insunits))
    (if ltscale (setvar "LTSCALE" ltscale))
    (if message (princ (strcat "\nSRSMOKECHECK stopped: " message)))
    (princ))
  (cond
    ((or (/= "Model" (getvar "CTAB")) (ssget "_X" '((410 . "Model"))))
     (princ "\nRun in the Model tab of a new blank test drawing."))
    ((or (= 0 (logand 1 (getvar "UNDOCTL"))) (/= 0 (logand 10 (getvar "UNDOCTL"))))
     (princ "\nEnable UNDO (All) and close any open undo group before testing."))
    (T
     (setq sr:settings-cache '(nil nil)
           srsmoke:passed 0 srsmoke:failed 0
           doc (vla-get-ActiveDocument (vlax-get-acad-object))
           insunits (getvar "INSUNITS") ltscale (getvar "LTSCALE"))
     (droad:ensure-layers doc)
     (if (not (tblsearch "LTYPE" "CENTER"))
       (princ "\nCENTER linetype could not be loaded; linetype checks will fail."))
     (srsmoke:check "all legacy physical unit choices retained" (length sr:units) 21)
     (srsmoke:check "mile unit factor" (sr:unit-factor "mi") (/ 1.0 1609.344))
     (srsmoke:units "m" 1.0)
     (srsmoke:units "mm" 1000.0)
     (srsmoke:units "ft" (/ 1.0 0.3048))
     (sr:write-drawing-units "m" "m")
     (setq saved (sr:pack-profile (sr:default-profile)))
     (sr:write-drawing-units "mm" "ft")
     (setvar "INSUNITS" 0)
     (srsmoke:check "portable template loads in different plugin units"
       (sr:unpack-profile saved nil) '((("Lane" 3500.0)) (("Lane" 3500.0))))
     (srsmoke:check "legacy tagged template uses stored source scale"
       (sr:unpack-profile
         (vl-prin1-to-string '("SR-PROFILE-1" 2 3.280839895 ((("Lane" 11.4829396325)) (("Lane" 11.4829396325)))))
         nil)
       '((("Lane" 3500.0)) (("Lane" 3500.0))))
     (setq saved (vl-prin1-to-string '((("Lane" 3.5)) (("Lane" 3.5)))))
     (srsmoke:check "unlabelled quick template has no assumed units"
       (sr:unpack-profile saved nil) nil)
     (srsmoke:check "unlabelled template accepts explicit source units"
       (sr:unpack-profile saved "m") '((("Lane" 3500.0)) (("Lane" 3500.0))))
     (setq undo (sr:begin-undo doc))
     (srsmoke:check "opens undo group" undo T)
     (srsmoke:check "no nested undo group" (sr:begin-undo doc) nil)
     (if undo (vla-EndUndoMark doc))
     (setq undo nil)
     (setq sr:settings-cache (list '((road-width . 8.0)) '((road-width . 9.0) (lane-width . 3.25))))
     (srsmoke:check "drawing setting wins" (sr:default-m 'road-width) 8.0)
     (srsmoke:check "user default next" (sr:default-m 'lane-width) 3.25)
     (srsmoke:check "built-in last" (sr:default-m 'turn-width) 3.25)
     (srsmoke:check "inherited ignores drawing" (sr:inherited-m 'road-width) 9.0)
     (srsmoke:check "source names" (mapcar 'sr:setting-source '(road-width lane-width turn-width))
       '("drawing" "my default" "built-in"))
     (srsmoke:check "invalid settings dropped"
       (sr:clean-settings '((road-width . -1.0) (junction-radius . 0) (no-such . 1.0) (lane-width . 3.0)))
       '((junction-radius . 0.0) (lane-width . 3.0)))
     (setq saved (sr:read-drawing-settings))
     (sr:write-drawing-settings '((turn-taper . 12.5) (plot-scale . 500.0)))
     (srsmoke:check "drawing defaults round trip" (sr:read-drawing-settings)
       '((turn-taper . 12.5) (plot-scale . 500.0)))
     (srsmoke:check "editing defaults retains units" (sr:read-drawing-units) '("mm" "ft"))
     (sr:write-drawing-settings saved)
     (setq user (getenv "SmartRoad_Settings") user-checked T)
     (sr:write-user-settings '((marking-dash . 4.0)))
     (srsmoke:check "user defaults round trip" (sr:read-user-settings) '((marking-dash . 4.0)))
     (*error* nil)
     (princ (strcat "\nSRSMOKECHECK: " (itoa srsmoke:passed) " passed, " (itoa srsmoke:failed) " failed."))))
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

(defun c:SRSMOKEPLAN (/ sr:new sr:collect-plan sr:previewing east at kind radius sr:settings-cache)
  (setq sr:settings-cache '(nil nil))
  (cond
    ((/= "Model" (getvar "CTAB")) (princ "\nRun in the Model tab of a blank test drawing."))
    ((ssget "_X" '((410 . "Model"))) (princ "\nUse a blank test drawing."))
    (T
     (setq srsmoke:passed 0 srsmoke:failed 0 east '(1.0 0.0 0.0))
     (if (null (sr:read-drawing-units)) (sr:write-drawing-units "m" "m"))
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

;;; SRSMOKESTYLE checks the actual entities drawn for dashed road markings.
;;; Run in a blank drawing. It leaves a road with a deliberate gap for inspection.

(defun srsmoke:dashed-markings (owner layer / result entity data linetype)
  (foreach entity (sr:style-owned owner)
    (setq data (entget entity) linetype (cdr (assoc 6 data)))
    (if (and (equal (cdr (assoc 8 data)) layer)
             linetype (not (member (strcase linetype) '("CONTINUOUS" "BYLAYER" "BYBLOCK"))))
      (setq result (cons entity result))))
  result
)

(defun srsmoke:check-dashed-entity (entity dimensions scale / data record pattern factor item)
  (if (srsmoke:check "dashed entity exists" (not (null entity)) T)
    (progn
      (setq data (entget entity)
            record (tblsearch "LTYPE" (cdr (assoc 6 data)))
            factor (* (getvar "LTSCALE") (cond ((cdr (assoc 48 data))) (1.0))))
      (foreach item record
        (if (= 49 (car item)) (setq pattern (cons (* factor (cdr item)) pattern))))
      (srsmoke:check "marking is one lightweight polyline" (cdr (assoc 0 data)) "LWPOLYLINE")
      (srsmoke:check "linetype generation across vertices" (logand 128 (cdr (assoc 70 data))) 128)
      (srsmoke:check "marking width" (cdr (assoc 43 data)) (* (car dimensions) scale))
      (srsmoke:check "dash and gap from Linetype"
        (reverse pattern) (list (* (cadr dimensions) scale) (* (- (caddr dimensions)) scale)))))
)

(defun c:SRSMOKESTYLE (/ axis owner scale profile widened model dimensions previous center divider entity extent
                        arc record sr:new sr:settings-cache)
  (cond
    ((or (/= "Model" (getvar "CTAB")) (ssget "_X" '((410 . "Model"))))
     (princ "\nRun in the Model tab of a new blank test drawing."))
    ((not (and (sr:modelspace-p) (= 0 (logand 10 (getvar "UNDOCTL")))))
     (princ "\nEnable UNDO (All) and close any open undo group before testing."))
    (T
     (if (null (sr:read-drawing-units)) (sr:write-drawing-units "m" "m"))
     (setvar "FILLMODE" 1)
     (setq sr:settings-cache '(nil nil)
           srsmoke:passed 0 srsmoke:failed 0
           scale (sr:unit-scale)
           dimensions '(0.15 2.25 4.75 0.12)
           profile (sr:scale-default-profile '((("Lane" 3.5) ("Lane" 3.5)) (("Lane" 3.5))))
           widened (sr:scale-default-profile '((("Lane" 4.0) ("Lane" 3.5)) (("Lane" 3.5))))
           axis (srsmoke:line 0.0 0.0 100.0 0.0 scale "SR-TEST-MARKINGS")
           owner (cdr (assoc 5 (entget axis)))
           model (list profile (list (list (* 20.0 scale) (* 50.0 scale) widened (* 5.0 scale)))))
     (if (srsmoke:check "build base corridor" (sr:quick-build (list axis) profile "YellowDash" dimensions) T)
       (progn
         (setq center (srsmoke:dashed-markings owner "SR-MARK-YELLOW")
               divider (srsmoke:dashed-markings owner "SR-MARK-WHITE"))
         (srsmoke:check "one center marking on base corridor" (length center) 1)
         (srsmoke:check "one divider on base corridor" (length divider) 1)
         (srsmoke:check-dashed-entity (car center) dimensions scale)
         (srsmoke:check-dashed-entity (car divider) dimensions scale)
         (setq previous (append center divider))
         (if (srsmoke:check "rebuild corridor with tapers" (sr:build axis model) T)
           (progn
             (setq center (srsmoke:dashed-markings owner "SR-MARK-YELLOW")
                   divider (srsmoke:dashed-markings owner "SR-MARK-WHITE"))
             (srsmoke:check "one center marking across contiguous spans" (length center) 1)
             (srsmoke:check "one divider across taper spans" (length divider) 1)
             (srsmoke:check "rebuild removes previous dashed entities" (vl-some 'entget previous) nil)
             (if (car divider)
               (srsmoke:check "divider still follows moving lane boundary"
                 (> (cdr (assoc 90 (entget (car divider)))) 2) T))))
         (sr:style-change axis model nil nil)
         (srsmoke:check "clearing removes all owned appearance" (sr:style-owned owner) nil)
         (sr:style-render axis
           (sr:span-without-ranges (list 0.0 (* 100.0 scale) profile nil)
                                  (list (list (* 45.0 scale) (* 55.0 scale))))
           owner "YellowDash" dimensions)
         (setq center (srsmoke:dashed-markings owner "SR-MARK-YELLOW")
               divider (srsmoke:dashed-markings owner "SR-MARK-WHITE"))
         (srsmoke:check "center marking stops at excluded stations" (length center) 2)
         (srsmoke:check "divider stops at excluded stations" (length divider) 2)
         (foreach entity (append center divider)
           (setq extent (sr:extent (cdadr (assoc -3 (entget entity '("SR-ROAD-STYLE"))))))
           (srsmoke:check "marking does not bridge the excluded interval"
             (or (<= (cadr extent) (* 45.0 scale)) (>= (car extent) (* 55.0 scale))) T))))
     (setq arc (sr:arc (list 0.0 (* -100.0 scale) 0.0) (* 40.0 scale)
                       0.0 (/ pi 2.0) (sr:layer "SR-TEST-MARKINGS-ARC" 4)))
     (if (srsmoke:check "build ARC corridor" (sr:quick-build (list arc) profile "YellowDash" dimensions) T)
       (progn
         (setq owner (cdr (assoc 5 (entget arc)))
               center (srsmoke:dashed-markings owner "SR-MARK-YELLOW")
               record (if (car center) (entget (car center))))
         (srsmoke:check "one center marking on the ARC" (length center) 1)
         (srsmoke:check "center marking preserves an arc segment"
           (and (assoc 42 record) (> (cdr (assoc 42 record)) 0.0)) T)))
     (vl-cmdf "_.ZOOM" "_Extents")
     (princ (strcat "\nSRSMOKESTYLE: " (itoa srsmoke:passed) " passed, " (itoa srsmoke:failed) " failed."))))
  (princ)
)
