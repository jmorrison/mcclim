(in-package :climi)

(define-abstract-pane-mapping 'menu-button 'menu-button-pane*)

(defclass menu-button-pane* (menu-button-pane)
  ((icon :initarg :icon
         :initform nil
         :accessor icon)
   (icon-position :initarg :icon-position
                  :accessor icon-position
                  :initform :left)
   (icon-padding :initarg :icon-padding
                 :accessor icon-padding
                 :initform (make-padding 3 3 3 3))))

(defmethod handle-repaint ((pane menu-button-pane*) region)
  (declare (ignore region))
  (with-slots (x-spacing y-spacing) pane
    (with-special-choices (pane)
      (let ((region (sheet-region pane)))
        (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* region)
          (draw-rectangle* pane x1 y1 x2 y2
                           :ink (effective-gadget-background pane)
                           :filled t)
          (cond ((slot-value pane 'armed)
                 (draw-bordered-rectangle* pane x1 y1 x2 y2 :style :outset :border-width *3d-border-thickness*))
                (t))
          (multiple-value-bind (x1 y1 x2 y2)
              (values (+ x1 x-spacing) (+ y1 y-spacing)
                      (- x2 x-spacing) (- y2 y-spacing))
            (let ((x1 (if (icon pane)
                          (progn
                            (draw-pattern* pane
                                           (icon pane)
                                           (+ x1 (padding-left (icon-padding pane)))
                                           (- (+ y1 (/ (- y2 y1) 2)) (/ (pattern-height (icon pane)) 2)))
                            (+ x1
                               (padding-left (icon-padding pane))
                               (pattern-width (icon pane))
                               (padding-right (icon-padding pane))))
                          x1)))
              (if (gadget-active-p pane)
                  (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
                  (draw-engraved-label* pane x1 y1 x2 y2)))))))))

(defmethod compose-space ((gadget menu-button-pane*) &key width height)
  (declare (ignore width height))
  (if (not (icon gadget))
      (call-next-method)
      (let* ((icon-space-width (+ (padding-left (icon-padding gadget))
                                  (pattern-width (icon gadget))
                                  (padding-right (icon-padding gadget))))
             (icon-space-height (+ (padding-top (icon-padding gadget))
                                   (pattern-height (icon gadget))
                                   (padding-bottom (icon-padding gadget))))
             (icon-space-requirement (make-space-requirement :width icon-space-width
                                                             :min-width icon-space-width
                                                             :max-width icon-space-width
                                                             :height icon-space-height
                                                             :min-height icon-space-height
                                                             :max-height icon-space-height))
             (label-space-requirement (space-requirement+* (compose-label-space gadget)
                                                           :min-width (* 2 (pane-x-spacing gadget))
                                                           :width (* 2 (pane-x-spacing gadget))
                                                           :max-width (* 2 (pane-x-spacing gadget))
                                                           :min-height (* 2 (pane-y-spacing gadget))
                                                           :height (* 2 (pane-y-spacing gadget))
                                                           :max-height (* 2 (pane-y-spacing gadget))))
             (inside-space-requirement (let ((width (+ (space-requirement-width label-space-requirement)
                                                       (space-requirement-width icon-space-requirement)))
                                             (height (max (space-requirement-height label-space-requirement)
                                                          (space-requirement-height icon-space-requirement))))
                                         (make-space-requirement :width width
                                                                 :min-width width
                                                                 :max-width width
                                                                 :height height
                                                                 :min-height height
                                                                 :max-height height))))
        (space-requirement+* inside-space-requirement
                             :min-width (* 2 *3d-border-thickness*)
                             :width (* 2 *3d-border-thickness*)
                             :max-width (* 2 *3d-border-thickness*)
                             :min-height (* 2 *3d-border-thickness*)
                             :height (* 2 *3d-border-thickness*)
                             :max-height (* 2 *3d-border-thickness*)))))

;; Plugging

(defclass menu-button-leaf-pane* (menu-button-pane* menu-button-leaf-pane)
  ())

(defclass menu-button-submenu-pane* (menu-button-pane* menu-button-submenu-pane)
  ())

(define-abstract-pane-mapping 'menu-button-leaf-pane 'menu-button-leaf-pane*)
(define-abstract-pane-mapping 'menu-button-submenu-pane 'menu-button-submenu-pane*)