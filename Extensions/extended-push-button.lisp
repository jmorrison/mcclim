(in-package :climi)

(defclass push-button-pane*
    ;; (push-button
    ;;  labelled-gadget-mixin
    ;;  changing-label-invokes-layout-protocol-mixin
    ;;  arm/disarm-repaint-mixin
    ;;  enter/exit-arms/disarms-mixin
    ;;  standard-gadget-pane)
    (push-button-pane)
  ((icon :initarg :icon
         :initform nil
         :accessor icon)
   (icon-position :initarg :icon-position
                  :accessor icon-position
                  :initform :left)
   (icon-padding :initarg :icon-padding
                 :accessor icon-padding
                 :initform (make-padding 3 3 3 3)))
  )

(define-abstract-pane-mapping 'push-button 'push-button-pane*)

(defmethod compose-space ((gadget push-button-pane*) &key width height)
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

(defmethod handle-repaint ((pane push-button-pane*) region)
  (declare (ignore region))
  (with-slots (armed pressedp) pane
    (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
      (draw-rectangle* pane x1 y1 x2 y2 :ink (effective-gadget-background pane))
      (draw-bordered-rectangle* pane x1 y1 x2 y2
                                :style (if (and pressedp armed) :inset :outset))
      (multiple-value-bind (x1 y1 x2 y2) (values (+ x1 *3d-border-thickness* (pane-x-spacing pane))
                                                 (+ y1 *3d-border-thickness* (pane-y-spacing pane))
                                                 (- x2 *3d-border-thickness* (pane-x-spacing pane))
                                                 (- y2 *3d-border-thickness* (pane-y-spacing pane)))
        (let ((x1 (if pressedp (+ x1 2) x1))
              (y1 (if pressedp (+ y1 2) y1)))
        ;; Draw the icon if on the left
        (let ((x1
               (if (and (icon pane) (equalp (icon-position pane) :left))
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
          
          ;; Draw the label
          (if (gadget-active-p pane)
              (draw-label* pane x1 y1 x2 y2 :ink (effective-gadget-foreground pane))
              (draw-engraved-label* pane x1 y1 x2 y2))

          ;; Draw the icon if on the right
          (when (and (icon pane) (equalp (icon-position pane) :right))
            (let ((label-space (compose-label-space pane)))
              (draw-pattern* pane
                             (icon pane)
                             (+ x1 (space-requirement-width label-space) (padding-left (icon-padding pane)))
                             (- (+ y1 (/ (- y2 y1) 2)) (/ (pattern-height (icon pane)) 2)))))))))))