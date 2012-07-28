(in-package :climi)

(define-abstract-pane-mapping 'list-pane 'extended-list-pane)

(defclass extended-list-pane (generic-list-pane sheet-padding-mixin)
  ((item-icon-function :initarg :item-icon-function
                       :initform (lambda (item selected-p)
                                   (declare (ignore item selected-p))
                                   nil)
                       :accessor item-icon-function)
   (item-icon-size :initarg :item-icon-size
                   :initform nil
                   :accessor item-icon-size)
   (item-padding :initarg :item-padding
                 :initform (make-empty-padding)
                 :accessor item-padding
                 :documentation "The displayed items padding")
   (item-name-padding :initarg :item-name-padding
                      :initform (make-empty-padding)
                      :accessor item-name-padding)
   (item-icon-padding :initarg :item-icon-padding
                      :initform (make-empty-padding)
                      :accessor item-icon-padding))
  (:documentation "The extended-list-pane implementation handles padding and icons"))

(defmethod generic-list-pane-items-width ((pane extended-list-pane))
  (with-slots (items-width) pane
    (or items-width
        (setf items-width
              (+ (padding-left (item-padding pane))
                 (if (item-icon-size pane)
                     (+
                      (padding-left (item-icon-padding pane))
                      (car (item-icon-size pane))
                      (padding-right (item-icon-padding pane)))
                     0)
                 (padding-left (item-name-padding pane))
                 (reduce #'max (map 'vector (lambda (item-string)
                                              (text-size pane item-string))
                                    (generic-list-pane-item-strings pane))
                         :initial-value 0)
                 (padding-right (item-name-padding pane)))))))

(defmethod generic-list-pane-item-height ((pane extended-list-pane))
  (+ (padding-top (item-padding pane))
     (text-style-ascent  (pane-text-style pane) pane)
     (text-style-descent (pane-text-style pane) pane)
     (padding-bottom (item-padding pane))))

(defmethod handle-repaint ((pane extended-list-pane) region)
  ;; Clear the region first. 
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    (draw-rectangle* pane x0 y0 x1 y1
                     :filled t
                     :ink (pane-background pane)))
  
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1) (sheet-region pane)
    (declare (ignore sx1 sy1))
    (with-bounding-rectangle* (rx0 ry0 rx1 ry1)
        (if (bounding-rectangle-p region)
            region
            (or (pane-viewport-region pane) ; workaround for +everywhere+
                (sheet-region pane)))
      (let ((item-height (generic-list-pane-item-height pane))
            (highlight-ink (list-pane-highlight-ink pane)))
        (do ((index (floor (- ry0 sy0) item-height) (1+ index)))
            ((or (> (+ sy0 (* item-height index)) ry1)
                 (>= index (generic-list-pane-items-length pane))))
          (let ((y0 (+ sy0 (* index item-height)))
                (y1 (+ sy0 (* (1+ index) item-height))))
            (multiple-value-bind (background foreground)
                (cond ((not (slot-boundp pane 'value))
                       (values (pane-background pane) (pane-foreground pane)))
                      ((if (list-pane-exclusive-p pane)
                           (funcall (list-pane-test pane)
                                    (elt (generic-list-pane-item-values pane) index)
                                    (gadget-value pane))
                           (member (elt (generic-list-pane-item-values pane) index) (gadget-value pane)
                                   :test (list-pane-test pane)))
                       (values highlight-ink (pane-background pane)))
                      (t (values (pane-background pane) (pane-foreground pane))))
              (draw-rectangle* pane rx0 y0 rx1 y1 :filled t :ink background)
              (let ((next-x
                     (let* ((item-value (elt (generic-list-pane-item-values pane) index))
                            (icon (and (item-icon-function pane)
                                       (funcall (item-icon-function pane)
                                                item-value
                                                (if (list-pane-exclusive-p pane)
                                                    (funcall (list-pane-test pane)
                                                             (elt (generic-list-pane-item-values pane) index)
                                                             (gadget-value pane))
                                                    (member (elt (generic-list-pane-item-values pane) index) (gadget-value pane)
                                                            :test (list-pane-test pane)))))))
                       (if icon
                           (progn
                             (draw-pattern* pane icon
                                            (+ rx0
                                               (padding-left (item-padding pane))
                                               (padding-left (item-icon-padding pane)))
                                            (- (+ y0 (/ item-height 2)) (/ (pattern-height icon) 2)))
                             (+ rx0
                                (padding-left (item-padding pane))  
                                (padding-left (item-icon-padding pane))
                                (pattern-width icon)
                                (padding-right (item-icon-padding pane))))
                           (+ rx0 (padding-left (item-padding pane)))))))
                (draw-text* pane (elt (generic-list-pane-item-strings pane) index)
                            (+ next-x (padding-left (item-name-padding pane)))
                            (+ y0
                               (padding-top (item-padding pane))
                               (text-style-ascent (pane-text-style pane) pane))
                            :ink foreground
                            :text-style (pane-text-style pane))))))))))


;; Example

(define-application-frame extended-list-test ()
  ((model :initarg :model
          :accessor model
          :initform (error "Provide the list")))
  (:panes
   (result-list
    (let ((icon (make-pattern-from-bitmap-file
                 (asdf:system-relative-pathname
                  :mcclim
                  "Apps/Listener/icons/folder.xpm")
                 :format :xpm :port nil)))
      (make-pane 'climi::list-pane
                 :items (model *application-frame*)
                                        ;:presentation-type-key (constantly 'list-test-symbol)
                 :test #'equalp
                 :item-padding (climi::make-padding 5 5 5 5)
                 :item-icon-function
                 (lambda (node selected-p)
                   (declare (ignore selected-p))
                   icon)
                 :item-icon-size (list (pattern-width icon)
                                       (pattern-height icon))
                 :item-icon-padding (climi::make-padding 5 5 5 5)))))
  (:layouts
   (defaults
       (labelling (:label "Extended list example"
                          :text-style (make-text-style :sans-serif :roman :normal))
         (vertically ()
           (scrolling (:height 200)
             result-list))))))

(defun extended-list-pane-example ()
  (clim::run-frame-top-level
   (clim::make-application-frame 'extended-list-test :model (directory #p"/home/marian/*"))))