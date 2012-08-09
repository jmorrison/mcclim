(in-package :clim-internals)

;; sheet-padding mixin

(defun padding-top (padding)
  (first padding))

(defun padding-right (padding)
  (second padding))

(defun padding-bottom (padding)
  (third padding))

(defun padding-left (padding)
  (nth 3 padding))

(defun make-empty-padding ()
  (list 0 0 0 0))

(defun make-padding (top right bottom left)
  (list top right bottom left))

(defclass sheet-padding-mixin ()
  ((sheet-padding-top :initarg :sheet-padding-top
                      :accessor sheet-padding-top
                      :initform 0)
   (sheet-padding-right :initarg :sheet-padding-right
                        :accessor sheet-padding-right
                        :initform 0)
   (sheet-padding-bottom :initarg :sheet-padding-bottom
                         :accessor sheet-padding-bottom
                         :initform 0)
   (sheet-padding-left :initarg :sheet-padding-left
                       :accessor sheet-padding-left
                       :initform 0)))

(defmethod initialize-instance :after ((sheet sheet-padding-mixin) &rest initargs)
  (when (getf initargs :sheet-padding)
    (destructuring-bind (padding-top padding-right padding-bottom padding-left)
        (getf initargs :sheet-padding)
      (setf (sheet-padding-top sheet) padding-top
            (sheet-padding-right sheet) padding-right
            (sheet-padding-bottom sheet) padding-bottom
            (sheet-padding-left sheet) padding-left))))

(defmethod compose-space :around ((sheet sheet-padding-mixin) &key width height)
  (let ((space-requirement (call-next-method)))
    (flet ((padding-width (width)
             (+ width
                (sheet-padding-left sheet)
                (sheet-padding-right sheet)))
           (padding-height (height)
             (+ height
                (sheet-padding-top sheet)
                (sheet-padding-bottom sheet))))
      (make-space-requirement :width (padding-width (space-requirement-width space-requirement))
                              :height (padding-height (space-requirement-height space-requirement))
                              :min-width (padding-width (space-requirement-min-width space-requirement))
                              :min-height (padding-height (space-requirement-min-height space-requirement))
                              :max-width (padding-width (space-requirement-max-width space-requirement))
                              :max-height (padding-height (space-requirement-max-height space-requirement))))))

(defmethod sheet-padding-region ((sheet sheet-padding-mixin))
  (let ((region (sheet-region sheet)))
    (with-bounding-rectangle* (x0 y0 x1 y1) region
      (make-bounding-rectangle (+ x0 (sheet-padding-left sheet))
                               (+ y0 (sheet-padding-top sheet))
                               (- x1 (sheet-padding-right sheet))
                               (- y1 (sheet-padding-bottom sheet))))))

(defmacro with-padding (bindings padding &body body)
  `(destructuring-bind ,bindings ,padding
     ,@body))