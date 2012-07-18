(in-package :clim-internals)

(defvar *clim-inspector-enabled* nil)

(defun enable-clim-inspector ()
  (setf *clim-inspector-enabled* t))

(defun disable-clim-inspector ()
  (setf *clim-inspector-enabled* nil))

(let ((output *standard-output*))
  (defmethod clim-internals::handle-event :after (sheet (event clim-internals::pointer-motion-event))
    (when *clim-inspector-enabled*
      (when (eql (event-modifier-state event) +shift-key+)
        (format output "On ~A~%" sheet)
        (let ((height (clim::bounding-rectangle-height (clim::sheet-region sheet)))
              (width (clim::bounding-rectangle-width (clim::sheet-region sheet))))
          (clim::draw-rectangle* sheet 0 0 width height :filled nil :ink clim::+red+))))))

(let ((output *standard-output*))
  (defmethod clim-internals::handle-event :after (sheet (event clim-internals::pointer-boundary-event))
    (when *clim-inspector-enabled*
      (when (eql (event-modifier-state event) +shift-key+)
        (format output "Crossing ~A~%" sheet)
        (clim-internals::repaint-sheet sheet (clim-internals::sheet-region sheet))))))

(defmethod handle-event :after (sheet (event pointer-button-press-event))
  (when *clim-inspector-enabled*
    (when (eql (event-modifier-state event) +shift-key+)
      (swank:inspect-in-emacs sheet))))