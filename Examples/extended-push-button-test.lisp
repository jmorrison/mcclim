(in-package :clim-demo)

(defparameter *icons-directory* (asdf::system-relative-pathname :mcclim #p"Extensions/icons/"))

(defun make-accept-button (&rest args)
  (with-look-and-feel-realization ((frame-manager *application-frame*) *application-frame*)
    (apply #'make-pane 'push-button
           (concatenate 'cons args
                        (list :icon (make-pattern-from-bitmap-file
                                     (merge-pathnames *icons-directory* #p"dialog-apply.xpm")
                                     :format :xpm :port nil))))))

(defun make-cancel-button (&rest args)
  (with-look-and-feel-realization ((frame-manager *application-frame*) *application-frame*)
    (apply #'make-pane 'push-button
           (concatenate 'cons args
                        (list :icon (make-pattern-from-bitmap-file
                                     (merge-pathnames *icons-directory* #p"dialog-cancel-3.xpm")
                                     :format :xpm :port nil))))))


(define-application-frame extended-push-button-test ()
  ()
  (:panes
   (accept-button (make-accept-button
                   :icon-position :left
                   :label "Accept"
                   :x-spacing 5
                   :y-spacing 5))
   (cancel-button (make-cancel-button
                   :label "Cancel"
                   :x-spacing 5
                   :y-spacing 5)))
  (:layouts
   (default
       (vertically ()
         accept-button
         cancel-button))))

(defun run-extended-push-button-test ()
  (run-frame-top-level
   (make-application-frame 'extended-push-button-test)))                   