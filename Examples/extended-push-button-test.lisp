(in-package :clim-demo)

(defparameter *icons-directory* (asdf::system-relative-pathname :mcclim #p"Extensions/icons/"))

(define-application-frame extended-push-button-test ()
  ()
  (:panes
   (accept-button :push-button
                  :icon (make-pattern-from-bitmap-file
                         (merge-pathnames *icons-directory* #p"dialog-apply.xpm")
                         :format :xpm :port nil)
                  :icon-position :left
                  :label "Accept"
                  :x-spacing 5
                  :y-spacing 5)
   (cancel-button :push-button
                  :icon (make-pattern-from-bitmap-file
                         (merge-pathnames *icons-directory* #p"dialog-cancel-3.xpm")
                         :format :xpm :port nil)
                  :icon-position :right
                  :label "Cancel"
                  :x-spacing 5
                  :y-spacing 5))
  (:layouts
   (default
       (vertically ()
         accept-button
         cancel-button))))

(defun run-extended-push-button-test ()
  (run-frame-top-level
   (make-application-frame 'extended-push-button-test)))                   