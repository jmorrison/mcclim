(in-package :clim-demo)

(define-application-frame tree-test ()
  ((tree :initarg :tree
         :accessor tree
         :initform (simple-tree)))
  (:panes
   (result-list
    (make-pane 'climi::tree-pane
               :value 'clim:region-intersection
               :model (tree *application-frame*)
                                        ;:presentation-type-key (constantly 'list-test-symbol)
               :test #'equalp
               :item-padding (climi::make-padding 5 5 5 5)))
   (interactor :interactor :height 200))
  (:layouts
   (defaults
       (labelling (:label "Tree example"
                          :text-style (make-text-style :sans-serif :roman :normal))
         (vertically ()
           (scrolling (:height 200)
             result-list
             )
           #+ignore(horizontally ()
                     substring
                     (make-pane 'push-button
                                :label "Update"
                                :activate-callback 'update-list-test))
           #+ignore interactor)))))

(define-presentation-type list-test-symbol ())

(define-list-test-command com-describe-symbol
    ((sym 'list-test-symbol :gesture :select))
  ;; Let's print only three lines, we don't have space for more.
  (with-input-from-string (s (with-output-to-string (s) (describe sym s)))
    (dotimes (x 3)
      (write-line (read-line s nil "") *standard-input*))))

(defun update-list-test (pane)
  (declare (ignore pane))
  (setf (tree-pane-items (find-pane-named *application-frame* 'result-list))
	(apropos-list (gadget-value
		       (find-pane-named *application-frame* 'substring))
		      :clim t)))

(defun subclasses-tree (class)
  (cons class
        (mapcar #'subclasses-tree (sb-mop::class-direct-subclasses class))))

(defun simple-tree ()
  (cons "Level1A"
        (list (cons "Level2A"
                    (list
                     (list "Level3A")
                     (list "Level3B")))
              (cons "Level2B"
                    (list
                     (list "Level3C")
                     (list "Level3D"))))))

(defun tree-gadget-basic-example ()
  (clim::run-frame-top-level
   (clim::make-application-frame 'tree-test)))

(defun tree-gadget-classes-example ()
  (clim::run-frame-top-level
   (clim::make-application-frame 'tree-test :tree (subclasses-tree (find-class 'standard-object)))))

(defun tree-gadget-directory-example ()
  (clim::run-frame-top-level
   (clim::make-application-frame 'tree-test :tree (climi::make-directory-node #p"/home/"))))

(define-application-frame tree-test-directory-icon ()
  ((tree :initarg :tree
         :accessor tree
         :initform (simple-tree)))
  (:panes
     (result-list
      (let ((icon (make-pattern-from-bitmap-file
                                          (asdf:system-relative-pathname
                                           :mcclim
                                           "Apps/Listener/icons/folder.xpm")
                                          :format :xpm :port nil)))
        (make-pane 'climi::tree-pane
                   :value 'clim:region-intersection
                   :model (tree *application-frame*)
                                        ;:presentation-type-key (constantly 'list-test-symbol)
                   :test #'equalp
                   :item-padding (climi::make-padding 5 5 5 5)
                   :item-icon-function
                   (lambda (node opened-p)
                     (declare (ignore opened-p))
                     (when (climi::node-children node)
                       icon))
                   :item-icon-size (list (pattern-width icon)
                                         (pattern-height icon))
                   :item-icon-padding (climi::make-padding 5 5 5 0)
                 
                   )))
     (interactor :interactor :height 200))
  (:layouts
     (defaults
         (labelling (:label "Tree example"
                            :text-style (make-text-style :sans-serif :roman :normal))
           (vertically ()
	     (scrolling (:height 200)
	       result-list
               )
	     #+ignore(horizontally ()
	       substring
	       (make-pane 'push-button
			  :label "Update"
			  :activate-callback 'update-list-test))
	     #+ignore interactor
             )))))


(defun tree-gadget-directory-icons-example ()
  (clim::run-frame-top-level
   (clim::make-application-frame 'tree-test-directory-icon
                                 :tree (climi::make-directory-node #p"/home/"))))