(in-package :mcclide)

(define-application-frame  mcclide ()
  ((current-project :accessor current-project
                    :initform nil)
   (current-source-file :accessor current-source-file
                        :initform nil))
  (:panes
   (projects-navigator (make-pane 'projects-navigator-pane))
   (source-files :text-editor-pane)
   (source-file-2 :text-editor-pane)
   (lisp-interaction (make-pane 'lisp-interaction-pane))
   (output :application-pane)
   (interactor :interactor-pane))
  (:command-table (mcclide
                   :inherit-from (file-commands
                                  edit-commands
                                  help-commands
                                  tools-commands
                                  project-commands
                                  asdf-commands
                                  quicklisp-commands)
                   :menu (("File" :menu file-commands)
                          ("Project" :menu project-commands)
                          ("Edit" :menu edit-commands)
                          ("Lisp" :menu lisp-interaction-commands)
                          ("Tools" :menu tools-commands)
                          ("Help" :menu help-commands))))
  (:menu-bar t)
  (:layouts
   (default
       (vertically (:width 500 :height 500)
         (4/6
          (horizontally ()
            (1/5 projects-navigator)
            (make-pane 'clim-extensions:box-adjuster-gadget)
            (4/5
             (clim-tab-layout:with-tab-layout ('clim-tab-layout:tab-page :name 'source-files-layout :height 500)
               ("ex1.lisp" source-files)
               ("ex2.lisp" source-file-2)))))
         (make-pane 'clim-extensions:box-adjuster-gadget)
         (1/6
          lisp-interaction)
         (make-pane 'clim-extensions:box-adjuster-gadget)
         (1/6 interactor)))))

(defvar *projects* nil)

(defclass project ()
  ((name :initarg :name
         :accessor name
         :initform (error "Provide the name"))
   (pathname :initarg :pathname
             :accessor project-pathname
             :initform (error "Provide the pathname"))))

(defmethod print-object ((project project) stream)
  (print-unreadable-object (project stream :type t :identity t)
    (format stream "~A" (name project))))

(push (make-instance 'project
                     :name "mcclim"
                     :pathname #p"/home/marian/src/lisp/mcclim/")
      *projects*)

(push 
 (make-instance 'project
                :name "cl-config"
                :pathname #p"/home/marian/src/lisp/cl-config/")
 *projects*)

(defmethod climi::node-value ((project project))
  (name project))

(defmethod climi::node-children ((project project))
  (list
   (cons "Source"
         (list (make-instance 'climi::directory-node :pathname (project-pathname project))))
   (cons "Settings" nil)
   (cons "Documentation" nil)
   (cons "Tests" nil)))

(defclass projects-navigator-pane (climi::generic-tree-pane)
  ()
  (:default-initargs
   :item-padding (climi::make-padding 5 5 5 5)))

(defmethod initialize-instance :after ((projects-navigator projects-navigator-pane) &rest initargs)
  (declare (ignore initargs))
  (setf (climi::tree-pane-model projects-navigator)
        (cons "Projects" *projects*)))
              
(defclass source-files-pane (application-pane)
  ())

(defclass lisp-interaction-pane (application-pane)
  ())

(define-command-table project-commands :inherit-from nil)

(define-command-table lisp-interaction-commands :inherit-from nil)

(define-command (com-lisp-evaluate :name "Evaluate"
                                   :command-table lisp-interaction-commands
                                   :menu t)
    ())

(define-command (com-lisp-inspect :name "Inspect"
                                  :command-table lisp-interaction-commands
                                  :menu t)
    ())

(define-command (com-lisp-describe :name "Describe"
                                   :command-table lisp-interaction-commands
                                   :menu t)
    ())

(define-command (com-lisp-apropos :name "Apropos"
                                   :command-table lisp-interaction-commands
                                   :menu t)
    ())

(define-command (com-lisp-whocalls :name "Who calls"
                                   :command-table lisp-interaction-commands
                                   :menu t)
    ())

(define-command (com-lisp-jump-to-definition :name "Jump to definition"
                                   :command-table lisp-interaction-commands
                                   :menu t)
    ())

(define-command-table help-commands :inherit-from nil)

(define-command (com-about-mcclide :name "About"
                                   :command-table help-commands
                                   :menu t)
    ()
  (run-frame-top-level
   (make-application-frame 'about-dialog)))

(define-command (com-room :name "Room"
                          :command-table help-commands
                          :menu t)
    ()
  (run-frame-top-level
   (make-application-frame 'room-dialog)))

(define-command-table tools-commands
    :inherit-from nil
    :menu (("ASDF" :menu asdf-commands)
           ("Quicklisp" :menu quicklisp-commands)
           ("Tests runner" :command com-run-test-runner)
           ("System browser" :command com-run-system-browser)
           ("Version control" :menu version-control-commands)))

(define-command (com-run-test-runner :command-table tools-commands)
    ()
    (clim-fiveam::test-runner))

(define-command (com-run-system-browser :command-table tools-commands)
    ()
  (clim-system-browser::system-browser))

(define-command-table file-commands :inherit-from nil)

(define-command (com-new-file :name "New file"
                              :command-table file-commands
                              :menu t)
    ())

(define-command (com-open-file :name "Open file"
                               :command-table file-commands
                               :menu t)
    ()
  (file-selector:select-file :own-window t :pathname-type "lisp" :style 'list))

(define-command-table edit-commands :inherit-from nil)

(define-command (com-edit-settings :name "Settings"
                                   :command-table edit-commands
                                   :menu t)
    ())

(define-command (com-edit-preferences :name "Preferences"
                                      :command-table edit-commands
                                      :menu t)
    ())

(define-command (com-new-project :name "New project"
                                 :command-table project-commands
                                 :menu t)
  ())

(define-command (com-open-project :name "Open project"
                                  :command-table project-commands
                                  :menu t)
    ())

(define-command-table version-control-commands :inherit-from nil)

(define-command (com-repository-status :name "Repository status"
                                       :command-table version-control-commands
                                       :menu t)
    ())

(define-application-frame room-dialog ()
  ()
  (:panes
   (info-pane :application-pane
              :width 500 :height 500
              :display-function (lambda (frame pane)
                                  (declare (ignore frame))
                                  (let ((*standard-output* pane))
                                    (room))))
   (accept-button :push-button
                  :label "Accept"
                  :activate-callback (lambda (button)
                                    (declare (ignore button))
                                    (frame-exit *application-frame*))))
  (:layouts
   (default
       (vertically ()
         info-pane
         accept-button))))

(defparameter +about-message+ "A wanabe Common Lisp IDE implemented on McCLIM")

(define-application-frame about-dialog ()
  ()
  (:panes
   (about-pane :application-pane
               :width 500 :height 500
               :display-function (lambda (frame pane)
                                   (declare (ignore frame))
                                   (format pane +about-message+)))
   (accept-button :push-button
                  :label "Accept"
                  :activate-callback (lambda (button)
                                    (declare (ignore button))
                                    (frame-exit *application-frame*))))
  (:layouts
   (default
       (vertically ()
         about-pane
         accept-button))))
  
(defun mcclide ()
  (run-frame-top-level
   (make-application-frame 'mcclide)))