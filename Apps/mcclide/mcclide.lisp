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

(defclass projects-navigator-pane (application-pane)
  ())

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
    ())

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
    ())

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

(defun mcclide ()
  (run-frame-top-level
   (make-application-frame 'mcclide)))