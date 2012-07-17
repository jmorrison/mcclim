(in-package :mcclide)

(define-application-frame  mcclide ()
  ((current-project :accessor current-project
                    :initform nil)
   (current-source-file :accessor current-source-file
                        :initform nil))
  (:panes
   (projects-navigator (make-pane 'projects-navigator-pane))
   (source-files :text-editor-pane)
   (repl (make-pane 'repl-pane))
   (output :application-pane)
   (interactor :interactor-pane))
  (:command-table (mcclide-commands
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
                          ("Repl" :menu repl-commands)
                          ("Tools" :menu tools-commands)
                          ("Help" :menu help-commands))))
  (:menu-bar t)
  (:layouts
   (default
       (vertically (:width 500 :height 500)
         (4/5
          (horizontally ()
            (1/5 projects-navigator)
            (4/5 source-files)))
         (1/5
          repl)))))

(defclass projects-navigator-pane (application-pane)
  ())

(defclass source-files-pane (application-pane)
  ())

(defclass repl-pane (application-pane)
  ())

(define-command-table project-commands :inherit-from nil)

(define-command-table repl-commands :inherit-from nil)

(define-command (com-repl-evaluate :name "Evaluate"
                                   :command-table repl-commands
                                   :menu t)
    ())

(define-command (com-repl-inspect :name "Inspect"
                                  :command-table repl-commands
                                  :menu t)
    ())

(define-command (com-repl-describe :name "Describe"
                                   :command-table repl-commands
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
           ("System browser" :command com-run-system-browser)))

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

(defun mcclide ()
  (run-frame-top-level
   (make-application-frame 'mcclide)))