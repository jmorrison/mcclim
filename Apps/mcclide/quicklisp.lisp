(in-package :mcclide)

;;;; CLIM defintions for interacting with Quicklisp

(define-command-table quicklisp-commands :inherit-from nil)

(define-presentation-type quicklisp-system ())

(define-presentation-method presentation-typep (object (type quicklisp-system))
  (typep object 'ql-dist:system))

(define-presentation-method present (object (type quicklisp-system) stream
                                            (view textual-view)
                                            &key acceptably)
  (if acceptably
      (princ (ql-dist:name object) stream)
      ))
  
(define-presentation-method accept ((type quicklisp-system) stream
                                    (view textual-view) &key)
  (multiple-value-bind (object success)
      (completing-from-suggestions (stream)
        (dolist (system (ql:system-list))
          (suggest (ql-dist:name system) system)))
    (if success
        object
        (simple-parse-error "Unknown system"))))

(define-command (com-ql-list-systems :name "List Systems"
                                     :command-table quicklisp-commands
                                     :menu t)
    ()
  (loop for system in (ql:system-list)
       do
       (progn
         (format t "~A" (ql-dist:name system))
         (terpri)))
  ;; (format-items 
  ;;  (ql:system-list)
  ;;  :printer (lambda (item stream)
  ;;             (present item 'quicklisp-system
  ;;                      :stream stream
  ;;                      ;; :view (make-instance 'asdf-attribute-view
  ;;                      ;;                      :note-unloaded t
  ;;                      ;;                      :ignore '(asdf:compile-op asdf:load-op))

  ;;                      ))
  ;;  :presentation-type 'quicklisp-system)
  )

;; (define-command (com-show-available-systems :name "Show System Files"
;;                                             :command-table asdf-commands
;;                                             :menu t)
;;     ()
;;   (format-items (asdf-registry-system-files)                
;;                 :presentation-type 'asdf-system-definition))

;; (define-command (com-operate-on-system :name "Operate On System"
;;                                        :command-table asdf-commands
;;                                        :menu t)
;;     ((system '(type-or-string asdf-system) :prompt "system")
;;      (operation '(member asdf::compile-op asdf::load-op)
;;                 :default 'asdf::load-op
;;                 :prompt "operation"))
;;   (asdf:oos operation system))

(defun ql-system-apropos (term)
  (loop for system  in (ql::provided-systems t)
     when (or (ql::search term (ql::name system))
              (ql::search term (ql::name (ql::release system))))
       collect system))

(define-command (com-ql-system-apropos :name "System apropos"
                                       :command-table quicklisp-commands
                                       :menu t)
    ((term 'string :prompt "term"))
  (loop for system in (ql-system-apropos term)
     do (progn
          (format t "~A" (ql::name system))
          (terpri))))

(define-command (com-ql-load-system :name "Load system"
                                    :command-table quicklisp-commands
                                    :menu t)
    ((system '(type-or-string quicklisp-system) :prompt "system"))
  (ql:quickload (if (stringp system)
                    system
                    (ql::name system))))