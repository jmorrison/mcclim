;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: FILE-SELECTOR; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A File Selector for the Common Lisp Interface Manager
;;;   Comment: Written for the CLIM implementation McCLIM and uses some
;;;            at least one function that is not in the CLIM 2.0 specification.
;;;            Needs the CLIM Listener by Andy Hefner (included in McCLIM
;;;            and :com.gigamonkeys.pathnames by Peter Seibel
;;;     Usage: Compile and load the file, and call (file-selector:select-file).
;;;   Created: 2005-08-30 - 2005-10-13, Version 1.3
;;;    Author: Max-Gerd Retzlaff <m.retzlaff@gmx.net>, http://bl0rg.net/~mgr
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2005 by Max-Gerd Retzlaff

(in-package :cl-user)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (asdf:oos 'asdf:load-op :cl-fad))

(defpackage :file-selector
  (:use :clim :clim-lisp)
  (:import-from :clim-listener :com-show-directory :draw-icon :icon-of :pathname-printing-name)
  (:import-from :cl-fad :pathname-as-directory) ;; :pathname-as-file :list-directory)
  (:export :select-file))

(in-package :climi)

;;; This is rather ugly. But right now named panes are pushed onto the slot
;;; FRAME-NAMED-PANES of the frame (in an :around method to make-pane-1 in frames.lisp)
;;; and *never* removed. Not nice, as the File Selector makes temporary panes that are
;;; nevertheless named. Even more ugly as they get the same name on every call of the
;;; File Selector. Apart from the accumulation of unused panes, which pane will be
;;; returned if I call (find-pane-named *application-frame* 'files) and there are
;;; several panes with this name? Therefore the panes are right now manually removed
;;; by the following function.

(defun forget-named-pane (pane &optional (frame *application-frame*))
  (setf (frame-named-panes frame)
        (delete pane (frame-named-panes frame))))


(in-package :file-selector)

(defparameter *the-pathname* nil)
(defparameter *the-pathname-type* nil)

;;; custom command-table for the accepting-values dialog
(define-command-table file-selector-commands :inherit-from (climi::accepting-values)
                      );; :inherit-menu t)

;;; present an pathname in textual-dialog-view
(define-presentation-method present (object (type pathname) stream
				     (view textual-dialog-view)
				     &key acceptably for-context-type)
  (declare (ignore acceptably for-context-type))
  (let ((pathname object)
        (long-name t))
    (let ((icon (clim-listener::icon-of pathname)))
      (when icon 
        (clim-listener::draw-icon stream icon :extra-spacing 30)))
    (princ (clim-listener::pathname-printing-name pathname long-name) stream)))

;;;; completion does not work as desired, therefore this isn't used anymore
;;;
;;; (clim:define-presentation-type file-namestring ())
;;;
;;; (define-presentation-method present (object (type file-namestring) stream view &key)
;;;   (write-string object stream))
;;;
;;; (define-presentation-method accept ((type file-namestring) stream view &key)
;;;   (values               ;suppress values after the first
;;;    (multiple-value-bind (object success string)
;;;    (completing-from-suggestions (Stream :partial-completers '(#\- #\. #\Space #\_) :allow-any-input t)
;;;      (mapcar (lambda (args)
;;;                (apply #'suggest args))
;;;              (mapcar (lambda (pathname)
;;;                         (let ((pathname (cl-fad::pathname-as-file pathname)))
;;;                           (list 
;;;                            (file-namestring pathname)
;;;                            (file-namestring pathname))))
;;;                            (cl-fad::list-directory *the-pathname*))))
;;;      (declare (ignore success object))
;;;      string)))
;;;         
;;; (define-presentation-translator pathname-to-file-namestring-translator
;;;     (pathname file-namestring file-selector-commands
;;;               :gesture :describe)
;;;     (object)
;;;   (file-namestring object))


;;; :select gesture (left-click) selects a pathname
(define-presentation-to-command-translator select-pathname-command-translator
    (pathname climi::com-deselect-query file-selector-commands
              :gesture :select
              :documentation "Select this as pathname"
              :pointer-documentation "Select this as pathname")
    (object)
  (setf *the-pathname* object)
  nil)

;;; :describe gesture (middle-click) sets a pathname-type filter
(define-presentation-to-command-translator select-pathname-type-command-translator
    (pathname climi::com-deselect-query file-selector-commands
              :gesture :describe
              :documentation ((object stream)
                                      (let ((type (pathname-type object)))
                                        (if type
                                            (format stream "Show only files with type ~a" type)
                                            (format stream "Show files of any type"))))
              :pointer-documentation ((object stream)
                                      (let ((type (pathname-type object)))
                                        (if type
                                            (format stream "Show only files with type ~a" type)
                                            (format stream "Show files of any type")))))
    (object)
  (setf *the-pathname-type* (pathname-type object))
  nil)

;;;; garbage that I don't want to dispose yet
;;;
;;; (clim:define-presentation-type file-type () :inherit-from 'define)
;;;
;;; (string-presentation-translator pathname-to-file-type-translator
;;;     (pathname file-type file-selector-commands
;;;               :gesture :describe)
;;;     (object)
;;;   (pathname-type object))
;;;
;;;
;;; (define-presentation-method accept ((type pathname) stream (view textua-dialog-view) &key)
;;;   (values               ;suppress values after the first
;;;    ;;; provide completion over the names of the towns
;;;    (completing-from-suggestions (Stream :partial-completers '(#\- #\. #\Space))
;;;      (mapcar (lambda (args)
;;;                (apply #'suggest args))
;;;              (mapcar (lambda (pathname)
;;;                          (list (let ((pathname (cl-fad::pathname-as-file pathname)))
;;;                                  (file-namestring pathname))
;;;                                pathname))
;;;                            (cl-fad::list-directory *default-pathname-defaults*))))))


;;; MAIN FUNCTION
;;;
;;; You can append parameters for the call to CLIM-LISTENER::COM-SHOW-DIRECTORY, as in:
;;;     (file-selector:select-file :own-window t :pathname-type "lisp" :style 'list)

(defun select-file (&rest args-for-com-show-directory ;; Don't forget to update own-args-of-select-file!
                    &key (stream *query-io*)
                    (own-window nil)
                    (pathname *default-pathname-defaults*)
                    pathname-type
                    return-even-a-directory
                    &allow-other-keys)

  (let ((own-args-of-select-file '(:stream :own-window :pathname :pathname-type
                                   :return-even-a-directory))
        parent
        children
        typed-pathname
        (*pointer-documentation-output* *pointer-documentation-output*)
        old-wild-directory)

    (setf *the-pathname* pathname
          *the-pathname-type* pathname-type)

    (unwind-protect
         
         (accepting-values (stream :initially-select-query-identifier 'tag :own-window own-window
                                   :command-table 'file-selector-commands :label "File Selector"
                                   :resynchronize-every-pass t)
           ;; get the gloval values (that might be changed by the presentation to command translators)
           (setf pathname *the-pathname*
                 pathname-type *the-pathname-type*)

           (unless children
             ;; determine the parent sheet
             (setf parent (sheet-parent (if own-window
                                            stream
                                            (let ((scroller-pane (pane-scroller (climi::encapsulating-stream-stream stream))))
                                              ;; if parent is a scroller-pane return the scroller-pane
                                              (if scroller-pane 
                                                  ;; if parent is a border-pane return the border-pane
                                                  (or (climi::pane-border scroller-pane) 
                                                      scroller-pane)
                                                  stream)))))
             ;; remember children
             (setf children (sheet-children parent))

             ;; ... and disown them
             (dolist (child children)
               (sheet-disown-child parent child))

             ;; construct new pane hierarchy
             (let ((fm (frame-manager *application-frame*)))
               (with-look-and-feel-realization (fm *application-frame*)
                 (sheet-adopt-child parent
                                    (make-pane 'vrack-pane :name 'main-container
                                               :contents 
                                               (append (list `(+fill+ ,(make-clim-application-pane
                                                                        ;; :scroll-bars :both
                                                                        :NAME 'files
                                                                        :HEIGHT 150))
                                                             (make-pane 'vrack-pane
                                                                        :name 'children-container
                                                                        :contents children
                                                                        :height 200))
                                                       (when own-window ;; pointer-doc only in own-window
                                                         (list (make-pane 'pointer-documentation-pane
                                                                          :name 'pointer-doc))))))))
             ;; capture *pointer-documentation-output*
             (when own-window
               (setf *pointer-documentation-output*
                     (find-pane-named *application-frame* 'pointer-doc)))

             (change-space-requirements parent))


           ;; add pathname-type as type to the pathname if appropriate
           (setf typed-pathname (if (and (not (and (not (wild-pathname-p pathname))
                                                   (probe-file pathname)))
                                         (pathname-name pathname)
                                         (not (pathname-type pathname)))
                                           (make-pathname :type pathname-type
                                                          :defaults pathname)
                                    pathname))
           
           ;; show listing of the directory in the files pane
           (let* ((*standard-output* (find-pane-named *application-frame* 'files))
                  (directory (directory-namestring pathname))
                  (wild-directory (if pathname-type
                                      (make-pathname :name :wild :type pathname-type
                                                     :defaults (directory-namestring pathname))
                                      (pathname directory))))
             (unless (equal old-wild-directory wild-directory) ;; reprint necessary?
               (window-clear *standard-output*)
               (if (probe-file directory)
                   (let ((args-for-com-show-directory
                          (climi::remove-keywords args-for-com-show-directory own-args-of-select-file)))
                     (apply #'clim-listener::com-show-directory #+nil wild-directory directory
                            :list-all-direct-subdirectories t
                            args-for-com-show-directory))
                   (progn 
                     (format t "~&The directory ")
                     (present directory 'pathname)
                     (format t " does not exist.")))
               (change-space-requirements *standard-output*))
             (setf old-wild-directory wild-directory))
      
           ;; present the currently selected pathname
           (format stream "~%Currently selected: ")
           ;; (present typed-pathname 'pathname :stream stream) ;; doesn't work for an accepting-values stream :(
           (present (namestring typed-pathname) 'pathname :stream stream :view +textual-view+)
           (princ #\newline stream)
           (princ #\newline stream)
      
           ;; accept text-field for the pathname components
           (setf ;; pathname
                 ;; (accept '((pathname) :default-type pathname-type) :default pathname :stream stream
                 ;; :query-identifier 'tag)
            
                 pathname
                 (let ((file-namestring (parse-namestring
                                         (accept 'string #+nil 'file-namestring
                                                 :default (or (file-namestring pathname) "") ;; ACL returns NIL for ""
                                                 :prompt "Filename" :stream stream :query-identifier 'tag))))
                   (make-pathname :name (pathname-name file-namestring) ;; merge-pathnames wouldn't work for ""
                                  :type (pathname-type file-namestring)
                                  :defaults pathname))
                 
                 pathname
                 (let ((difference-pathname (cl-fad:pathname-as-directory
                                             (accept 'string :default (directory-namestring pathname)
                                                     :prompt "Directory" :stream stream))))
                   (if (equal #p"" difference-pathname)
                       #p""
                       (merge-pathnames difference-pathname
                                        pathname)))
              
                 pathname-type
                 (let ((type (accept 'string :default (or pathname-type "") :stream stream
                                     :prompt "Filetype")))
                   (if (string= type "")
                       nil
                       type)))
           
           (setf *the-pathname* pathname
                 *the-pathname-type* pathname-type)
           ) ;; of ACCEPTING-VALUES

      ;; reconstruct original pane hierarchy (only if embedded)
      (unless own-window
        ;; disown children
        (dolist (child (sheet-children (find-pane-named *application-frame* 'children-container)))
          (sheet-disown-child (find-pane-named *application-frame* 'children-container) child))
        
        ;; disown the main container of the File Selector
        (sheet-disown-child parent (find-pane-named *application-frame* 'main-container))
        
        ;; adopt remembered children to their former parent
        (let ((fm (frame-manager *application-frame*)))
          (with-look-and-feel-realization (fm *application-frame*)
            (dolist (child children)
              (sheet-adopt-child parent child))))

        (change-space-requirements parent))

      ;; forget the temporery but nevertheless named panes (argh)
      (climi::forget-named-pane (find-pane-named *application-frame* 'children-container))
      (climi::forget-named-pane (find-pane-named *application-frame* 'files))
      (climi::forget-named-pane (find-pane-named *application-frame* 'main-container))
      (when own-window
        (climi::forget-named-pane (find-pane-named *application-frame* 'pointer-doc)))
      ) ;; of UNWIND-PROTECT
    
    ;; return the selected file
    (if (or (pathname-name pathname)
            return-even-a-directory)
        typed-pathname
        (abort))))

;;; accept-method for presentation type pathname in textual-view
;;; try: (accept 'pathname :view +gadget-view+ :prompt nil)
(define-presentation-method accept ((type pathname) stream (view gadget-view) &key)
  (values (select-file :own-window t :return-even-a-directory t)
          'pathname))
