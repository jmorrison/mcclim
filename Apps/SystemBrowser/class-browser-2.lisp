;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLIM-CLASS-BROWSER; -*-
;;; ---------------------------------------------------------------------------
;;;     Title: A basic class browser in CLIM
;;;   Created: 2003-05-07
;;;    Author: Gilbert Baumann <unk6@rz.uni-karlsruhe.de>
;;; ---------------------------------------------------------------------------
;;;  (c) copyright 2003 by Gilbert Baumann

;; Note: Don't try with a non PCL-based CLOS.
;; After loading try calling (clim-class-browser::class-browser)

(defpackage :clim-class-browser
    (:use :clim :clim-lisp))

(in-package :clim-class-browser)

(define-application-frame class-browser ()
  ((classes :initarg :classes
            :initform (list (find-class 'climi::basic-pane))
            :accessor classes))
  (:pointer-documentation t)
  (:panes
   (app :application
    :width 3000 :height 2000
    :scroll-bars nil
    :incremental-redisplay t
    :display-function 'class-browser-display)
   (io :interactor
    :height 50))
  (:layouts
   (default
       (vertically (:width 800 :min-width 100 :max-width +fill+)
         (:fill
          (horizontally ()
            (scrolling (:scroll-bars t)
              app)
            ))
         io))))

(defvar *expanded* nil)

(define-presentation-type node ())

(define-class-browser-command toggle-node ((node 'node :gesture :select))
  (if (member node *expanded*)
      (setf *expanded* (remove node *expanded*))
      (push node *expanded*)))

(defun class-browser-display (app pane)
  app pane
  (let ((*standard-output* pane))
    (format-graph-from-roots
     (classes *application-frame*)
     #'(lambda (node *standard-output*)
         (let ((*print-case* :downcase))
           (surrounding-output-with-border
            (*standard-output* :shape :drop-shadow)
            (with-text-style (t (make-text-style :sans-serif nil nil))
              (with-output-as-presentation (t node 'node)
                (with-text-style (t (make-text-style :sans-serif :bold :large))
                  (princ (class-name node))))
              (terpri)
              (with-drawing-options (*standard-output* :ink +red4+)
                (princ (clean-docu-string(class-documentation node))))
              (terpri)
              (formatting-table ()
                (dolist (sd (sb-mop::class-direct-slots node))
                  (formatting-row ()
                    (formatting-cell (t :align-y :top)
                      (princ (sb-mop::slot-definition-name sd))
                      (princ " "))
                    (formatting-cell (t :align-y :top)
                      (with-drawing-options (*standard-output* :ink +red4+)
                        (princ (clean-docu-string (slot-documentation sd))))))))
              (terpri)) )))
     #'(lambda (node)
         (if (member node *expanded*)
             (sb-mop:class-direct-subclasses node)
             ;(sb-mop:class-direct-superclasses node)
             nil))
     :cutoff-depth nil
     :graph-type :tree
     :merge-duplicates t
     :arc-drawer #'climi::arrow-arc-drawer
     :arc-drawing-options (list :ink +gray66+ :line-thickness 1)
     :generation-separation 30)
    (terpri)))

;; some tweaks:

(defun clean-docu-string (string)
  (with-output-to-string (bag)
    (let ((last-was-nl nil))
      (loop for c across string do
            (cond ((eql c #\newline)
                   (princ c bag)
                   (setf last-was-nl t))
                  ((member c '(#\space #\tab))
                   (if last-was-nl
                       nil
                       (princ c bag)))
                  (t
                   (setf last-was-nl nil)
                   (princ c bag)))))))

(defun class-documentation (class)
  (or (documentation class t) ""))

(defun slot-documentation (slot-def)
  (or (documentation slot-def t) ""))  

(defun class-browser (&optional (classes nil classes-p))
  (run-frame-top-level
   (if classes-p
       (make-application-frame 'class-browser :classes classes)
       (make-application-frame 'class-browser))))