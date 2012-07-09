(defpackage clim-system-browser
  (:use :clim-lisp :clim :sb-mop :climacs :clim-extensions))

(in-package :clim-system-browser)

;; Some introspective code stolen from manifest

(defparameter *categories* '(:function :macro :generic-function :slot-accessor :variable :class :condition :constant))

(defun names (package what)
  (sort
   (loop for sym being the present-symbols of package
      when (is sym what) collect sym
      when (is `(setf ,sym) what) collect `(setf ,sym))
   #'name<))

(defun name< (n1 n2)
  (cond
    ((and (symbolp n1) (symbolp n2))
     (string< n1 n2))
    ((and (symbolp n1) (listp n2))
     (cond
       ((string< n1 (second n2)) t)
       ((string< (second n2) n1) nil)
       (t t)))
    ((and (listp n1) (symbolp n2))
     (cond
       ((string< (second n1) n2) t)
       ((string< n2 (second n1)) nil)
       (t nil)))
    ((and (listp n1) (listp n2))
     (string< (second n1) (second n2)))))

(defgeneric is (symbol what))
(defgeneric docs-for (symbol what))
(defgeneric pluralization (what))

(defmethod pluralization (what) (format nil "~as" what))

(defmacro define-category (name (symbol what) &body body)
  (let ((is-test (cdr (assoc :is body)))
        (get-docs (cdr (assoc :docs body)))
        (pluralization (cdr (assoc :pluralization body))))
    `(progn
       (defmethod is (,symbol (,what (eql ',name))) ,@is-test)
       (defmethod docs-for (,symbol (,what (eql ',name))) ,@get-docs)
       ,@(when pluralization
               `((defmethod pluralization ((,what (eql ',name)))
                   ,@pluralization))))))

(defun function-p (name)
  (ignore-errors (fdefinition name)))

(defun macro-p (name)
  (and (symbolp name) (macro-function name)))

(defun generic-function-p (name)
  (and (function-p name)
       (typep (fdefinition name) 'generic-function)))

(defun variable-p (name)
  (ignore-errors (boundp name)))

(defun automatic-p (docstring)
  (member docstring '("automatically generated reader method" "automatically generated writer method") :test #'string-equal))

(defun gf-docs (name)
  (let ((simple (documentation (fdefinition name) t))
        (from-setf (and (consp name) (documentation (fdefinition (second name)) t))))

    (or
     (and simple (not (automatic-p simple)) (format nil "The ~a" simple))
     (and from-setf (not (automatic-p from-setf)) (format nil "Set the ~a" from-setf))
     (first (remove-if #'automatic-p (remove nil (mapcar
                         (lambda (m) (documentation m t))
                         (generic-function-methods (fdefinition name)))))))))


(define-category :function (symbol what)
  (:is (and (function-p symbol)
            (not (or (is symbol :macro)
                     (is symbol :generic-function)
                     (is symbol :slot-accessor)))))
  (:docs (documentation symbol 'function)))

(define-category :macro (symbol what)
  (:is (macro-p symbol))
  (:docs (documentation symbol 'function)))

(define-category :generic-function (symbol what)
  (:is (and (generic-function-p symbol)
            (not (is symbol :slot-accessor))))
  (:docs (documentation symbol 'function)))

(define-category :class (symbol what)
  (:is (and (find-class symbol nil) (not (is symbol :condition))))
  (:docs (documentation (find-class symbol) t))
  (:pluralization (format nil "~aes" what)))

(define-category :condition (symbol what)
  (:is (and (find-class symbol nil) (subtypep (find-class symbol nil) 'condition)))
  (:docs (documentation (find-class symbol) t)))

(define-category :variable (symbol what)
  (:is (and (variable-p symbol) (not (is symbol :constant))))
  (:docs   (documentation symbol 'variable)))

(define-category :constant (symbol what)
  (:is (and (variable-p symbol) (constantp symbol)))
  (:docs (documentation symbol 'variable)))

(define-category :slot-accessor (symbol what)
  (:is (and (generic-function-p symbol)
            (some (lambda (m)
                    (or (eql (class-of m) (find-class 'standard-reader-method))
                        (eql (class-of m) (find-class 'standard-writer-method))))
                  (generic-function-methods (fdefinition symbol)))))
  (:docs (gf-docs symbol)))

;; The CLIM system browser

(let ((packages (list-all-packages)))
  (define-application-frame clim-system-browser ()
    ((current-package :initform ;(first packages)
                      (find-package :sb-mop)
                      :accessor current-package
                      :initarg :current-package)
     (current-category :initform (first *categories*)
                       :accessor current-category
                       :initarg :current-category)
     (current-element :initform (first
                                 (list-elements (first packages)
                                                (first *categories*)))
                      :accessor current-element
                      :initarg :current-element))
  (:panes
   (packages-pane
    (make-pane 'list-pane
                 :value (current-package *application-frame*)
                 :items packages
                 :mode :exclusive
                 :value-changed-callback #'package-pane-changed
                 :name-key #'package-name
                 :visible-items 10))
   (categories-pane
      (make-pane 'list-pane
                 :value (first *categories*)
                 :items *categories*
                 :mode :exclusive
                 :value-changed-callback #'categories-pane-changed
                 :name-key #'symbol-name
                 :visible-items 10))
   (elements-pane
    (let ((elements (list-elements (current-package *application-frame*)
                                   (current-category *application-frame*))))
      (make-pane 'list-pane
                   :value (first elements)
                   :items elements
                   :mode :exclusive
                   :value-changed-callback #'elements-pane-changed
                   ;:name-key #'symbol-name
                   :visible-items 10)))
   (definition-pane
       ;; (make-pane 'climacs::climacs-pane
       ;;                         :height 500
       ;;                         :active t
       ;;                         :view
       ;;                         (make-instance 'climacs::textual-drei-syntax-view
       ;;                                        :buffer 
       ;;                                        (with-open-file (stream #p"/home/marian/src/lisp/quicklisp.lisp" :direction :input)
       ;;                                          (esa-buffer::make-buffer-from-stream stream))))
       (make-pane 'application-pane :display-function #'display-definition-pane)

       ))
  (:layouts
   (default
       (vertically ()
         (horizontally ()
           (labelling (:label "Packages")
             (scrolling (:height 200 :scroll-bar :vertical)
               packages-pane))
           (labelling (:label "Categories")
             (scrolling (:height 200 :scroll-bar :vertical)
               categories-pane))
           (labelling (:label "Elements" :scroll-bar :vertical)
             (scrolling (:height 200)
               elements-pane)))
         (3/4
          (scrolling (:height 500 :scroll-bar :vertical)
            definition-pane)))))))

(defun list-elements (package category)
  (names package category))
  
(defun package-pane-changed (pane value)
  (setf (current-package *application-frame*) value)
  (let ((categories-pane (find-pane-named *application-frame* 'categories-pane))
        (first-category (first *categories*)))
      (setf (gadget-value categories-pane) first-category)))

(defun categories-pane-changed (pane value)
  (setf (current-category *application-frame*) value)
  (let ((category-elements (list-elements (current-package *application-frame*)
                                          (current-category *application-frame*)))
        (elements-pane (find-pane-named *application-frame* 'elements-pane)))
    (setf (gadget-value elements-pane) (first category-elements))
    (setf (list-pane-items elements-pane) category-elements)
    ))

(defun elements-pane-changed (pane value)
  (setf (current-element *application-frame*) value)
  (redisplay-frame-pane *application-frame* (get-frame-pane *application-frame* 'definition-pane) :force-p t))

(defun display-definition-pane (frame pane)
  (when (current-element frame)
    (when (equalp (current-category frame) :class)
      (let ((class-symbol (intern (symbol-name (current-element frame)) (current-package frame))))
        (let ((location (swank::find-source-location (find-class class-symbol))))
          (let ((snippet (cadr (nth 3 location))))
            (format pane "~A" snippet))))))
  #+nil(with-open-file (stream #p"/home/marian/src/lisp/quicklisp.lisp" :direction :input)
    (cl-fad:copy-stream stream pane))
  )

(defmethod esa-buffer::frame-make-buffer-from-stream ((application-frame clim-system-browser) stream)
  (let* ((buffer (esa-buffer::make-new-buffer)))
;    (input-from-stream stream buffer 0)
;    (clear-undo-history buffer)
    buffer))

(defmethod esa-buffer::frame-make-new-buffer ((application-frame clim-system-browser)
                                  &key (name "*scratch*"))
  (make-instance 'climacs::climacs-buffer :name name))


(run-frame-top-level
 (make-application-frame 'clim-system-browser))