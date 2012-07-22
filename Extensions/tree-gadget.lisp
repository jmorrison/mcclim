(in-package :clim-internals)

; The tree model-api

(defgeneric node-value (node))
(defgeneric node-children (node))

; The tree model-implementations

;; A list made from lists

(defmethod node-value ((node cons))
  (car node))

(defmethod node-children ((node cons))
  (cdr node))

;; A lazy directory tree

(defclass directory-node ()
  ((directory-pathname :initarg :pathname
                       :accessor directory-pathname)))

(defmethod initialize-instance :after ((node directory-node) &rest initargs)
  (let ((directory-pathname (getf initargs :pathname)))
    (when (not directory-pathname)
      (error "Provide a directory pathname"))))

(defmethod print-object ((node directory-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A" (directory-pathname node))))

(defmethod node-value ((node directory-node))
  (princ-to-string (directory-pathname node)))

(defmethod node-children ((node directory-node))
  (when (not (pathname-name (directory-pathname node)))
    (mapcar #'make-directory-node
            (directory (merge-pathnames (directory-pathname node) #p"*")))))

(defun make-directory-node (pathname)
  (make-instance 'directory-node :pathname pathname))


;; Convenient model api

(defmacro with-tree-node ((value children) node &body body)
  (let ((node-var (gensym "NODE-")))
    `(let ((,node-var ,node)) 
       (let ((,value (node-value ,node-var))
             (,children (node-children ,node-var)))
         ,@body))))

; The abstract tree-pane

(defclass tree-pane (value-gadget)
  ()
  (:documentation 
   "The instantiable class that implements an abstract tree pane")
  (:default-initargs :value nil))

; The concrete tree-pane

(define-abstract-pane-mapping 'tree-pane 'generic-tree-pane)

(defclass meta-tree-pane ()
  ((mode        :initarg :mode
                :initform :exclusive
                :reader tree-pane-mode
                :type (member :one-of :some-of :exclusive :nonexclusive))
   (model       :initarg :model
                :initform nil
                :accessor tree-pane-model
                :type sequence)
   (name-key    :initarg :name-key
                :initform #'princ-to-string
                :reader tree-pane-name-key
                :documentation "A function to be applied to items to gain a printable representation")
   (value-key   :initarg :value-key
                :initform #'identity
                :reader tree-pane-value-key
                :documentation "A function to be applied to items to gain its value
                                for the purpose of GADGET-VALUE.")
   (presentation-type-key :initarg :presentation-type-key
			  :initform (constantly nil)
			  :reader tree-pane-presentation-type-key
			  :documentation "A function to be applied to items to find the presentation types for their values, or NIL.")
   (indentation :initarg :indentation
                :initform 10
                :accessor indentation
                :documentation "Indentation size for displaying the tree")
   (test        :initarg :test
                :initform #'eql
                :reader tree-pane-test
                :documentation "A function to compare two items for equality.")))

(defun draw-right-arrow (sheet region &key (ink +blue+))
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* region)
    (draw-polygon* sheet (list x1 y1
                               x1 y2
                               x2 (+ y1 (/ (- y2 y1) 2))
                               x1 y1)
                   :filled t
                   :ink ink)))

(defun draw-down-arrow (sheet region &key (ink +blue+))
  (multiple-value-bind (x1 y1 x2 y2)
      (bounding-rectangle* region)
    (draw-polygon* sheet (list x1 y1
                               x2 y1
                               (+ x1 (/ (- x2 x1) 2)) y2
                               x1 y1)
                   :filled t
                   :ink ink)))

(defun flatten-tree (node opened-nodes &key (test #'eql))
  (with-tree-node (value children) node
    (cons value 
          (when (member value opened-nodes :test test)
            (loop for child in children appending
                 (flatten-tree child opened-nodes :test test))))))

(defclass generic-tree-pane (tree-pane meta-tree-pane
                                       standard-sheet-input-mixin ;; Hmm..
                                       value-changed-repaint-mixin
                                       mouse-wheel-scroll-mixin)
  ((highlight-ink :initform +royalblue4+
                  :initarg :highlight-ink
                  :reader tree-pane-highlight-ink)
   (opened-nodes :initform nil
                 :initarg :opened-nodes
                 :accessor opened-nodes)
   (last-action  :initform nil
                 :documentation "Last action performed on items in the pane, either
:select, :deselect, or NIL if none has been performed yet."))
  (:default-initargs :text-style (make-text-style :sans-serif :roman :normal)
    :background +white+ :foreground +black+))

(defmethod initialize-instance :after ((gadget meta-tree-pane) &rest rest)
  (declare (ignorable rest))
  ;; Initialize slot value if not specified
  #+NIL ;; XXX
  (when (slot-boundp gadget 'value)
    (setf (slot-value gadget 'value)
          (if (tree-pane-exclusive-p gadget)
              (funcall (tree-pane-value-key gadget) (first (tree-pane-items gadget)))
              (mapcar #'tree-pane-value-key (list (first (tree-pane-items gadget)))))))
    
  #+ignore
  (when (and (not (tree-pane-exclusive-p gadget))
             (not (listp (gadget-value gadget))))
    (error "A :nonexclusive tree-pane cannot be initialized with a value which is not a list."))
  #+ignore
  (when (not (tree-pane-exclusive-p gadget))
    (with-slots (value) gadget
      (setf value (copy-list value))))
  #+IGNORE
  (when (and (tree-pane-exclusive-p gadget)
             (> (length (gadget-value gadget)) 1))
    (error "An 'exclusive' tree-pane cannot be initialized with more than one item selected.")))

(defun generic-tree-pane-items (pane)
  (flatten-tree (tree-pane-model pane)
                (opened-nodes pane)
                :test (tree-pane-test pane)))

(defmethod value-changed-callback :before
    ((gadget generic-tree-pane) client gadget-id value)
  (declare (ignore client gadget-id))
  ;; Maybe act as if a presentation was clicked on, but only if the
  ;; list pane only allows single-selection.
  #+ignore(when (or (eq (tree-pane-mode gadget) :one-of)
            (eq (tree-pane-mode gadget) :exclusive))
    (let* ((i (position value (generic-tree-pane-item-values gadget)))
           (item (elt (tree-pane-items gadget) i))
           (ptype (funcall (tree-pane-presentation-type-key gadget) item)))
      (when ptype
        (throw-object-ptype value ptype)))))

(defun tree-pane-exclusive-p (pane)
  (or (eql (tree-pane-mode pane) :exclusive)
      (eql (tree-pane-mode pane) :one-of)))

(defmethod initialize-instance :after ((gadget generic-tree-pane) &rest rest)
  (declare (ignorable rest))
  ;; For a nonexclusive tree-pane, compute some reasonable default for the last
  ;; selected item to make shift-click do something useful.
  #+ignore
  (when (not (tree-pane-exclusive-p gadget))
    (with-slots (test last-action last-index) gadget
      (when (not (zerop (length (gadget-value gadget))))
        (setf last-action :select
              last-index
              (reduce #'max
                      (mapcar #'(lambda (item) (position item (generic-tree-pane-item-values gadget) :test test))
                              (gadget-value gadget))))))))

(defmethod generic-tree-pane-needed-width ((pane generic-tree-pane))
  )

(defmethod generic-tree-pane-items-length ((pane generic-tree-pane))
  )

(defmethod generic-tree-pane-item-height ((pane generic-tree-pane))
  (+ (text-style-ascent  (pane-text-style pane) pane)
     (text-style-descent (pane-text-style pane) pane)))

(defmethod compose-space ((pane generic-tree-pane) &key width height)
  (declare (ignore width height))

  (let ((node (tree-pane-model pane))
        (opened-nodes (opened-nodes pane))
        (indentation (indentation pane))
        (name-key (tree-pane-name-key pane))
        (test (tree-pane-test pane))
        (node-height (+ (text-style-ascent  (pane-text-style pane) pane)
                        (text-style-descent (pane-text-style pane) pane))))
    (labels ((node-width (node indentation)
               (+ indentation
                  (text-size (sheet-medium pane)
                             (funcall name-key node))
                  20))
             (tree-width (tree current-indentation)
               (with-tree-node (node children) tree
                 (max (node-width node current-indentation)
                      (or (and (member node opened-nodes :test test)
                               (apply #'max
                                      (cons 0
                                            (mapcar (lambda (node)
                                                      (node-width node (+ current-indentation indentation)))
                                                    children))))
                          0))))
             (tree-height (tree)
               (with-tree-node (node children) tree
                 (+ node-height
                   (apply #'+
                          (cons 0
                                (and (member node opened-nodes :test test)
                                     (mapcar #'tree-height children))))))))
      (let ((w (tree-width node 0))
            (h (tree-height node)))
        (make-space-requirement :width w     :height h
                                :min-width w :min-height h
                                :max-width +fill+ :max-height +fill+)))))

(defmethod allocate-space ((pane generic-tree-pane) w h)
  (resize-sheet pane w h))

(defmethod scroll-quantum ((pane generic-tree-pane))
  (generic-tree-pane-item-height pane))

(defun paint-generic-tree-pane-node (node pane region current-indentation indentation)
  (with-bounding-rectangle* (x0 y0 x1 y1) region
    (let ((item-height (generic-tree-pane-item-height pane))
          (highlight-ink (tree-pane-highlight-ink pane)))
      (with-tree-node (value children) node
        (multiple-value-bind (background foreground)
            (cond ((not (slot-boundp pane 'value))
                   (values (pane-background pane) (pane-foreground pane)))
                  ((if (tree-pane-exclusive-p pane)
                       (funcall (tree-pane-test pane)
                                value
                                (gadget-value pane))
                       (member value (gadget-value pane)
                               :test (tree-pane-test pane)))
                   (values highlight-ink (pane-background pane)))
                  (t (values (pane-background pane) (pane-foreground pane))))

          ;; Draw the node
        
          (draw-rectangle* pane x0 y0 x1 (+ y0 item-height) :filled t :ink background)

          ;; Draw corresponding arrow
          (let* ((arrow-width 8)
                 (arrow-height 8)
                 (arrow-region (make-bounding-rectangle (+ x0 current-indentation)
                                                        (+ y0 2)
                                                        (+ x0 current-indentation arrow-width)
                                                        (+ y0 2 arrow-height))))
            (when (node-children node)
              (if (member value (opened-nodes pane) :test (tree-pane-test pane))
                  (draw-down-arrow pane arrow-region)
                  (draw-right-arrow pane arrow-region)))

            ;; Draw the item text
            (draw-text* pane (funcall (tree-pane-name-key pane) value)
                        (+ x0 current-indentation arrow-width 5)
                        (+ y0 (text-style-ascent (pane-text-style pane) pane))
                      :ink foreground
                      :text-style (pane-text-style pane)))

          ;; Draw the node's children y the node is opened

          (let ((next-region  (make-bounding-rectangle x0
                                                       (+ y0 item-height)
                                                       x1
                                                       y1)))
            (when (member value (opened-nodes pane) :test (tree-pane-test pane))
              (loop
                 with children-indentation = (+ current-indentation indentation)
                 for child in children
                 do
                   (setf next-region
                         (paint-generic-tree-pane-node child
                                                       pane
                                                       next-region
                                                       children-indentation
                                                       indentation))))
            (return-from paint-generic-tree-pane-node next-region)))))))
          

(defmethod handle-repaint ((pane generic-tree-pane) region)
  (declare (ignore region))
  ;; Clear the pane first
  ;; Use this for fixing list-pane redisplays too?
  (with-bounding-rectangle* (x0 y0 x1 y1) (sheet-region pane)
    (draw-rectangle* pane x0 y0 x1 y1
                     :filled t
                     :ink (pane-background pane)))
  
  (paint-generic-tree-pane-node
   (tree-pane-model pane)
   pane
   (sheet-region pane)
   0
   (indentation pane)))

(defun generic-tree-pane-select-item (pane node)
  "Toggle selection  of a single item in the generic-tree-pane.
Returns :select or :deselect, depending on what action was performed."
  (if (tree-pane-exclusive-p pane)
      (progn
        (setf (gadget-value pane :invoke-callback t) node)
        
        ;; Toggle opened items list
        (setf (opened-nodes pane)
              (set-exclusive-or
               (opened-nodes pane)
               (list node) :test (tree-pane-test pane)))
        
        :select)
      ;; else
      (let ((member (member node (gadget-value pane) :test (tree-pane-test pane))))
        (setf (gadget-value pane :invoke-callback t)
              (cond ((tree-pane-exclusive-p pane)
                     (list node))
                    (member
                     (remove node (gadget-value pane)
                             :test (tree-pane-test pane)))
                    ((not member) (cons node (gadget-value pane)))))
        (if member :deselect :select))))

(defun generic-tree-pane-add-selected-items (pane items)
  "Add a set of items to the current selection"
  (when (not (tree-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-callback t)
          (remove-duplicates (append items
                                     (gadget-value pane))
                             :test (tree-pane-test pane)))))

(defun generic-tree-pane-deselect-items (pane items)
  "Remove a set of items from the current selection"
  (when (not (tree-pane-exclusive-p pane))
    (setf (gadget-value pane :invoke-calback t)
          (labels ((fun (items result)
                     (if (null items)
                         result
                         (fun (rest items)
                              (delete (first items) result
                                      :test (tree-pane-test pane))))))
            (fun items (gadget-value pane))))))

(defun generic-tree-pane-item-from-x-y (pane mx my)
  "Given a pointer event, determine what item in the pane it has fallen upon. 
Returns two values, the item itself, and the index within the item list."
  (declare (ignore mx))
  (with-bounding-rectangle* (sx0 sy0 sx1 sy1)  (sheet-region pane)
    (declare (ignorable sx0 sx1 sy1))
    (let ((items (generic-tree-pane-items pane)))
      (let* ((item-height (generic-tree-pane-item-height pane))
             (number-of-items (length items))
             (n (floor (- my sy0) item-height))
             (index (and (>= n 0)
                           (< n number-of-items)
                           n))
             (item-value (and index (elt items index))))
          (values item-value index)))))

(defun generic-tree-pane-handle-click (pane x y modifier)
  (multiple-value-bind (item-value index)
      (generic-tree-pane-item-from-x-y pane x y)
    (if (tree-pane-exclusive-p pane)
        ;; Exclusive mode
        (when index
          (setf (slot-value pane 'last-action)
                (generic-tree-pane-select-item pane item-value)))
        ;; Nonexclusive mode
        (when index
          (with-slots (last-index last-action items prefer-single-selection) pane
            (cond
              ;; Add single selection
              ((not (zerop (logand modifier +control-key+)))
               (setf last-action (generic-tree-pane-select-item pane item-value)))
              ;; Maybe extend selection
              ((not (zerop (logand modifier +shift-key+)))
               (if (and (numberp last-index)
                        (not (null last-action)))
                   ;; Extend last selection
                   (funcall (if (eql last-action :select)
                                #'generic-tree-pane-add-selected-items
                                #'generic-tree-pane-deselect-items)
                            pane
                            (let ((items (generic-tree-pane-items pane)))
                              (coerce (subseq items
                                              (min last-index index)
                                              (1+ (max last-index index))) 'list)))
                   (setf last-action (generic-tree-pane-select-item pane item-value))))
              ;; Toggle single item
              (t (if prefer-single-selection
                     (setf (gadget-value pane :invoke-callback t) (list item-value)
                           last-action :select)
                     (setf last-action (generic-tree-pane-select-item pane item-value)))))
            (setf last-index index)))))
  (change-space-requirements pane)
  )

(defun generic-tree-pane-handle-click-from-event (pane event)
  (multiple-value-bind (x y) (values (pointer-event-x event) (pointer-event-y event))
    (generic-tree-pane-handle-click pane x y (event-modifier-state event))))

;; (defclass ad-hoc-presentation (standard-presentation) ())

;; (defmethod output-record-hit-detection-rectangle*
;;     ((presentation ad-hoc-presentation))
;;   (values most-negative-fixnum most-negative-fixnum
;; 	  most-positive-fixnum most-positive-fixnum))

;; (defun generic-tree-pane-handle-right-click (pane event)
;;   (multiple-value-bind (x y)
;;       (values (pointer-event-x event) (pointer-event-y event))
;;     (multiple-value-bind (item-value index)
;; 	(generic-tree-pane-item-from-x-y pane x y)
;;       (let* ((item (elt (tree-pane-items pane) index)))
;; 	(meta-tree-pane-call-presentation-menu pane item)))))

;; (defun meta-tree-pane-call-presentation-menu (pane item)
;;   (let ((ptype (funcall (tree-pane-presentation-type-key pane) item)))
;;     (when ptype
;;       (let ((presentation
;; 	     (make-instance 'ad-hoc-presentation
;; 	       :object (funcall (tree-pane-value-key pane) item)
;; 	       :single-box t
;; 	       :type ptype)))
;; 	(call-presentation-menu
;; 	 presentation
;; 	 *input-context*
;; 	 *application-frame*
;; 	 pane
;; 	 42 42
;; 	 :for-menu t
;; 	 :label (format nil "Operation on ~A" ptype))))))

(defmethod handle-event ((pane generic-tree-pane) (event pointer-button-press-event))
  (case (pointer-event-button event)
    (#.+pointer-left-button+
      (generic-tree-pane-handle-click-from-event pane event)
      (setf (slot-value pane 'armed) nil))      
    (#.+pointer-right-button+
      (generic-tree-pane-handle-right-click pane event))
    (t
      (when (next-method-p) (call-next-method)))))

(defmethod handle-event ((pane generic-tree-pane) (event pointer-button-release-event))
  (if (eql (pointer-event-button event) +pointer-left-button+)
      (and (slot-value pane 'armed)
           (generic-tree-pane-handle-click-from-event pane event))
      (when (next-method-p) (call-next-method))))

;; (defgeneric (setf tree-pane-items)
;;     (newval pane &key invoke-callback)
;;   (:documentation
;;    "Set the current list of items for this list pane.
;; The current GADGET-VALUE will be adjusted by removing values not
;; specified by the new items.  VALUE-CHANGED-CALLBACK will be called
;; if INVOKE-CALLBACK is given."))

;; (defmethod (setf tree-pane-items)
;;     (newval (pane meta-tree-pane) &key invoke-callback)
;;   (declare (ignore invoke-callback))
;;   (setf (slot-value pane 'items) newval))

;; (defmethod (setf tree-pane-items)
;;     :after
;;     (newval (pane meta-tree-pane) &key invoke-callback)
;;   (when (slot-boundp pane 'value)
;;     (let ((new-values
;; 	   (coerce (climi::generic-tree-pane-item-values pane) 'list))
;; 	  (test (tree-pane-test pane)))
;;       (setf (gadget-value pane :invoke-callback invoke-callback)
;; 	    (if (tree-pane-exclusive-p pane)
;; 		(if (find (gadget-value pane) new-values :test test)
;; 		    (gadget-value pane)
;; 		    nil)
;; 		(intersection (gadget-value pane) new-values :test test))))))

;; (defmethod (setf tree-pane-items)
;;     (newval (pane generic-tree-pane) &key invoke-callback)
;;   (call-next-method)
;;   (with-slots (items items-length item-strings item-values) pane
;;     (setf items-length (length newval))
;;     (setf item-strings nil)
;;     (setf item-values nil)))

;; (defmethod (setf tree-pane-items) :after
;;     (newval (pane generic-tree-pane) &key invoke-callback)
;;   (change-space-requirements
;;    pane
;;    :height (space-requirement-height (compose-space pane)))
;;   (handle-repaint pane +everywhere+))
