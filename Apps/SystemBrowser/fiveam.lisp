(defpackage clim-fiveam
  (:use :clim :clim-lisp :clim-internals :clim-extensions :fiveam))

(in-package :clim-fiveam)

(def-suite example-suite :description "This is an example suite")

(in-suite example-suite)

(test sum-test ()
      (is (equalp (+ 2 2) 4)))

(test successful-test ()
      (is t))

(test failing-test ()
      (is nil))

(defun fiveam-test-suites ()
  (loop for test-or-suite being the hash-value of fiveam::*test*
     when (equalp (type-of test-or-suite) 'fiveam::test-suite)
     collect test-or-suite))

(define-application-frame fiveam-test-runner ()
  ((tests-to-run :initarg :tests-to-run
                 :accessor tests-to-run
                 :initform nil))
  (:panes
   (test-suites-pane :list-pane
                     :items (fiveam-test-suites)
                     :mode :nonexclusive
                     :name-key #'fiveam::name
                     :value-changed-callback #'selected-test-suites-changed)
   (tests-pane :list-pane
               :items nil
               :mode :nonexclusive
               :value-changed-callback #'selected-tests-changed
               :name-key #'fiveam::name)
   (progress-bar (make-pane 'progress-bar-pane
                            :done 0
                            :foreground-color +green+))
   (run-tests-button :push-button
                     :label "Run"
                     :activate-callback #'run-tests)
   (output-pane :application-pane
                :width 500))
  (:layouts
   (default
       (vertically ()
         (1/10 progress-bar)
         (7/10
          (horizontally ()
            (1/2
             (scrolling (:scroll-bar :vertical)
               test-suites-pane))
            (1/2
             (scrolling (:scroll-bar :vertical)
               tests-pane))))
         (1/10 run-tests-button)
         (1/10
          (scrolling (:scroll-bar :vertical)
            output-pane))))))

(defclass progress-bar-pane (application-pane)
  ((total :initarg :total
         :initform 100
         :accessor total)
   (done :initarg :done
         :initform 0
         :accessor done)
   (background-color :initarg :background-color
                     :initform +white+
                     :accessor background-color)
   (foreground-color :initarg :foreground-color
                     :initform +red+
                     :accessor foreground-color)))

(defmethod handle-repaint ((pane progress-bar-pane) region)
  (multiple-value-bind (x1 y1 x2 y2) (bounding-rectangle* (sheet-region pane))
    (draw-rectangle* pane x1 y1 x2 y2
                     :ink (background-color pane))
    ;; x2 - x1 = total
    ;; pbw     = done
    ;; pbw = pbx2 - x1
    ;; pbx2 = ((total/done) * (x2 - x1) / total) + x1
    (with-slots (total done) pane
      (when (plusp done)
        (let ((progress-bar-width (/ (* done (- x2 x1)) total)))
          (draw-rectangle* pane x1 y1
                           (+ progress-bar-width x1)
                           y2
                           :ink (foreground-color pane)))))))

(defun run-tests (button)
  (declare (ignore button))
  (let ((progress-bar (find-pane-named *application-frame* 'progress-bar))
        (output-pane (find-pane-named *application-frame* 'output-pane))
        (tests-to-run (tests-to-run *application-frame*)))
    ;; Initialize progress bar
    (setf (done progress-bar) 0
          (foreground-color progress-bar) +green+
          (total progress-bar) (length tests-to-run))
    ;; Clear output pane
    (redisplay-frame-pane *application-frame* output-pane :force-p t)
    
    (loop for test in tests-to-run
         for i from 1 to (length tests-to-run)
         do
         (progn
           (format output-pane "Running test: ~A ..." test)
           (if (fiveam::%run test)
               (progn
                 (format output-pane "Passed"))
               ;else
               (progn
                 (format output-pane "Failed")
                 (setf (foreground-color progress-bar) +red+)))
           (incf (done progress-bar))
           (terpri output-pane)
           (sleep 0.5)
           ))))


(defmethod (setf done) :after (new-value (pane progress-bar-pane))
  (handle-repaint pane (or (pane-viewport-region pane)
                             (sheet-region pane))))

(defmethod (setf foreground-color) :after (new-value (pane progress-bar-pane))
  (handle-repaint pane (or (pane-viewport-region pane)
                           (sheet-region pane))))

(defun selected-tests-changed (pane value)
  (setf (tests-to-run *application-frame*) value))

(defun selected-test-suites-changed (pane value)
  (let ((tests
         (loop for suite in value
            appending (loop for test being the hash-value of (fiveam::tests suite)
                           collect (fiveam::get-test test))))
        (tests-pane (find-pane-named *application-frame*
                           'tests-pane)))
    (setf (list-pane-items tests-pane)
          tests)
    ;; Update tests to run
    (let ((new-tests-to-run
           (intersection (tests-to-run *application-frame*)
                         value)))
    (setf (tests-to-run *application-frame*) new-tests-to-run)
    (setf (gadget-value tests-pane) new-tests-to-run))))

(defun test-runner ()
  (run-frame-top-level
   (make-application-frame 'fiveam-test-runner)))