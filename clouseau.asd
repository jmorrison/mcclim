;;; -*- lisp -*-

(defpackage :clouseau.system
  (:use :cl :asdf))

(in-package :clouseau.system)

(defsystem :clouseau
    :depends-on (:mcclim)
    :serial t
    :components
    ((:module "Apps/Inspector"
              :pathname #.(make-pathname :directory '(:relative "Apps" "Inspector"))
              :components
	      ((:file "package")
	       (:file "disassembly" :depends-on ("package"))
	       (:file "inspector" :depends-on ("disassembly"))))))

(defmethod perform :after ((op load-op) (c (eql (find-system :clouseau))))
  (pushnew :clouseau *features*))
