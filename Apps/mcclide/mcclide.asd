(defpackage :mcclide.system
  (:use :asdf :cl))

(in-package :mcclide.system)

(defsystem mcclide
  :components
  ((:file "package")
   (:file "mcclide"))
  :serial t
  :depends-on (:mcclim))