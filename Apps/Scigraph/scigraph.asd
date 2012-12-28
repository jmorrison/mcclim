;;; -*- lisp -*-

(defpackage :scigraph.system
  (:use :cl :asdf))

(in-package :scigraph.system)

;;; This won't load in SBCL, either. I have really crappy code to
;;; extract dependency information from :serial t ASDF systems, but
;;; this comment is too narrow to contain it.
;;;
;;; 2011-05-17: In order to get this to load under SBCL and CCL, I
;;; entered :depends-on information as it appeared in the old
;;; mk-defsystem directives that came with the system.  I also used
;;; ASDF's :module facility to mirror the code structure.  This seemed
;;; to work.  jm@symbolic-simulation.com
;;;

(defsystem :scigraph
    :depends-on (:mcclim)
    ;; The DWIM part of SCIGRAPH
    :serial t
    :components
    #+MCCLIM
    ((:module "dwim"
	      :components
	      ((:file "package")
	       (:file "feature-case" :depends-on ("package"))
	       (:file "macros" :depends-on ("feature-case"))
	       (:file "tv" :depends-on ("macros"))
	       (:file "draw" :depends-on ("tv"))
	       (:file "present" :depends-on ("draw"))
	       (:file "extensions" :depends-on ("present"))
	       (:file "wholine" :depends-on ("extensions"))
	       (:file "export" :depends-on ("wholine"))))
     (:module scigraph
	      :depends-on (dwim)
	      :components
	      ((:file "package")
	       (:file "copy" :depends-on ("package"))
	       (:file "dump" :depends-on ("copy"))
	       (:file "duplicate" :depends-on ("dump"))
	       (:file "random" :depends-on ("duplicate"))
	       (:file "menu-tools" :depends-on ("random"))
	       (:file "basic-classes" :depends-on ("menu-tools"))
	       (:file "draw" :depends-on ("basic-classes"))
	       (:file "mouse" :depends-on ("draw"))
	       (:file "color" :depends-on ("mouse"))
	       (:file "basic-graph" :depends-on ("color"))
	       (:file "graph-mixins" :depends-on ("basic-graph"))
	       (:file "axis" :depends-on ("graph-mixins"))
	       (:file "moving-object" :depends-on ("axis"))
	       (:file "symbol" :depends-on ("moving-object"))
	       (:file "graph-data" :depends-on ("symbol"))
	       (:file "legend" :depends-on ("graph-data"))
	       (:file "graph-classes" :depends-on ("legend"))
	       (:file "present" :depends-on ("graph-classes"))
	       (:file "annotations" :depends-on ("present"))
	       (:file "annotated-graph" :depends-on ("annotations"))
	       (:file "contour" :depends-on ("annotated-graph"))
	       (:file "equation" :depends-on ("contour"))
	       (:file "popup-accept" :depends-on ("equation"))
	       (:file "popup-accept-methods" :depends-on ("popup-accept"))
	       (:file "duplicate-methods" :depends-on ("popup-accept-methods"))
	       (:file "frame" :depends-on ("duplicate-methods"))
	       (:file "export" :depends-on ("frame"))
	       (:file "demo-frame" :depends-on ("export")))))
    #-MCCLIM
    (
     (:file "dwim/package")
     (:file "dwim/feature-case")
     (:file "dwim/macros")
     (:file "dwim/tv")
     (:file "dwim/draw")
     (:file "dwim/present")
     (:file "dwim/extensions")
     (:file "dwim/wholine")
     (:file "dwim/export")
     ;; The Scigraph part
     (:file "scigraph/package")
     (:file "scigraph/copy")
     (:file "scigraph/dump")
     (:file "scigraph/duplicate")
     (:file "scigraph/random")
     (:file "scigraph/menu-tools")
     (:file "scigraph/basic-classes")
     (:file "scigraph/draw")
     (:file "scigraph/mouse")
     (:file "scigraph/color")
     (:file "scigraph/basic-graph")
     (:file "scigraph/graph-mixins")
     (:file "scigraph/axis")
     (:file "scigraph/moving-object")
     (:file "scigraph/symbol")
     (:file "scigraph/graph-data")
     (:file "scigraph/legend")
     (:file "scigraph/graph-classes")
     (:file "scigraph/present")
     (:file "scigraph/annotations")
     (:file "scigraph/annotated-graph")
     (:file "scigraph/contour")
     (:file "scigraph/equation")
     (:file "scigraph/popup-accept")
     (:file "scigraph/popup-accept-methods")
     (:file "scigraph/duplicate-methods")
     (:file "scigraph/frame")
     (:file "scigraph/export")
     (:file "scigraph/demo-frame")))