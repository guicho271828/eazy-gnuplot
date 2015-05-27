#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#


(in-package :cl-user)
(defpackage eazy-gnuplot.test-asd
  (:use :cl :asdf))
(in-package :eazy-gnuplot.test-asd)


(defsystem eazy-gnuplot.test
  :author "guicho"
  :license ""
  :description "Test library of eazy-gnuplot"
  :depends-on (:eazy-gnuplot
               :fiveam)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (load-op :after (op c) (PROGN (EVAL (READ-FROM-STRING "(fiveam:run! :eazy-gnuplot)")) (CLEAR-SYSTEM C))))
