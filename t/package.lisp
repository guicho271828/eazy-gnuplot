#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage :eazy-gnuplot.test
  (:use :cl
        :eazy-gnuplot
        :fiveam))
(in-package :eazy-gnuplot.test)



(def-suite :eazy-gnuplot)
(in-suite :eazy-gnuplot)

;; run test with (run! test-name) 
;;   test as you like ...

(test eazy-gnuplot
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "sample.pdf")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "x-label"       ; strings are "quoted"
                :ylabel "y-label"
                :output path            ; pathnames are "quoted"
                :terminal :pdf          ; keyword/symbols are not quoted
                                        ; (but not escaped)
                :key '(:bottom :right :font "Times New Roman, 6")
                ;; lists contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-plot "sin(x)")
      (plot (lambda ()
              (format t "~&0 0")
              (format t "~&1 1"))
            :using '(1 2)
            :title "1"
            :with '(:linespoint))
      (plot (lambda ()
              (format t "~&0 1")
              (format t "~&1 0"))
            :using '(1 2)
            :title "2"
            :with '(:lines)))
  (is-true (probe-file path))))

