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
                ;; list contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
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

(test top-left-reverse-left
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "sample-top-left.pdf")))
    (when (probe-file path)
      (delete-file path))
    (with-plots (*standard-output* :debug t)
      (gp-setup :output path
                :terminal :pdf
                :key '(:top :left :reverse :|Left| :font "Times New Roman, 6")
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
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

(test no-stdout
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "nostdout.pdf")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots (s :debug t)
      (gp-setup :xlabel "x-label"       ; strings are "quoted"
                :ylabel "y-label"
                :output path            ; pathnames are "quoted"
                :terminal :pdf          ; keyword/symbols are not quoted
                                        ; (but not escaped)
                :key '(:bottom :right :font "Times New Roman, 6")
                ;; list contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
      (plot (lambda ()
              (format s "~&0 0")
              (format s "~&1 1"))
            :using '(1 2)
            :title "1"
            :with '(:linespoint))
      (plot (lambda ()
              (row 0 1)
              (format s "~&1 0"))
            :using '(1 2)
            :title "2"
            :with '(:lines)))
  (is-true (probe-file path))))

(test row
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "row.pdf")))
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
                ;; list contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
      (plot (lambda ()
              (row 0 0)
              (row 1 1))
            :using '(1 2)
            :title "1"
            :with '(:linespoint))
      (plot (lambda ()
              (row 0 1)
              (row 1 0))
            :using '(1 2)
            :title "2"
            :with '(:lines)))
  (is-true (probe-file path))))

(test splot
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "splot.pdf")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots (*standard-output* :debug t)
      (gp-setup :xlabel "x-label"       ; strings are "quoted"
                :ylabel "y-label"
                :zlabel "z-label"
                :output path            ; pathnames are "quoted"
                :terminal :pdf          ; keyword/symbols are not quoted
                                        ; (but not escaped)
                :key '(:bottom :right :font "Times New Roman, 6")
                ;; list contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-splot "sin(x)+cos(y)" :title "super sin/cos surface!")
      (flet ((neg (x) (if (zerop x) 1 0)))
        (dotimes (x 2)
          (dotimes (y 2)
            (dotimes (z 2)
              (let ((x x) (y y) (z z))
                (splot (lambda ()
                         (row x y z)
                         (apply #'row (mapcar #'neg (list x y z))))
                       :title (format nil "~a~a~a" x y z)
                       :with '(:lines))))))))
  (is-true (probe-file path))))

(test incompatible-plot
  (signals error
    (with-plots (*standard-output* :debug t)
      (gp-setup)
      (func-plot "sin(x)")
      (func-splot "sin(x)+cos(y)")))
  (signals error
    (with-plots (*standard-output* :debug t)
      (gp-setup)
      (plot (lambda () (row 1 2)))
      (splot (lambda () (row 1 2 3))))))

(test optional-arg
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "optional.pdf")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots ()
      (gp-setup :xlabel "x-label"       ; strings are "quoted"
                :ylabel "y-label"
                :output path            ; pathnames are "quoted"
                :terminal :pdf          ; keyword/symbols are not quoted
                                        ; (but not escaped)
                :key '(:bottom :right :font "Times New Roman, 6")
                ;; list contents are recursively quoted
                ;; then joined by a space
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
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

(test png
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "sample.png")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots (*standard-output* :debug t)
      (gp-setup :output path
                :terminal :png
                :key '(:bottom :right :font "Times New Roman, 6")
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
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

(test issue-8
  ;; allow putting a printer to arbitrary position
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test "sample.png")))
    (print path)
    (when (probe-file path)
      (delete-file path))
    (with-plots (*standard-output* :debug t)
      (gp-setup :output path
                :terminal :png
                :key '(:bottom :right :font "Times New Roman, 6")
                :pointsize "0.4px")
      (func-plot "sin(x)" :title "super sin curve!")
      (format t "~&set label \"1\" at graph 0.2,0.2 center")
      (plot (lambda ()
              (format t "~&0 0")
              (format t "~&1 1"))
            :using '(1 2)
            :title "1"
            :with '(:linespoint))
      (format t "~&set label \"2\" at graph 0.5,0.5 center")
      (plot (lambda ()
              (format t "~&0 1")
              (format t "~&1 0"))
            :using '(1 2)
            :title "2"
            :with '(:lines))
      (format t "~&set label \"3\" at graph 0.8,0.8 center"))
  (is-true (probe-file path))))
