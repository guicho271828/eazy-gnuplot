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

(def-fixture test-plot (out)
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test out)))
    (print path)
    (terpri)
    (finish-output)
    (when (probe-file path)
      (delete-file path))
    (&body)
    (is-true (probe-file path))))

(def-fixture test-not-plot (out)
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test out)))
    (print path)
    (terpri)
    (finish-output)
    (when (probe-file path)
      (delete-file path))
    (&body)
    (is-false (probe-file path))))

(def-fixture just-plot (out)
  (let ((path (asdf:system-relative-pathname
               :eazy-gnuplot.test out)))
    (print path)
    (terpri)
    (finish-output)
    (ignore-errors
      (when (probe-file path)
        (delete-file path))
      (&body))))

(defun rel (pathname)
  (asdf:system-relative-pathname :eazy-gnuplot pathname))

(test eazy-gnuplot
  (with-fixture test-plot ("sample.pdf")
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
            :with '(:lines)))))

(test top-left-reverse-left
  (with-fixture test-plot ("sample-top-left.pdf")
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
            :with '(:lines)))))

(test no-stdout
  (with-fixture test-plot ("nostdout.pdf")
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
            :with '(:lines)))))

(test row
  (with-fixture test-plot ("row.pdf")
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
            :with '(:lines)))))

(test splot
  (with-fixture test-plot ("splot.pdf")
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
                       :with '(:lines))))))))))

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

;; change of mind: I don't think its a good idea
#+nil
(test optional-arg
  (with-fixture test-plot ("optional.pdf")
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
            :with '(:lines)))))

(test png
  (with-fixture test-plot ("sample.png")
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
            :with '(:lines)))))

(test issue-8
  (with-fixture test-plot ("issue-8.png")
    (with-plots (s :debug t)
      (gp-setup :output path
                :terminal :png
                :key '(:bottom :right :font "Times New Roman, 6")
                :pointsize "0.4px")
      (gp :set :label '(1 \"aaaaa\" at graph "0.8,0.8" center))
      (gp :set :label '(3 \"ccccccccc\" at graph "0.2,0.2" center))
      (gp :set :label '(4 \"ddddd\" at graph " 0.2,0.2" center))
      
      (func-plot "sin(x)" :title "super sin curve!")
      ;; once something has been plotted, everything written to the stream
      ;; is moved to the end of the script
      (plot (lambda ()
              (format s "~&0 0")
              (format s "~&1 1"))
            :using '(1 2)
            :title "1"
            :with '(:linespoint))
      (plot (lambda ()
              (format s "~&0 1")
              (format s "~&1 0"))
            :using '(1 2)
            :title "2"
            :with '(:lines))
      )))

(test issue-12-multi-using
  (with-fixture test-plot ("issue-10-multi-using.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path
                             :terminal :png
                             :title "Test Issue 10"
                             :key '( invert reverse Left outside)
                             :key '( autotitle columnheader)
                             :style '( data histogram)
                             :style '( histogram rowstacked)
                             :style '( fill solid border -1))
      (plot (lambda ()
              (loop for r in '(( 8.01   1   5   1)
                               ( 8.02   3   5   1)
                               ( 8.03   4   4   1)
                               ( 8.04   3   4   1)
                               ( 8.05   1   2   1))
                    do (format t "~&~{~^~A ~}" r)))
            :using '(2 "xtic(1)")
            :title "Col0"
            :using 2
            :title "Col1"
            :using 3
            :title "Col2"
            :using 4
            :title "Col3"))))

(test issue-12-no-using
  (with-fixture test-plot ("issue-10-no-using.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path
                             :terminal :png
                             :style '(fill pattern 5))
      (plot (lambda () (loop for i from 0 upto 50
                             do (format t "~&~A ~A"  i (sin i))))
            :lt '(rgb "blue")
            :with '(:filledcurves :above :y1 = 0.07)))))

(test issue-14-guess-terminal-type
  (with-fixture test-plot ("issue-14-guess-terminal-type.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      (func-plot "sin(x)")))
  (with-fixture test-plot ("issue-14-guess-terminal-type.pdf")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      (func-plot "sin(x)")))
  (with-fixture test-plot ("issue-14-guess-terminal-type.svg")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      (func-plot "sin(x)")))
  (with-fixture just-plot ("issue-14-guess-terminal-type") ; no pathname-type, or unspecified
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (signals error
        (eazy-gnuplot:gp-setup :output path))))
  (with-fixture just-plot ("issue-14-guess-terminal-type.*") ; wild
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (signals error
        (eazy-gnuplot:gp-setup :output path))))
  (with-fixture just-plot ("issue-14-guess-terminal-type.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (signals error
        ;; missing output
        (eazy-gnuplot:gp-setup))))
  (with-fixture just-plot ("issue-14-guess-terminal-type.nosuchterminal")
    (signals uiop:subprocess-error
      (eazy-gnuplot:with-plots (*standard-output* :debug t)
        ;; missing output
        (eazy-gnuplot:gp-setup :output path)))))

(test multiplot
  (with-fixture test-plot ("multiplot.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path
                             :title "Multiplot"
                             :multiplot (list "layout 2,2 columnsfirst title \"Multiplot\"")
                             )
      (func-plot "besj0(x)")
      (func-plot "besj1(x)")
      (func-plot "besy0(x)")
      (func-plot "besy1(x)")
      (gp :unset 'multiplot)
      )))

(test multiplot-w/data-producing-functions
  (with-fixture test-plot ("multiplot-with-data-producing-functions.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
       (eazy-gnuplot:gp-setup :output path
                              :terminal :png
                              :title "Test Issue 10"
                              :key '( invert reverse Left outside)
                              :key '( autotitle columnheader)
                              :style '( data histogram)
                              :style '( histogram rowstacked)
                              :style '( fill solid border -1)
                              :multiplot (list "layout 2,2 columnsfirst title \"Multiplot\""))
      (plot (lambda ()
              (loop for r in '(( 8.1   1   5   1)
                               ( 8.2   3   5   1)
                               ( 8.3   4   4   1)
                               ( 8.4   3   4   1)
                               ( 8.5   1   2   1))
                    do (format t "~&~{~^~A ~}" r)))
            :using '(2 "xtic(1)")
            :title "Col0"
            :using 2
            :title "Col1"
            :using 3
            :title "Col2"
            :using 4
            :title "Col3")
        (plot (lambda ()
              (loop for r in '(( 8.1   1   5   1)
                               ( 8.2   3   5   1)
                               ( 8.3   4   4   1)
                               ( 8.4   3   4   1)
                               ( 8.5   1   2   1))
                    do (format t "~&~{~^~A ~}" r)))
            :using '(2 "xtic(1)")
            :title "Col0"
            :using 2
            :title "Col1"
            :using 3
            :title "Col2"
            :using 4
            :title "Col3")
      (func-plot "besy0(x)")
      (func-plot "besy1(x)")
        (gp :unset 'multiplot))))

(test maintain-order-of-sets-unsets-issue-21
  (with-fixture test-plot ("maintain-order-of-sets-unsets-issue-21.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path
                             :multiplot (list ""))
      (gp :set :title "O     O")
      (gp :set :polar '())
      (gp :set :size (list "1,.5"))
      (gp :set :border 0)
      (gp :unset :key)
      (gp :unset :tics)
      (func-plot "[pi:2*pi] -1" :lw 5 )
      (gp :set :title "###################")
      (gp :set :origin (list "0,.5"))
      (gp :set :size (list ".5, .5"))
      (func-plot "-2*pi" :lw (list "2, .2") :with 'filledcurves)
      (gp :set  :origin (list ".5, .5"))
      (gp :set :title "###################")
      (func-plot "1" :lw (list "2, .2") :with 'filledcurves)
      (gp :unset 'multiplot))))

(test maintain-order-of-gp-setup
  (with-fixture test-plot ("maintain-order-of-gp-setup.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      (gp :plot :x**2))))

(test plot-with-datafile
  (with-fixture test-plot ("plot-data.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (let ((dat (rel "test.dat")))
        (eazy-gnuplot:gp-setup :output path)
        (datafile-plot dat)        ; backward compatibility
        (datafile-plot (namestring dat))        ; backward compatibility
        (plot dat)))))


(test plot-with-function
  (with-fixture test-plot ("plot-function.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      (func-plot "sin(x)")        ; backward compatibility
      (plot "sin(x)"))))

(test fit
  (with-fixture test-plot ("fit.png")
    (eazy-gnuplot:with-plots (*standard-output* :debug t)
      (eazy-gnuplot:gp-setup :output path)
      ;; f(x) = a + b*x + c*x**2
      ;; fit f(x) 'measured.dat' using 1:2 via a,b,c
      (let ((dat (rel "test.dat")))
        (fit "a + b*x + c*x**2" dat :using '(1 2) :via '(a b c))
        (datafile-plot dat)
        (func-plot "a + b*x + c*x**2")))))
