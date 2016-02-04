#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage eazy-gnuplot
  (:use :cl :iterate
        :trivia
        :alexandria)
  (:export :with-plots
           :func-plot
           :func-splot
           :datafile-plot
           :datafile-splot
           :plot
           :splot
           :gp-setup
           :gp
           :gp-quote
           :*gnuplot-home*
           :row))
(in-package :eazy-gnuplot)

;; gnuplot interface

(defvar *gnuplot-home* "gnuplot"
  "gnuplot command available in the system.")
(defvar *user-stream* nil
  "a stream which is bounded by with-plots, and which the user can write
  some additional gnuplot commands into.")
(defvar *plot-command-stream* nil
  "a stream dedicated for printing the plot command, and its non-data
  arguments, like plot style, linewidth, linetype.  not meant to be exposed
  to the users.")
(defvar *data-stream* nil
  "a stream dedicated for printing the inline data fed to the plot
  command. not meant to be exposed to the users.")
(defvar *plot-type*)
(defvar *plot-type-multiplot*)

(defun gp-quote (value)
  "Map a value to the corresponding gnuplot string"
  (match value
    ((type string) (format nil "\"~a\"" value))
    ((type pathname) (gp-quote (namestring value)))
    (nil "")
    ((symbol name)
     (if (some (conjoin #'both-case-p #'lower-case-p) name)
         name                           ; escaped characters e.g. |Left|
         (string-downcase name)))
    ((type list)
     (reduce (lambda (str val)
               (format nil "~a ~a"
                       str (gp-quote val)))
             value))
    ;; numbers etc
    (_ value)))

(defun map-plist (args fn)
  (when args
    (destructuring-bind (key value . rest) args
      (funcall fn key value)
      (map-plist rest fn))))

(defun gp-setup (&rest args &key terminal output multiplot &allow-other-keys)
  "Special command for setting up gnuplot. This is almost the same as GP command,
however it serves some special purposes such as terminal detection from the output,
multiplot etc."
  (let ((*print-case* :downcase))
    (unless output
      (error "missing ouptut!"))
    (when (null terminal)
      (ematch (pathname-type (pathname output))
        ((and type (or :unspecific :wild nil "*"))
         (error "gp-setup is missing :terminal, and ~
                 it failed to guess the terminal type from the output pathname ~a,
                 based on its pathname-type ~a"
                output type))
        ((and type (type string)) 
         (setf terminal (make-keyword type)))))
    (setf *plot-type-multiplot* multiplot)
    (format *user-stream* "~&set ~a ~a" :terminal (gp-quote terminal))
    (format *user-stream* "~&set ~a ~a" :output (gp-quote output))
    (remf args :terminal)
    (remf args :output)
    (map-plist args
               (lambda (key val)
                 (format *user-stream* "~&set ~a ~a"
                         key (gp-quote val))))))

(defun gp (left-side &rest right-side)
  "Single statement ,render gnuplot `left-side` as string infront of gp-quoted contents of ars
   For example:
     Command:
       (gp :set :param)
       (gp :set :style :line 1 :lc :rgb '(\"'#999999'\") :lt 1 '(\"#border\"))
     Generates:
       set param
       set style line 1 lc rgb \"'#999999'\" lt 1 \"#border\"

- Arguments:
  - left-side : first word in gnuplot statement
  - right-side : arguments remaining in the argument list rendered as parameters to left-side command
- Return:
  NIL
"
  (format *user-stream* "~&~A ~A~%" left-side (gp-quote right-side)))

(defmacro with-plots ((stream &key debug (external-format :default))
                      &body body)
  (check-type stream symbol)
  `(call-with-plots ,external-format ,debug (lambda (,stream) ,@body)))


(define-condition new-plot () ())

(defun call-with-plots (external-format debug body)
    (let ((*plot-type* nil)
          (*print-case* :downcase)
          (before-plot-stream (make-string-output-stream))
          (after-plot-stream (make-string-output-stream))
          (*data-stream* (make-string-output-stream))
          (*plot-command-stream* (make-string-output-stream))
          *plot-type-multiplot*)
      (let ((*user-stream* before-plot-stream))
        (handler-bind ((new-plot
                        (lambda (c)
                          (declare (ignore c))
                          (setf *user-stream* after-plot-stream)
                          ;; ensure there is a newline
                          (terpri after-plot-stream))))
          (funcall body (make-synonym-stream '*user-stream*))
          ;; this is required when gnuplot handles png -- otherwise the file buffer is not flushed
          (format after-plot-stream "~&set output"))
        (with-input-from-string (in ((lambda (str)
                                       (if debug
                                           (print str *error-output*)
                                           str))
                                     (concatenate 'string
                                                  (get-output-stream-string before-plot-stream)
                                                  (get-output-stream-string *plot-command-stream*)
                                                  (get-output-stream-string *data-stream*)
                                                  (get-output-stream-string after-plot-stream))))
          (uiop:run-program *gnuplot-home*
                            :input in
                            :external-format external-format)))))


(defun %plot (data-producing-fn &rest args
              &key (type :plot) &allow-other-keys
              &aux (filename
                    (etypecase data-producing-fn
                      (string
                       data-producing-fn)
                      (pathname
                       (format nil "'~a'" data-producing-fn))
                      (function "'-'"))))
  ;; print the filename
  (cond
    ((or (null *plot-type*) *plot-type-multiplot*)
     (format *plot-command-stream* "~%~a ~a" type filename)
     (setf *plot-type* type))
    ((and (eq type *plot-type*) (not *plot-type-multiplot*))
     (format *plot-command-stream* ", ~a" filename)
     )
    (t
     (error "Using incompatible plot types ~a and ~a in a same figure! (given: ~a expected: ~a)"
            type *plot-type* type *plot-type*)))
  (remf args :type)
  ;; process arguments
  (let ((first-using t))
    (map-plist
     args
     (lambda (&rest args)
       (match args
         ((list :using (and val (type list)))
          (format *plot-command-stream* "~:[, ''~;~] using ~{~a~^:~}" first-using val)
          (setf first-using nil))
         ((list :using (and val (type atom)))
          (format *plot-command-stream* "~:[, ''~;~] using ~a" first-using val)
          (setf first-using nil))
         ((list key val)
          (format *plot-command-stream* " ~a ~a" key (gp-quote val)))))))

  (signal 'new-plot)
  (when (functionp data-producing-fn)
    ;; ensure the function is called once
    (let ((data (with-output-to-string (*user-stream*)
                  (funcall data-producing-fn)))
          (correct-stream (if *plot-type-multiplot* *plot-command-stream* *data-stream*)))
      (flet ((plt ()
               (terpri correct-stream)
               (write-sequence data correct-stream)
               (format correct-stream "~&end~%")))
        (let ((n (count :using args)))
          (if (> n 0)
              (loop repeat n do (plt))
              (plt)))))))

(defun plot (data-producing-fn &rest args &key using &allow-other-keys)
  "DATA-PRODUCING-FN is either a function producing data, a string
representing gnuplot functions, or a pathanme for input data."
  (declare (ignorable using))
  (apply #'%plot data-producing-fn args))
(defun splot (data-producing-fn &rest args &key using &allow-other-keys)
  "DATA-PRODUCING-FN is either a function producing data, a string
representing gnuplot functions, or a pathanme for input data."
  (declare (ignorable using))
  (apply #'plot data-producing-fn :type :splot args))
(defun func-plot (expression &rest args &key using &allow-other-keys)
  (declare (ignorable using))
  (check-type expression string)
  (warn "FUNC-PLOT is deprecated. Use the PLOT function with a string.")
  (apply #'%plot expression args))
(defun func-splot (expression &rest args &key using &allow-other-keys)
  (declare (ignorable using))
  (warn "FUNC-SPLOT is deprecated. Use the PLOT function with a string.")
  (apply #'func-plot expression :type :splot args))
(defun datafile-plot (pathspec &rest args &key using &allow-other-keys)
  (declare (ignorable using))
  (check-type pathspec (or string pathname))
  (warn "DATAFILE-PLOT is deprecated. Use the PLOT function with a pathname.")
  (apply #'%plot (pathname pathspec) args))
(defun datafile-splot (pathspec &rest args &key using &allow-other-keys)
  (declare (ignorable using))
  (warn "DATAFILE-SPLOT is deprecated. Use the SPLOT function with a pathname.")
  (apply #'datafile-plot (pathname pathspec) :type :splot args))

(defun row (&rest args)
  "Write a row"
  (format *user-stream* "~&~{~a~^ ~}" args))
