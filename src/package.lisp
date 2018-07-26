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
           :row
           :fit))
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
  (ematch value
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
    ((number) value)))

(defvar *keyword-separator-alist* nil
  "an alist from a keyword to the corresponding separator string.")

(setf (getf *keyword-separator-alist* :using) ":")
(iter (for keyword in '(:via :at :size :errors :offset :dgrid3d))
      (setf (getf *keyword-separator-alist* keyword) ","))
(iter (for dimension in '("X" "Y" "Z" "R" "T" "U" "V" "CB"))
      (iter (for num in '("2" ""))
            (iter (for middle in '("M" ""))
                  (iter (for type in '("TICS"))
                        (setf (getf *keyword-separator-alist*
                                    (make-keyword
                                     (concatenate 'string
                                                  dimension num middle type)))
                              ",")))))


(iter (for dimension in '("X" "Y" "Z" "R" "T" "U" "V" "CB"))
      (iter (for num in '("2" ""))
            (iter (for middle in '("M" ""))
                  (iter (for type in '("RANGE"))
                        (setf (getf *keyword-separator-alist*
                                    (make-keyword
                                     (concatenate 'string
                                                  dimension num middle type)))
                              ":")))))

(defun wrap-range-result (keyword result)
  (let ((name (symbol-name keyword)))
    (if (and (< 5 (length name))
             (equal "RANGE" (subseq name (- (length name) 5))))
        (format nil "[~a]" result)
        result)))

(defun gp-quote-for (keyword value)
  (typecase value
    (atom
     (gp-quote value))
    (list
     (if-let ((sep (getf *keyword-separator-alist* keyword)))
       (make-keyword
        (wrap-range-result
         keyword
         (apply #'concatenate
                'string
                (iter (for arg in value)
                      (unless (first-time-p)
                        (collect sep))
                      (collect (princ-to-string arg))))))
       (gp-quote value)))))


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
    (cond ((and (null terminal) (null output)) (error "gp-setup is missing both :terminal and :output."))
	  ((null terminal) (ematch (pathname-type (pathname output))
			     ((and type (or :unspecific :wild nil "*"))
			      (error "gp-setup is missing :terminal, and ~
                               it failed to guess the terminal type from the output pathname ~a,
                                based on its pathname-type ~a"
				     output type))
			     ((and type (type string)) 
			      (setf terminal (make-keyword type))))))
    (setf *plot-type-multiplot* multiplot)
    (apply #'gp :set :terminal (ensure-list terminal))
    (apply #'gp :set :output (ensure-list output))
    (remf args :terminal)
    (remf args :output)
    (map-plist args (curry #'gp :set))))

(defun gp (&rest args)
  "Render single line gnuplot commands
   For example:
     Command:
       (gp :set :param)
       (gp :set :style :line 1 :lc :rgb '(\"'#999999'\") :lt 1 '(\"#border\"))
     Generates:
       set param
       set style line 1 lc rgb \"'#999999'\" lt 1 \"#border\"

- Return:
  NIL
"
  (fresh-line *user-stream*)
  (%gp args)
  (values))

(defun %gp (args)
  (write-char #\Space *user-stream*)
  (iter (generate (arg next rest) on args)
        (next arg)
        (cond
          ((getf *keyword-separator-alist* arg)
           (format *user-stream* "~A ~A " arg (gp-quote-for arg next))
           (next arg))
          (t
           (format *user-stream* "~A " (gp-quote arg))))))

(defmacro with-plots ((stream &key debug (external-format :default) (persist nil))
                      &body body)
  "with-plots 
      stream                      - print commands to this stream.

    Keywords
      :debug boolean              - prints debugging information.
      :external-format  encoding  - the external format need for output, default is :default.
      :persist boolean            - allows windows from GUI terminals to remain open; 
                                    default is nil (not to persist).
    Body
      Place your plot code here."
  (check-type stream symbol)
  `(call-with-plots ,external-format ,persist ,debug (lambda (,stream) ,@body)))


(define-condition new-plot () ())

(defun call-with-plots (external-format persist debug body)
    (let ((*plot-type* nil)
          (*print-case* :downcase)
          (before-plot-stream (make-string-output-stream))
          (after-plot-stream (make-string-output-stream))
          (*data-stream* (make-string-output-stream))
          (*plot-command-stream* (make-string-output-stream))
          *plot-type-multiplot*)
      (let ((*user-stream* before-plot-stream))
        (unwind-protect
            (handler-bind ((new-plot
                            (lambda (c)
                              (declare (ignore c))
                              (setf *user-stream* after-plot-stream)
                              ;; ensure there is a newline
                              (terpri after-plot-stream))))
              (funcall body (make-synonym-stream '*user-stream*)))
          ;; this is required when gnuplot handles png -- otherwise the file buffer is not flushed
          (format after-plot-stream "~%set output")
          (with-input-from-string (in ((lambda (str)
                                         (if debug
                                             (print str *error-output*)
                                             str))
                                       (concatenate 'string
                                                    (get-output-stream-string before-plot-stream)
                                                    (get-output-stream-string *plot-command-stream*)
                                                    (get-output-stream-string *data-stream*)
                                                    (get-output-stream-string after-plot-stream))))
            (uiop:run-program (if persist (prog1 (concatenate 'string *gnuplot-home* " -persist ")
					    (if debug (print "-persist" *error-output*)))
				  *gnuplot-home*)
                              :input in
                              :output :interactive
                              :error-output :interactive
                              :external-format external-format))))))

(defun data-filename (data)
  (etypecase data
    (string   data) ; expression
    (pathname (format nil "'~a'" data))
    (function "'-'")))

(defun %plot (data &rest args
              &key (type :plot) &allow-other-keys
              &aux (filename (data-filename data)))
  ;; print the filename
  (cond
    ((or (null *plot-type*) *plot-type-multiplot*)
     (format *plot-command-stream* "~%~a ~a" type filename)
     (setf *plot-type* type))
    ((and (eq type *plot-type*) (not *plot-type-multiplot*))
     (format *plot-command-stream* ", ~a" filename))
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
         ((list :using val)
          (format *plot-command-stream* "~:[, ''~;~] using ~a" first-using (gp-quote-for :using val))
          (setf first-using nil))
         ((list key val)
          (format *plot-command-stream* " ~a ~a" key (gp-quote val)))))))

  (signal 'new-plot)
  (when (functionp data)
    ;; ensure the function is called once
    (let ((correct-stream (if *plot-type-multiplot* *plot-command-stream* *data-stream*))
          (*user-stream* (make-string-output-stream)))
      (flet ((plt ()
               (terpri correct-stream)
               (write-sequence (get-output-stream-string *user-stream*)
                               correct-stream)
               (format correct-stream "~&end~%")))
        (unwind-protect
            (funcall data) ;;; protect against local escape from DATA
          (prog1
            (let ((n (count :using args)))
              (if (> n 0)
                  (loop repeat n do (plt))
                  (plt)))
            (close *user-stream*)))))))

(defun plot (data &rest args &key using &allow-other-keys)
  "DATA is either a function producing data, a string
representing gnuplot functions, or a pathanme for input data."
  (declare (ignorable using))
  (apply #'%plot data args))
(defun splot (data &rest args &key using &allow-other-keys)
  "DATA is either a function producing data, a string
representing gnuplot functions, or a pathanme for input data."
  (declare (ignorable using))
  (apply #'plot data :type :splot args))
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

(defun fit (expression data &rest args &key using via unitweights yerror xyerror zerror errors &allow-other-keys)
  "EXPRESSION is a string representing gnuplot fitting target expressions e.g. f(x), ax**2+bx+c.
DATA is either a function producing data, a string
representing a gnuplot expression, or a pathanme for input data."
  (declare (ignorable using unitweights yerror xyerror zerror errors))
  (assert via (via) "specify the values to be fitted")
  ;; I considered using GP here, but it quotes functions unavoidably
  (format *user-stream* "~&fit ~a ~a " expression (data-filename data))
  (%gp args)
  (when (functionp data)
    (fresh-line *user-stream*)
    (funcall data)
    (gp :end)))
