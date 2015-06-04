#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage eazy-gnuplot
  (:use :cl :iterate
        :optima
        :alexandria)
  (:import-from 
   #+(and ccl linux)
   :trivial-shell
   #-(and ccl linux)
   :eazy-process
   :shell-command)
  (:export :with-plots
           :func-plot
           :func-splot
           :plot
           :splot
           :gp-setup
           :*gnuplot-home*
           :row))
(in-package :eazy-gnuplot)

;; gnuplot interface

(defvar *gnuplot-home* "gnuplot")

(defun gp-quote (value)
  (match value
    ((type string) (format nil "\"~a\"" value))
    ((type pathname) (gp-quote (namestring value)))
    (nil "")
    ((type list)
     (reduce (lambda (str val)
               (format nil "~a ~a"
                       str (gp-quote val)))
             value))
    (_ value)))

(defun gp-map-args (args fn)
  (iter (for keyword in args by #'cddr)
        (for value in (cdr args) by #'cddr)
        (funcall fn keyword value)))

(defun gp-setup (&rest args
                 &key
                   (terminal :pdf terminal-p)
                   (output nil output-p)
                   &allow-other-keys)
  (let ((*print-case* :downcase))
    (unless terminal-p
      (format *plot-stream* "~&set ~a ~a" :terminal (gp-quote terminal)))
    (unless output-p
      (format *plot-stream* "~&set ~a ~a" :output (gp-quote output)))
    (gp-map-args args
                 (lambda (key val)
                   (format *plot-stream* "~&set ~a ~a"
                           key (gp-quote val))))))

(defmacro with-plots ((&optional
                       (stream '*standard-output*)
                       &key debug (external-format :default))
                      &body body)
  (assert (symbolp stream))
  (once-only (debug)
    (with-gensyms (output-string-stream)
      `(let* ((,output-string-stream (make-string-output-stream))
              (,stream (if ,debug
                           (make-broadcast-stream
                              ,output-string-stream
                              *error-output*)
                           ,output-string-stream)))
         (call-with-plots ,stream
                          ,output-string-stream
                          ,external-format
                          (lambda () ,@body))))))

(defvar *data-functions*)
(defvar *plot-stream*)
(defvar *plot-type*)
(defun call-with-plots (*plot-stream*
                        output-string-stream
                        external-format
                        body)
  (let (*data-functions*
        *plot-type*
        (*print-case* :downcase))
    (funcall body)
    (dolist (fn (nreverse *data-functions*))
      (when fn ; could be nil
        (funcall fn))))
  ;; this is required when gnuplot handles png -- otherwise the file buffer is not flushed
  (format *plot-stream* "~&set output")
  (shell-command
   *gnuplot-home*
   :input (get-output-stream-string output-string-stream)
   #-(and ccl linux)
   :external-format external-format))

(defun %plot (data-producing-fn &rest args
              &key (type :plot) string &allow-other-keys)
  (if *data-functions*
      (if (eq type *plot-type*)
          (format *plot-stream* ", ~a" string)
          (error "Using incompatible plot type in a same figure!"))
      (progn
        (format *plot-stream* "~%~a ~a" type string)
        (setf *plot-type* type)))
  (remf args :type)
  (remf args :string)
  (gp-map-args
   args
   (lambda (&rest args)
     (match args
       ((list :using (and val (type list)))
        (format *plot-stream* " using ~{~a~^:~}" val))
       ((list key val)
        (format *plot-stream* " ~a ~a"
                key (gp-quote val))))))
  (push
   (if data-producing-fn
       (lambda ()
         (funcall data-producing-fn)
         (format *plot-stream* "~&end"))
       nil)
   *data-functions*))

(defun plot (data-producing-fn &rest args &key using &allow-other-keys)
  (apply #'%plot data-producing-fn :string "'-'" args))
(defun splot (data-producing-fn &rest args &key using &allow-other-keys)
  (apply #'plot data-producing-fn :type :splot args))
(defun func-plot (string &rest args &key using &allow-other-keys)
  (apply #'%plot nil :string string args))
(defun func-splot (string &rest args &key using &allow-other-keys)
  (apply #'func-plot string :type :splot args))

(defun row (&rest args)
  "Write a row"
  (format *plot-stream* "~&~{~a~^ ~}" args))

