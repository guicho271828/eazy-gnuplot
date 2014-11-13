#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#

(in-package :cl-user)
(defpackage eazy-gnuplot
  (:use :cl :iterate
        :optima
        :alexandria
        :trivial-shell)
  (:export :with-plots
           :func-plot
           :plot
           :gp-setup
           :*gnuplot-home*))
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



(defmacro with-plots ((stream &key debug) &body body)
  (assert (symbolp stream))
  (once-only (debug)
    (with-gensyms (string-stream)
      `(let* ((,string-stream (make-string-output-stream))
              (,stream ,(if debug
                            `(make-broadcast-stream
                              ,string-stream
                              *trace-output*)
                            string-stream)))
         (call-with-plots ,stream ,string-stream (lambda () ,@body))))))

(defvar *data-functions*)
(defvar *plot-stream*)
(defun call-with-plots (*plot-stream* string-stream body)
  (let (*data-functions*)
    (funcall body)
    (dolist (fn (nreverse *data-functions*))
      (when fn ; could be nil
        (funcall fn))))
  (shell-command
   *gnuplot-home*
   :input
   (make-string-input-stream
    (get-output-stream-string string-stream))))

(defun plot (data-producing-fn &rest args
             &key &allow-other-keys)
  (let ((*print-case* :downcase))
    (format *plot-stream* "~:[~&plot~;,~] '-'" *data-functions*)
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
     (lambda ()
       (funcall data-producing-fn)
       (format *plot-stream* "~&end"))
     *data-functions*)))

(defun func-plot (string &rest args
                  &key &allow-other-keys)
  (let ((*print-case* :downcase))
    (format *plot-stream* "~@[,~] ~a" *data-functions* string)
    (gp-map-args
     args
     (lambda (&rest args)
       (match args
         ((list :using (and val (type list)))
          (format *plot-stream* " using ~{~a~^:~}" val))
         ((list key val)
          (format *plot-stream* " ~a ~a"
                  key (gp-quote val))))))
    (push nil *data-functions*)))

