#|
  This file is a part of eazy-gnuplot project.
  Copyright (c) 2014 guicho
|#

#|
  Author: guicho
|#



(in-package :cl-user)
(defpackage eazy-gnuplot-asd
  (:use :cl :asdf))
(in-package :eazy-gnuplot-asd)


(defsystem eazy-gnuplot
  :version "0.1"
  :author "Masataro Asai --  guicho2.71828@gmail.com"
  :license "LLGPL"
  :depends-on (:iterate :optima :alexandria
                        #+(and ccl linux)
                        :trivial-shell
                        #-(and ccl linux)
                        :eazy-process)
  :components ((:module "src"
                :components
                ((:file "package"))))
  :description "An intuitive CL interface to gnuplot."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"includes/README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  
  :in-order-to ((test-op (load-op eazy-gnuplot.test))))
