#|
  This file is a part of game-of-life project.
  Copyright (c) 2016 akeo_quick
|#

#|
  Author: akeo_quick
|#

(in-package :cl-user)
(defpackage game-of-life-asd
  (:use :cl :asdf))
(in-package :game-of-life-asd)

(defsystem game-of-life
  :version "0.1"
  :author "akeo_quick"
  :license ""
  :depends-on (:lispbuilder-sdl
               :local-time)
  :components ((:module "src"
                :components
                ((:file "game-of-life" :depends-on ("model"))
                 (:file "model"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op game-of-life-test))))
