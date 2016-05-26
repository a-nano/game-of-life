#|
  This file is a part of game-of-life project.
  Copyright (c) 2016 akeo_quick
|#

(in-package :cl-user)
(defpackage game-of-life-test-asd
  (:use :cl :asdf))
(in-package :game-of-life-test-asd)

(defsystem game-of-life-test
  :author "akeo_quick"
  :license ""
  :depends-on (:game-of-life
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "game-of-life"))))
  :description "Test system for game-of-life"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
