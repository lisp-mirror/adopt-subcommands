;;;; adopt-subcommands-test system definition
;;;;
;;;; This software is part of adopt-subcommands. See README.org for more
;;;; information. See LICENSE for license information.

(defsystem #:adopt-subcommands-test
  :license "MIT"
  :version (:read-file-form "version.lisp-expr")
  :pathname "test"
  :components ((:file "test"))
  :depends-on (#:fiveam #:adopt-subcommands))
