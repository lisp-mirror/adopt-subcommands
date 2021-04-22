;;;; adopt-subcommands-test system definition
;;;;
;;;; This software is part of adopt-subcommands. See README.org for more
;;;; information. See LICENSE for license information.

(defsystem #:adopt-subcommands-test
  :description "Tests for adopt-subcommands"
  :author "Eric Timmons <eric@timmons.dev>"
  :homepage "https://gitlab.com/daewok/adopt-subcommands"
  :bug-tracker "https://gitlab.com/daewok/adopt-subcommands/-/issues"
  :license "MIT"
  :version (:read-file-form "version.lisp-expr")
  :pathname "test"
  :components ((:file "test"))
  :depends-on (#:fiveam #:adopt-subcommands))
