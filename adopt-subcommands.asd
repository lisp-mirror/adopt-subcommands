;;;; adopt-subcommands system definition
;;;;
;;;; This software is part of adopt-subcommands. See README.org for more
;;;; information. See LICENSE for license information.

(defsystem #:adopt-subcommands
  :description "Extend the Adopt command line processing library to handle nested subcommands."
  :author "Eric Timmons <eric@timmons.dev>"
  :homepage "https://gitlab.com/daewok/adopt-subcommands"
  :bug-tracker "https://gitlab.com/daewok/adopt-subcommands/-/issues"
  :license "MIT"
  :version (:read-file-form "version.lisp-expr")
  :pathname "src"
  :in-order-to ((test-op (load-op "adopt-subcommands-test")))
  :perform (test-op (op c) (unless (symbol-call :fiveam :run! :adopt-subcommands)
                             (error "test failue")))
  :components ((:file "adopt-subcommands"))
  :depends-on (#:adopt #:bobbin #:split-sequence #:uiop))
