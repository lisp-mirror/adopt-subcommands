(require "asdf")
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; When this gets into Quicklisp...
  #+quicklisp
  (ql:quickload '(:adopt-subcommands) :silent t)
  #-quicklisp
  (asdf:load-system :adopt-subcommands))

(defpackage :subex
  (:use :cl)
  (:export :toplevel *ui*))

(in-package :subex)

;;;; Subcommand Foo -----------------------------------------------------------
(defparameter *o/foo/a*
  (adopt:make-option 'a :result-key 'mode :short #\a :help "run foo in mode A" :reduce (constantly :a)))

(defparameter *o/foo/b*
  (adopt:make-option 'b :result-key 'mode :short #\b :help "run foo in mode B" :reduce (constantly :b)))

(defparameter *ui/foo*
  (adopt-subcommands:make-subcommand-terminal
    :name "subex foo"
    :usage "foo [-a|-b]"
    :summary "foo some things"
    :help "foo some things"
    :contents (list *o/foo/a* *o/foo/b*)
    :function 'run/foo))

(defun run/foo (arguments options path)
  (declare (ignore path))
  (unless (null arguments)
    (error "Foo does not take arguments, got ~S" arguments))
  (format t "Running foo in ~A mode.~%" (gethash 'mode options)))

;;;; Subcommand Bar -----------------------------------------------------------
(defparameter *o/bar/meow*
  (adopt:make-option 'meow :long "meow" :help "meow loudly after each step" :reduce (constantly t)))

(defparameter *ui/bar*
  (adopt-subcommands:make-subcommand-terminal
    :name "subex bar"
    :usage "bar [--meow] FILE..."
    :summary "bar some files"
    :help "bar some files"
    :contents (list *o/bar/meow*)
    :function 'run/bar))

(defun run/bar (paths options path)
  (declare (ignore path))
  (when (null paths)
    (error "Bar requires arguments, got none."))
  (dolist (p paths)
    (format t "Bar-ing ~A.~%" p)
    (when (gethash 'meow options)
      (write-line "meow."))))

;;;; Global Options and UI ----------------------------------------------------
(defparameter *o/help*
  (adopt:make-option 'help :long "help" :help "display help and exit" :reduce (constantly t)))

(defparameter *o/version*
  (adopt:make-option 'version :long "version" :help "display version and exit" :reduce (constantly t)))

(defparameter *ui*
  (adopt-subcommands:make-subcommand-folder
    :name "subex"
    :usage "[subcommand] [options]"
    :help "subcommand example program"
    :summary "an example program that uses subcommands"
    :contents (list *o/help* *o/version* *ui/foo* *ui/bar*)
    :function 'run/global))

(defun run/global (arguments options path thunk)
  (declare (ignore arguments path))
  (when (gethash 'version options)
    (write-line "1.0.0")
    (adopt:exit))
  (funcall thunk))

;;;; Toplevel -----------------------------------------------------------------

(defun toplevel ()
  #+sbcl (sb-ext:disable-debugger)
  (handler-bind ((adopt-subcommands:folder-is-terminal 'adopt-subcommands:print-help-and-exit))
    (adopt-subcommands:dispatch *ui* :print-help-and-exit 'help)))

(toplevel)
