;;;; test suite
;;;;
;;;; This software is part of adopt-subcommands. See README.org for more
;;;; information. See LICENSE for license information.

(uiop:define-package #:adopt-subcommands-test
    (:use #:cl
          #:adopt-subcommands)
  (:import-from #:adopt)
  (:import-from #:fiveam
                #:def-test
                #:is
                #:is-true
                #:signals))

(in-package #:adopt-subcommands-test)

(fiveam:def-suite :adopt-subcommands)
(fiveam:in-suite :adopt-subcommands)

(defparameter *opt-verbose*
  (adopt:make-option
   :verbose
   :long "verbose"
   :short #\V
   :help "verbosity"
   :initial-value 0
   :reduce #'1+))

(defparameter *opt-host*
  (adopt:make-option
   :host
   :long "host"
   :help "host"
   :parameter "HOST"
   :reduce #'adopt:last))

(defparameter *opt-mode*
  (adopt:make-option
   :mode
   :short #\a
   :help "mode a"
   :reduce (constantly :a)))

(defparameter *c*
  (make-subcommand-terminal :name "a b c"
                            :summary "c"
                            :usage "c"
                            :help "c"
                            :contents (list *opt-mode*)
                            :function 'run-c))

(defparameter *b*
  (make-subcommand-folder :name "a b"
                          :summary "b"
                          :usage "b"
                          :help "b"
                          :contents (list *c*
                                          *opt-host*)
                          :function 'run-b))

(defparameter *a*
  (make-subcommand-folder :name "a"
                          :summary "a"
                          :usage "a"
                          :help "a"
                          :contents (list *b*
                                          *opt-verbose*)))

(defvar *c-run-p*)
(defvar *c-args*)
(defvar *c-opts*)

(defvar *b-run-p*)
(defvar *b-args*)
(defvar *b-opts*)

(defun run-b (arguments options thunk)
  (psetf *b-run-p* t
         *b-args* arguments
         *b-opts* options)
  (funcall thunk))

(defun run-c (arguments options path)
  (declare (ignore path))
  (psetf *c-run-p* t
         *c-args* arguments
         *c-opts* options))

(fiveam:def-fixture globals-bound ()
  (let (*b-run-p*
        *b-args*
        *b-opts*
        *c-run-p*
        *c-args*
        *c-opts*)
    (&body)))

(defun hash-table-equal-p (ht &rest args)
  (and
   (is (hash-table-p ht))
   (is (= (hash-table-count ht) (/ (length args) 2)))
   (loop
     :for (k v) :on args :by #'cddr
     :always (is (equal (gethash k ht) v)))))

(def-test simple (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "c"))
  (is-true *b-run-p*)
  (is (equal '("c") *b-args*))
  (is-true *c-run-p*)
  (is (null *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host nil
                          :verbose 0
                          :mode nil)))

(def-test simple-with-args (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "c" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host nil
                          :verbose 0
                          :mode nil)))

(def-test simple-with-a-opts (:fixture globals-bound)
  (dispatch *a* :arguments (list "-VV" "b" "c" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host nil
                          :verbose 2
                          :mode nil)))

(def-test simple-with-a-opts-2 (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "-VV" "c" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host nil
                          :verbose 2
                          :mode nil)))

(def-test simple-with-a-opts-3 (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "-VV" "c" "-V" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host nil
                          :verbose 3
                          :mode nil)))

(def-test simple-with-b-opts (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "--host" "gitlab.com" "-VV" "c" "-V" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host "gitlab.com"
                          :verbose 3
                          :mode nil)))

(def-test simple-with-b-opts-2 (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "-VV" "c" "-V" "--host" "gitlab.com" "1" "2"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host "gitlab.com"
                          :verbose 3
                          :mode nil)))

(def-test simple-with-c-opts (:fixture globals-bound)
  (dispatch *a* :arguments (list "b" "--host" "gitlab.com" "-VV" "c" "-V" "1" "2" "-a" "-V"))
  (is-true *c-run-p*)
  (is (equal (list "1" "2") *c-args*))
  (is (hash-table-equal-p *c-opts*
                          :host "gitlab.com"
                          :verbose 4
                          :mode :a)))

(def-test unknown-subcommand (:fixture globals-bound)
  (signals unknown-subcommand
    (dispatch *a* :arguments (list "b" "d"))))

(def-test folder-is-terminal (:fixture globals-bound)
  (signals folder-is-terminal
    (dispatch *a* :arguments (list "b")))
  (signals folder-is-terminal
    (dispatch *a* :arguments (list)))
  (let ((string
          (with-output-to-string (s)
            (handler-case
                (dispatch *a* :arguments (list "b"))
              (folder-is-terminal (c)
                (print-help (folder-is-terminal-path c) :stream s))))))
    (is (uiop:string-suffix-p (uiop:stripln string) "Available subcommands:
  c"))))
