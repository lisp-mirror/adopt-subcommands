;;;; adopt-subcommands
;;;;
;;;; This software is part of adopt-subcommands. See README.org for more
;;;; information. See LICENSE for license information.

(uiop:define-package #:adopt-subcommands
    (:use #:cl)
  (:export #:add-subcommand
           #:dispatch
           #:folder-is-terminal
           #:folder-is-terminal-folder
           #:make-subcommand-folder
           #:make-subcommand-terminal
           #:print-folder-help
           #:print-help
           #:print-help-and-exit
           #:unknown-subcommand
           #:unknown-subcommand-folder
           #:unknown-subcommand-key))

(in-package #:adopt-subcommands)


;;; * Utilities

(defun plist-remove-keys (plist &rest keys)
  "Return a new plist that contains all keys and values from PLIST, excepting
any key in KEYS."
  (loop
    :for (k v) :on plist :by #'cddr
    :unless (member k keys)
      :append (list k v)))

(defun key-from-name (name)
  "Generate a key from NAME by splitting on space and returning the last
element."
  (first (split-sequence:split-sequence #\Space name
                                        :remove-empty-subseqs t
                                        :from-end t
                                        :count 1)))

(defun copy-hash-table (ht)
  "Copy a hash table"
  (let ((out (make-hash-table :test 'eql
                              :size (hash-table-size ht))))
    (maphash (lambda (k v)
               (setf (gethash k out) v))
             ht)
    out))

(defun merge-hts (existing-ht new-ht)
  "Return a copy of EXISTING-HT with all key/value pairs from NEW-HT inserted
into it."
  (let ((out (copy-hash-table existing-ht)))
    (maphash (lambda (k v)
               (setf (gethash k out) v))
             new-ht)
    out))

(defun hash-table-keys (ht)
  "Return a list of all keys in HT."
  (loop :for k :being :the :hash-keys :in ht
        :collect k))


;;; * Terminal subcommands

(defclass subcommand-terminal ()
  ((key
    :initarg :key
    :accessor key)
   (interface
    :initarg :interface
    :accessor interface)
   (function
    :initarg :function
    :accessor %function)))

(defun subcommand-terminal-p (object)
  (typep object 'subcommand-terminal))

(defun make-subcommand-terminal (&rest keys
                                 &key name summary usage help manual examples contents function
                                   key)
  "Create and return a terminal subcommand interface.

The subcommand folder into which this is placed uses `key` to determine when to
dispatch to this interface. If `key` is not specified, one is determined
automatically by splitting `name` on space characters and taking the last
entry.

When this subcommand is dispatched, `function` is called with a list of
remaining arguments and an `eql` hash table containing all the parsed
options. `function` is required.

The remaining arguments are passed directly to `adopt:make-interface`."
  (declare (ignore summary usage help manual examples contents))
  (check-type function (or (and (not null) symbol) function))
  (check-type key (or null string))
  (make-instance 'subcommand-terminal
                 :function function
                 :key (or key (key-from-name name))
                 :interface (apply #'adopt:make-interface (plist-remove-keys keys :key :function))))


;;; * Subcommand folders

(defclass subcommand-folder ()
  ((key
    :initarg :key
    :accessor key)
   (pseudo-interface
    :initarg :pseudo-interface
    :accessor pseudo-interface)
   (subcommands
    :initform (make-hash-table :test 'equal)
    :accessor subcommands)
   (function
    :initarg :function
    :accessor %function)))

(defun subcommand-folder-p (object)
  (typep object 'subcommand-folder))

(defun add-subcommand (folder subcommand)
  "Add a subcommand to a folder."
  (setf (gethash (key subcommand) (subcommands folder)) subcommand))

(defun make-subcommand-folder (&rest keys
                               &key name summary usage help manual examples contents key function)
  "Create and return a subcommand folder.

If this is placed into another folder, that folder uses `key` to determine when
to dispatch to this interface. If `key` is not specified, one is determined
automatically by splitting `name` on space characters and taking the last
entry.

When this subcommand is dispatched, `function` is called with a list of
remaining arguments, an `eql` hash table containing all the parsed options, and
a thunk that continues processing the subcommands when called. If not provided,
the thunk is simply called.

The remaining arguments are passed directly to `adopt:make-interface`."
  (declare (ignore summary usage help manual examples))
  (check-type contents list)
  (check-type key (or null string))
  (check-type function (or null symbol function))
  (let* ((subcommand-terminals (remove-if-not #'subcommand-terminal-p contents))
         (subcommand-folders (remove-if-not #'subcommand-folder-p contents))
         (options (set-difference (set-difference contents subcommand-terminals)
                                  subcommand-folders))
         (pseudo-interface (apply #'adopt:make-interface
                                    :contents options
                                    (plist-remove-keys keys :key :contents :function)))
         (folder (make-instance 'subcommand-folder
                                 :key (or key (key-from-name name))
                                 :pseudo-interface pseudo-interface
                                 :function function)))
    (dolist (sub subcommand-terminals)
      (add-subcommand folder sub))
    (dolist (sub subcommand-folders)
      (add-subcommand folder sub))
    folder))


;;; * Printing Help

(defun print-folder-help (folder &rest args &key (stream *standard-output*) (width 80))
  "Prints help for the folder. Ends the help message with a list of the
available subcommands."
  (apply #'adopt:print-help (pseudo-interface folder) args)
  (terpri stream)
  (write-line (bobbin:wrap (format nil "Available subcommands: ~{~A~^ ~}"
                                   (hash-table-keys (subcommands folder)))
                           width)
              stream))

(defun print-help-and-exit (c &key (code 1) (stream *standard-output*))
  (invoke-restart (find-restart 'print-help-and-exit c) :code code :stream stream))

(defgeneric print-help (ui &rest args))

(defmethod print-help ((ui subcommand-folder) &rest args)
  (apply #'print-folder-help ui args))

(defmethod print-help ((ui subcommand-terminal) &rest args)
  (apply #'adopt:print-help (interface ui) args))

(defmethod print-help (ui &rest args)
  (apply #'adopt:print-help ui args))


;;; * Dispatching

(define-condition unknown-subcommand (error)
  ((folder
    :initarg :folder
    :reader unknown-subcommand-folder)
   (key
    :initarg :key
    :reader unknown-subcommand-key))
  (:documentation
   "Signals that a subcommand could not be found."))

(define-condition folder-is-terminal (error)
  ((folder
    :initarg :folder
    :reader folder-is-terminal-folder))
  (:documentation
   "Signals that no terminal subcommand was found"))

(defun signal-folder-is-terminal (folder)
  (restart-case
      (error 'folder-is-terminal :folder folder)
    (print-help-and-exit (&key (code 1) (stream *standard-output*))
      :report "Print help to STREAM and exit with CODE"
      (print-folder-help folder :stream stream)
      (adopt:exit code))))

(defun signal-unknown-subcommand (folder key)
  (restart-case
      (error 'unknown-subcommand :folder folder :key key)
    (print-help-and-exit (&key (code 1) (stream *standard-output*))
      :report "Print help to STREAM and exit with CODE"
      (print-folder-help folder :stream stream)
      (adopt:exit code))))

(defgeneric dispatch (interface &key arguments options))

(defmethod dispatch ((terminal subcommand-terminal)
                     &key (arguments (rest (adopt:argv)))
                       (options (make-hash-table)))
  (multiple-value-bind (new-arguments new-options)
      (adopt:parse-options (interface terminal) arguments)
    (funcall (%function terminal) new-arguments (merge-hts options new-options))))

(defmethod dispatch ((folder subcommand-folder)
                     &key (arguments (rest (adopt:argv)))
                       (options (make-hash-table)))
  (multiple-value-bind (new-arguments new-options)
      (handler-bind ((adopt:unrecognized-option 'adopt:treat-as-argument))
        (adopt:parse-options (pseudo-interface folder) arguments))
    (let ((options (merge-hts options new-options))
          (arguments new-arguments))
      (flet ((thunk (&key (arguments arguments) (options options))
               (unless (not (null (first arguments)))
                 ;; This folder is terminal. Can't figure out what to do next!
                 (signal-folder-is-terminal folder))
               (let ((subcommand (gethash (first arguments) (subcommands folder))))
                 (unless (not (null subcommand))
                   (signal-unknown-subcommand folder (first arguments)))
                 (dispatch subcommand :options options :arguments (rest arguments)))))
        (let ((next (%function folder)))
          (if next
              (funcall next arguments options #'thunk)
              (thunk)))))))
