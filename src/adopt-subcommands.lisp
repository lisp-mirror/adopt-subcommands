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
           #:folder-is-terminal-path
           #:make-subcommand-folder
           #:make-subcommand-terminal
           #:print-help
           #:print-help-and-exit
           #:unknown-subcommand
           #:unknown-subcommand-folder
           #:unknown-subcommand-key
           #:unknown-subcommand-path))

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


;;; * Subcommands

(defclass subcommand ()
  ((key
    :initarg :key
    :accessor key)
   (function
    :initarg :function
    :accessor %function)
   (interface
    :initarg :interface
    :accessor interface)))


;;; * Terminal subcommands

(defclass subcommand-terminal (subcommand)
  ())

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

(defclass subcommand-folder (subcommand)
  ((subcommands
    :initform (make-hash-table :test 'equal)
    :accessor subcommands)))

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
         (interface (apply #'adopt:make-interface
                             :contents options
                             (plist-remove-keys keys :key :contents :function)))
         (folder (make-instance 'subcommand-folder
                                 :key (or key (key-from-name name))
                                 :interface interface
                                 :function function)))
    (dolist (sub subcommand-terminals)
      (add-subcommand folder sub))
    (dolist (sub subcommand-folders)
      (add-subcommand folder sub))
    folder))


;;; * Subcommand paths

(defclass subcommand-path ()
  ((path
    :initarg :path
    :accessor path)))

(defun extend-path (new-head path)
  (make-instance 'subcommand-path
                  :path (list* new-head (path path))))


;;; * Conditions

(define-condition subcommand-error (error)
  ())

(define-condition unknown-subcommand (subcommand-error)
  ((folder
    :initarg :folder
    :reader unknown-subcommand-folder)
   (key
    :initarg :key
    :reader unknown-subcommand-key)
   (path
    :initarg :path
    :reader unknown-subcommand-path))
  (:documentation
   "Signals that a subcommand could not be found."))

(define-condition folder-is-terminal (subcommand-error)
  ((folder
    :initarg :folder
    :reader folder-is-terminal-folder)
   (path
    :initarg :path
    :reader folder-is-terminal-path))
  (:documentation
   "Signals that no terminal subcommand was found"))


;;; * Printing Help

(defun print-entry-options (entry stream width option-width)
  (format stream "~%~A Options:~%" (adopt::name (interface entry)))
  (dolist (group (adopt::groups (interface entry)))
    (when (or (adopt::options group) (adopt::help group))
      (format stream "~%~A:~%" (or (adopt::title group) (adopt::name group) "Options"))
      (let* ((help (adopt::help group))
             (help-column 2)
             (help-width (- width help-column))
             (option-column 2)
             (option-padding 2)
             (doc-column (+ option-column option-width option-padding))
             (doc-width (- width doc-column)))
        (when help
          (format stream "~{  ~A~^~%~}~2%"
                  (bobbin:wrap (list help) help-width)))
        (dolist (option (adopt::options group))
          (adopt::print-option-help stream option option-column doc-column doc-width))))))

(defun print-path-help (path
                        &key
                          (stream *standard-output*)
                          (program-name (first (adopt:argv)))
                          (width 80)
                          (option-width 20)
                          (include-examples t))
  (assert (> width (+ 2 option-width 2)) (width option-width)
          "WIDTH (~D) must be at least 4 greater than OPTION-WIDTH (~D)"
          width option-width)
  (let* ((path (path path))
         (most-specific-interface (interface (first path))))
    (format stream "~A - ~A~2%"
            (adopt::name most-specific-interface)
            (adopt::summary most-specific-interface))
    (format stream "USAGE: ~A ~A~2%" program-name (adopt::usage most-specific-interface))
    (format stream (bobbin:wrap (adopt::help most-specific-interface) width))
    (format stream "~%")
    (dolist (entry (reverse path))
      (print-entry-options entry stream width option-width))
    (let* ((examples (adopt::examples most-specific-interface))
           (example-column 2)
           (example-width (- width example-column)))
      (when (and examples include-examples)
        (format stream "~%Examples:~%")
        (loop :for (prose . command) :in examples
              :do
                 (format stream "~%~{  ~A~^~%~}~2%      ~A~%"
                         (bobbin:wrap (list prose) example-width)
                         command))))
    (when (subcommand-folder-p (first path))
      (format stream "~%Available subcommands:~%~{  ~A~^~%~}~%"
              (bobbin:wrap (list (format nil "~{~A~^, ~}"
                                         (hash-table-keys (subcommands (first path)))))
                           width)))))

(defgeneric print-help-and-exit (object &key exit-code stream program-name width option-width
                                          include-examples))

(defmethod print-help-and-exit (object &rest keys &key (exit-code 0) &allow-other-keys)
  (apply #'print-help object (plist-remove-keys keys :exit-code))
  (uiop:quit exit-code))

(defmethod print-help-and-exit ((c subcommand-error) &rest keys)
  (apply #'invoke-restart (find-restart 'print-help-and-exit c) keys))

(defgeneric print-help (object &key stream program-name width option-width include-examples))

(defmethod print-help ((object subcommand-path) &rest args)
  (apply #'print-path-help object args))

(defmethod print-help ((ui subcommand-folder) &rest args)
  (apply #'print-help (make-instance 'subcommand-path :path (list ui)) args))

(defmethod print-help ((ui subcommand-terminal) &rest args)
  (apply #'adopt:print-help (interface ui) args))

(defmethod print-help (ui &rest args)
  (apply #'adopt:print-help ui args))


;;; * Dispatching

(defun signal-folder-is-terminal (folder path)
  (restart-case
      (error 'folder-is-terminal :folder folder :path path)
    (print-help-and-exit (&key (exit-code 1) (stream *standard-output*))
      :report "Print help to STREAM and exit with EXIT-CODE"
      (print-help-and-exit path :exit-code exit-code :stream stream))))

(defun signal-unknown-subcommand (folder path key)
  (restart-case
      (error 'unknown-subcommand :folder folder :key key :path path)
    (print-help-and-exit (&key (exit-code 1) (stream *standard-output*))
      :report "Print help to STREAM and exit with EXIT-CODE"
      (print-help-and-exit path :stream stream :exit-code exit-code))))

(defun dispatch (interface &key (arguments (rest (adopt:argv))) print-help-and-exit)
  (dispatch-subcommand interface :arguments arguments
                                 :options (make-hash-table)
                                 :path (make-instance 'subcommand-path :path nil)
                                 :print-help-and-exit print-help-and-exit))

(defgeneric dispatch-subcommand (interface &key arguments options print-help-and-exit))

(defmethod dispatch-subcommand ((terminal subcommand-terminal)
                                &key (arguments (rest (adopt:argv)))
                                  (options (make-hash-table))
                                  path
                                  print-help-and-exit)
  (multiple-value-bind (new-arguments new-options)
      (adopt:parse-options (interface terminal) arguments)
    (let ((options (merge-hts options new-options))
          (arguments new-arguments)
          (path (extend-path terminal path)))
      (when (and (not (null print-help-and-exit))
                 (gethash print-help-and-exit options))
        (print-help-and-exit path))
      (funcall (%function terminal) arguments options path))))

(defmethod dispatch-subcommand ((folder subcommand-folder)
                                &key (arguments (rest (adopt:argv)))
                                  (options (make-hash-table))
                                  path
                                  print-help-and-exit)
  (multiple-value-bind (new-arguments new-options)
      (handler-bind ((adopt:unrecognized-option 'adopt:treat-as-argument))
        (adopt:parse-options (interface folder) arguments))
    (let ((options (merge-hts options new-options))
          (arguments new-arguments))
      (flet ((thunk (&key (arguments arguments) (options options))
               (let ((path (extend-path folder path)))
                 (unless (not (null (first arguments)))
                   (if (and (not (null print-help-and-exit))
                            (gethash print-help-and-exit options))
                       (print-help-and-exit path)
                       ;; This folder is terminal and help was not
                       ;; requested. Can't figure out what to do next!
                       (signal-folder-is-terminal folder path)))
                 (let ((subcommand (gethash (first arguments) (subcommands folder))))
                   (unless (not (null subcommand))
                     (signal-unknown-subcommand folder path (first arguments)))
                   (dispatch-subcommand subcommand :options options
                                                   :arguments (rest arguments)
                                                   :path path
                                                   :print-help-and-exit print-help-and-exit)))))
        (let ((next (%function folder)))
          (if next
              (funcall next arguments options #'thunk)
              (thunk)))))))
