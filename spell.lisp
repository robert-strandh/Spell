(defpackage #:spell
  (:use #:cl)
  (:export #:english-lookup
           #:english-check-paragraph))
(in-package #:spell)

(defgeneric lookup (string dictionary))
(defgeneric insert (object string dictionary))

(defclass node () ())

(defmethod make-load-form ((object node) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defvar *dictionary*)

(defclass dictionary ()
  ((%contents :initform (make-instance 'node) :accessor contents)))

(defmethod make-load-form ((object dictionary) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defun load-dictionary (filename)
  (with-open-file (stream filename)
    (let* ((counter 0)
           (*dictionary* (make-instance 'dictionary)))
      (do ((line (read-line stream nil stream)
                 (read-line stream nil stream)))
          ((eq stream line))
        (unless (eq #\; (aref line 0))
          (let* ((string (concatenate 'string "(" line ")"))
                 (args (read-from-string string)))
            (apply #'word :spelling args)
            (incf counter))))
      (values *dictionary* counter))))

(defmethod lookup (string (dictionary dictionary))
  (assert (plusp (length string)))
  (%lookup string (length string) (contents dictionary)))

(defgeneric entries (node))

(defgeneric %lookup (string suffix node)
  (:method (string suffix node)
    (declare (ignore string suffix node))
    '()))

(defclass leaf-mixin ()
  ((%entries :initform '() :initarg :entries :accessor entries)))

(defmethod %lookup (string (suffix (eql 0)) (node leaf-mixin))
  (entries node))

(defclass interior-mixin ()
  ((%children :initform '() :initarg :children :accessor children)))

(defclass interior-node (interior-mixin node) ())

(defmethod %lookup (string (suffix (eql 0)) (node interior-node))
  '())

(defmethod %lookup (string suffix (node interior-mixin))
  (let ((child (find-child (aref string (- (length string) suffix))
                           (children node))))
    (if (null child)
        nil
        (%lookup string (1- suffix) child))))

(defclass leaf-node (leaf-mixin node) ())
(defclass interior-leaf-node (interior-mixin leaf-mixin node) ())

(defgeneric %insert (object string suffix node))

(defmethod %insert (object string (suffix (eql 0)) (node leaf-mixin))
  (push object (entries node)))

(defmethod %insert (object string (suffix (eql 0)) (node node))
  (change-class node 'leaf-node)
  (%insert object string 0 node))

(defmethod %insert (object string (suffix (eql 0)) (node interior-node))
  (change-class node 'interior-leaf-node)
  (%insert object string 0 node))

(defmethod %insert (object string suffix (node leaf-mixin))
  (change-class node 'interior-leaf-node)
  (%insert object string suffix node))

(defmethod %insert (object string suffix (node node))
  (change-class node 'interior-node)
  (%insert object string suffix node))

(defmethod %insert (object string suffix (node interior-mixin))
  (let ((child (find-child (aref string (- (length string) suffix))
                           (children node))))
    (when (null child)
      (setf child (make-instance 'node))
      (setf (children node)
            (add-child child
                       (aref string (- (length string) suffix))
                       (children node))))
    (%insert object string (1- suffix) child)))

(defmethod insert (object string (dictionary dictionary))
  (%insert object string (length string) (contents dictionary)))

(defgeneric find-child (char entries))

(defmethod find-child (char (entries list))
  (cdr (assoc char entries)))

(defmethod find-child (char (entries vector))
  (aref entries (- (char-code char) #.(char-code #\a))))

(defgeneric add-child (node char entries))

(defmethod add-child (node char (entries list))
  (acons char node entries))

(defmethod add-child (node char (entries vector))
  (setf (aref entries (- (char-code char) #.(char-code #\a)))
        node))
