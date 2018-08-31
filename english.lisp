(in-package #:spell)

(defparameter *english-dictionary* (make-instance 'dictionary))

(defun word (&rest arguments &key type spelling &allow-other-keys)
  (let ((arguments (copy-list arguments)))
    (remf arguments :type)
    (insert (apply #'make-instance (gethash type *word-types*) arguments)
            spelling
            *english-dictionary*)))

(defun lookup (word &optional (dictionary *english-dictionary*))
  (%lookup word dictionary))

(defun load-dictionary (filename)
  (setf *english-dictionary* (make-instance 'dictionary))
  (let ((stream (open filename)))
    (unwind-protect
         (let ((counter 0))
           (do ((line (read-line stream nil stream)
                      (read-line stream nil stream)))
               ((eq stream line))
             (unless (eq #\; (aref line 0))
               (let* ((string (concatenate 'string "(" line ")"))
                      (args (read-from-string string)))
                 (apply #'word :spelling args)
                 (incf counter))))
           counter)
      (close stream))))

(load-dictionary (asdf:system-relative-pathname :spell "english.txt"))
