(in-package #:spell)

(eval-when (:compile-toplevel :load-toplevel :execute)
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
        (values *dictionary* counter)))))

(defun lookup (word &optional (dictionary *english-dictionary*))
  (%lookup word dictionary))

(defparameter *english-dictionary*
  #.(load-dictionary (asdf:system-relative-pathname :spell "english.txt")))
