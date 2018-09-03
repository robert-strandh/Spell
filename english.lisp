(in-package #:spell)

(defun english-lookup (word)
  (let ((decapitalized (copy-seq word)))
    (setf (aref word 0) (char-downcase (aref word 0)))
    (or (lookup word *english-dictionary*)
        (lookup decapitalized *english-dictionary*))))

(defparameter *english-dictionary*
  #.(load-dictionary (asdf:system-relative-pathname :spell "english.txt")))
