(in-package #:spell)

(defparameter *english-dictionary*
  #.(load-dictionary (asdf:system-relative-pathname :spell "english.txt")))

(defun english-lookup (word)
  (when (and word (string/= word ""))
    (let ((decapitalized (copy-seq word)))
      (setf (aref word 0) (char-downcase (aref word 0)))
      (or (lookup word *english-dictionary*)
          (lookup decapitalized *english-dictionary*)))))
