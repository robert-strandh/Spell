(in-package #:spell)

(defun word (&rest arguments &key spelling &allow-other-keys)
  (declare (ignore arguments))
  (insert t spelling *dictionary*))

(defmethod %insert (object string (suffix (eql 0)) (node leaf-mixin))
  (setf (entries node) t))
