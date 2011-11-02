(defgeneric lookup (string dictionary))
(defgeneric insert (object string dictionary))

(defclass node () ())

(defclass dictionary ()
  ((%contents :initform (make-instance 'node) :accessor contents)))

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

(defclass interior-node (interior-mixin) ())

(defmethod %lookup (string (suffix (eql 0)) (node interior-node))
  '())

(defmethod %lookup (string suffix (node interior-mixin))
  (let ((child (find-child (aref string (- (length string) suffix))
			   (children node))))
    (if (null child)
	nil
	(%lookup string (1- suffix) child))))

(defclass leaf-node (leaf-mixin) ())
(defclass interior-leaf-node (interior-mixin leaf-mixin) ())

(defgeneric %insert (object string suffix dictionary))

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
(defgeneric add-child (node char entries))

(defmethod find-child (char (entries list))
  (cdr (assoc char entries)))

(defmethod find-child (char (entries vector))
  (aref entries (- (char-code char) #.(char-code #\a))))

(defmethod add-child (node char (entries list))
  (acons char node entries))

(defmethod add-child (node char (entries vector))
  (setf (aref entries (- (char-code char) #.(char-code #\a)))
	node))

(defparameter *word-types* (make-hash-table :test #'eq))

(defmacro defword (class-name &body body)
  (let ((type (intern (symbol-name class-name) :keyword)))
    `(progn (setf (gethash ,type *word-types*) ',class-name)
	    (defclass ,class-name ,@body))))

(defword word ()
  ((%spelling :initarg :spelling :reader spelling)
   (%base :initarg :base :reader base)))

(defword noun (word)
  ((%number :initarg :number :reader %number)
   (%case :initarg :case :initform :nominative :reader %case)
   (%gender :initarg :gender :initform :any :reader gender)
   (%singular :initarg :singular :reader singular)))
	  
(defword proper-noun (noun) ())

(defword negative-mixin ()
  ((%negative :initform nil :initarg :negative :reader negative)))

(defword contraction-mixin ()
  ((%contraction :initform nil :initarg :contraction :reader contraction)))

(defword verb (word negative-mixin contraction-mixin)
  ((%person :initform :any :initarg :person :reader person)
   (%number :initform :any :initarg :number :reader %number)
   (%tense :initarg :tense :reader tense)
   (%mood :initarg :mood :reader mood)
   (%negative :initarg :negative :initform nil :reader negative)
   (%contraction :initarg :contraction :initform nil :reader contraction)
   (%strength :initarg :strength :initform :weak :reader strength)
   (%infinitive :initarg :infinitive :reader infinitive)))

(defword preposition (word) ())

(defword adjective (word)
  ((%degree :initarg :degree :reader degree)))

(defword adverb (word) ())

(defword pronoun (word negative-mixin)
  ((%person :initarg :person :reader person)
   (%number :initarg :number :reader %number)
   (%gender :initarg :gender :reader gender)
   (%case :initarg :case :initform :nominative :reader %case)))

(defword personal-pronoun (pronoun) ())

(defword possessive-pronoun (pronoun)
  ((%refnumber :initform :any :initarg :refnumber :reader refnumber)))

(defword reflexive-pronoun (pronoun) ())

(defword demonstrative-pronoun (pronoun) ())

(defword interjection (word) ())

(defword conjunction (word) ())

(defword subordinate (conjunction) ())

(defword determiner (word)
  ((%number :initform :any :initarg :number :reader %number)))

(defword article (determiner)
  ((%number :initarg :number :reader %number)
   (%determinate :initform nil :initarg :determinate :reader determinate)))

(defword quantifier (determiner) ())

(defword possessive-adjective (determiner)
  ((%gender :initform :any :initarg :gender :reader gender)
   (%person :initarg :person :reader person)
   (%refnumber :initform :any :initarg :refnumber :reader refnumber)))

(defword demonstrative-adjective (determiner) ())  

(defword interrogative-adjective (determiner) ())

(defword noun-verb-contraction (noun verb) ())  

(defword verb-verb-contraction (verb) ())  

(defparameter *english-dictionary* (make-instance 'dictionary))

(defun word (&rest arguments &key type spelling &allow-other-keys)
  (insert (apply #'make-instance
		 (gethash type *word-types*)
		 (progn (remf arguments :type) arguments))
	  spelling
	  *english-dictionary*))
