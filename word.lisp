(in-package #:spell)

(defparameter *word-types* (make-hash-table :test #'eq))

(defclass word ()
  ((%base :initarg :base :reader base)))

(defmethod initialize-instance :after ((object word) &key spelling)
  (declare (ignore spelling)))

(defmethod make-load-form ((object word) &optional environment)
  (make-load-form-saving-slots object :environment environment))

(defmacro defword (class-name &body body)
  (let ((type (intern (symbol-name class-name) :keyword)))
    `(progn (setf (gethash ,type *word-types*) ',class-name)
            (defclass ,class-name ,@body))))

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

(defun word (&rest arguments &key type spelling &allow-other-keys)
  (let ((arguments (copy-list arguments)))
    (remf arguments :type)
    (insert (apply #'make-instance (gethash type *word-types*) arguments)
            spelling
            *dictionary*)))
