;;;; spell.asd

(asdf:defsystem #:spell
  :description "Spellchecking package for Common Lisp"
  :author "Robert Strandh <robert.strandh@gmail.com>
Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "spell")
               (:file "word")
               (:file "english")))

(asdf:defsystem #:spell/simple
  :description "Spellchecking package for Common Lisp - simple version"
  :author "Robert Strandh <robert.strandh@gmail.com>
Michał \"phoe\" Herda <phoe@disroot.org>"
  :license "BSD"
  :version "0.0.1"
  :serial t
  :components ((:file "spell")
               (:file "simple")
               (:file "english")))
