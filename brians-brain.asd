(asdf:defsystem :brians-brain
  :version "1.0"
  :author "Jānis Džeriņš"
  :license "Do whatever you want with this."
  :description "Common Lisp implementation of Brian's Brain"
  :long-description "
Brian's brain (http://en.wikipedia.org/wiki/Brian's_Brain)
implementation in Common Lisp, as described in http://t-b-o-g.blogspot.com/."
  :depends-on (cl-opengl cl-glut)
  :components ((:file "package")
               (:file "brain" :depends-on ("package"))
               (:file "display" :depends-on ("package" "brain"))))
