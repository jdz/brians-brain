(asdf:defsystem :brians-brain
    :version "1.0"
    :author "Jānis Džeriņš"
    :license "Do whatever you want with this."
    :depends-on (cl-opengl cl-glut)
    :components ((:file "package")
                 (:file "brain" :depends-on ("package"))
                 (:file "display" :depends-on ("package" "brain"))))
