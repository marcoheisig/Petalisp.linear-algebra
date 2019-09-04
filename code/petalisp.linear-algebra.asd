(defsystem :petalisp.linear-algebra
  :description "A collection of linear algebra utilities, using Petalisp lazy arrays."
  :author "Marco Heisig <marco.heisig@fau.de>"
  :license "MIT"

  :depends-on ("trivia" "petalisp")

  :components
  ((:file "matrix")))
