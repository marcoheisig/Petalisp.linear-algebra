(defpackage #:petalisp.linear-algebra
  (:use :common-lisp :petalisp)
  (:export
   #:coerce-to-matrix
   #:coerce-to-scalar
   #:matrix
   #:square-matrix
   #:zeros
   #:eye
   #:matrix-*
   #:matrix-+
   #:matrix--
   #:transpose
   #:dot
   #:norm))
