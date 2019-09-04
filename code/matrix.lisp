(in-package #:petalisp.linear-algebra)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Conversion

(defun coerce-to-matrix (x)
  (setf x (coerce-to-lazy-array x))
  (trivia:ematch (shape x)
    ;; Rank 0
    ((shape) (reshape x (τ () (1 1))))
    ;; Rank 1
    ((shape (range 1 _)) (reshape x (τ (i) (i 1))))
    ((shape (range 0 _)) (reshape x (τ (i) ((1+ i) 1))))
    ;; Rank 2
    ((shape (range 1 _) (range 1 _)) x)
    ((shape (range 0 _) (range 1 _)) (reshape x (τ (i j) ((1+ i) j))))
    ((shape (range 1 _) (range 0 _)) (reshape x (τ (i j) (i (1+ j)))))
    ((shape (range 0 _) (range 0 _)) (reshape x (τ (i j) ((1+ i) (1+ j)))))))

(defun coerce-to-scalar (x)
  (setf x (coerce-to-lazy-array x))
  (trivia:ematch (shape x)
    ((shape) x)
    ((shape (range i))
     (reshape x (make-transformation
                 :input-mask (vector i)
                 :output-rank 0)))
    ((shape (range i) (range j))
     (reshape x (make-transformation
                 :input-mask (vector i j)
                 :output-rank 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Pattern Matching

(trivia:defpattern matrix (m n)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 ,it (lazy-array-p ,it)
                    (shape ,it) (shape (range 1 ,m) (range 1 ,n)))))

(trivia:defpattern square-matrix (m)
  (alexandria:with-gensyms (g)
    `(matrix (and ,m ,g) (= ,g))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Matrix Constructors

(defun zeros (shape)
  (reshape 0 (shape shape)))

(defun eye (shape)
  (α (lambda (i j) (if (= i j) 1 0))
     (indices shape 0)
     (indices shape 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Matrix Arithmetic

(defun matrix-* (&rest matrices)
  (trivia:match matrices
    ((list)
     (coerce-to-lazy-array 1))
    ((list matrix)
     (coerce-to-matrix matrix))
    (_
     (flet ((binary-matrix-* (A B)
              (β #'+
                 (α #'*
                    (reshape (coerce-to-matrix A) (τ (m n) (n m 1)))
                    (reshape (coerce-to-matrix B) (τ (n k) (n 1 k)))))))
       (reduce #'binary-matrix-* matrices :key #'coerce-to-matrix)))))

(defun matrix-+ (&rest matrices)
  (trivia:match matrices
    ((list)
     (coerce-to-lazy-array 0))
    ((list matrix)
     (coerce-to-matrix matrix))
    (_
     (flet ((binary-matrix-+ (A B)
              (α #'+ A B)))
       (reduce #'binary-matrix-+ matrices :key #'coerce-to-matrix)))))

(defun matrix-- (matrix &rest more-matrices)
  (apply #'-
         (coerce-to-matrix matrix)
         (mapcar #'coerce-to-matrix more-matrices)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Matrix Operations

(defun transpose (x)
  (reshape
   (coerce-to-matrix x)
   (τ (m n) (n m))))

(defun dot (x y)
  (coerce-to-scalar
   (matrix-*
    (transpose x)
    (coerce-to-matrix y))))

(defun norm (x)
  (α #'sqrt (dot x x)))
