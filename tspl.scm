;;; Chapter 2

(define square
  (lambda (n) (* n n)))

(define reciprocal
  (lambda (n)
    (if (= n 0)
      "oops!"
      (/ 1 n))))

;;; Chapter 3

;; same as let*
(define-syntax let-seq
  (syntax-rules ()
    [(_ () e) e]
    [(_ ([n v]) e) (let ([n v]) e)]
    [(_ ([n1 v1] [n2 v2]) e) (let ([n1 v1]) (let ([n2 v2]) e))]
    [(_ ([n1 v1] [n2 v2] ...) e) (let ([n1 v1]) (let-seq ([n2 v2] ...) e))]))
