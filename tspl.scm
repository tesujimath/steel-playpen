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
    [(_ ([n1 v1] [n2 v2] ...) e) (let ([n1 v1]) (let-seq ([n2 v2] ...) e))]))

;; not tail call, also terribly inefficient
(define fibonacci-bad
  (lambda (n)
    (let fib ([i n])
      (cond
        [(= i 1) 1]
        [else (+ (fib (- i 1)) (fib (- i 2)))]))))

;; tail call
(define fibonacci
  (lambda (n)
    (let fib ([i n] [a1 1] [a2 0])
      (cond
        [(= i 1) a1]
        [else (fib (- i 1) (+ a1 a2) a1)]))))
