;; SRFI 64/(chibi test) shim.

(define-syntax test-equal
  (syntax-rules ()
    ((test-equal comp expected expr)
     (test-assert (comp expected expr)))))

(define-syntax test-values
  (syntax-rules ()
    ((test-values expected expr)
     (let-values ((as expected) (bs expr))
       (test as bs)))))

(define-syntax test-not
  (syntax-rules ()
    ((test-not expr)
     (test-assert (not expr)))))

(include "test.scm")
