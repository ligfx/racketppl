#lang racket

(provide histogram)
(require (only-in racket/format ~a))

(define (unzip seq)
  (match seq
    ['()
     (values '() '())]
    [(cons (list a b) tl)
     (define-values (as bs) (unzip tl))
     (values (cons a as) (cons b bs))]))

(define (max/seq seq) (apply max seq))

(define (left-pad min-width value)
  (~a #:min-width min-width #:align 'left value))
(define (right-pad min-width value)
  (~a #:min-width min-width #:align 'right value))

(define (histogram seq)
  (define-values (keys values) (unzip seq))
  (define maxvalue (max/seq values))
  (define normalized (for/list ([v values]) (/ v maxvalue)))
  (define maxkeylength (max/seq (map (compose string-length ~a) keys)))
  (for ([key keys] [value values] [normal normalized])
    (printf "~a: ~a (~a)\n"
            (right-pad maxkeylength key)
            (left-pad 50 (make-string (exact-ceiling (* normal 50)) #\#))
            value)))