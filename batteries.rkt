#lang racket

;
; A data structure for counting things
;
(provide make-counter normalize-sum update-counter!)
(define (sum lst) (apply + lst))
(define make-counter make-hash)
(define-syntax-rule (update-counter! counter x n)
  (dict-set! counter x (+ n (dict-ref counter x 0))))
(define (normalize-sum counter)
  (define norm (sum (dict-values counter)))
  (for/list ([(key value) (in-dict counter)] #:when (not (= 0 value)))
    (list key (/ value norm))))

;
; Helper functions for working with max-heaps
;
(provide heap-empty? heap-pop! make-max-heap heap-add!)
(require data/heap)
(define (make-max-heap <=? [init '()])
  (define >? (lambda (a b) (not (<=? a b))))
  (define new (make-heap >?))
  (heap-add-all! new init)
  new)
(define-syntax-rule (heap-pop! heap)
  (let* ([min (heap-min heap)])
    (heap-remove-min! heap)
    min))
(define (heap-empty? heap)
  (= 0 (heap-count heap)))

;
; A `while` form, because Racket doesn't come with one
;
(provide while)
(define-syntax-rule (while test body ...)
  (local [(define (loop)
            (when test
              body ...
              (loop)))]
    (loop)))

