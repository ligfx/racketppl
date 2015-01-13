#!/usr/bin/env racket
#lang racket

(require racket/generic)
(require (prefix-in control: racket/control))

(require generic-bind)

(require "batteries.rkt")
(require (only-in "histogram.rkt" histogram))


(define-generics erp
  (.support erp)
  (.score erp x))

(~struct Bernoulli (p)
  #:methods gen:erp
  [(define (.support self) (list 0 1))
   (~define (.score ($Bernoulli p) x)
     (log (match x
       [1 p]
       [0 (- 1 p)])))])

(define (Binomial n p)
  (define a (sample (Bernoulli p)))
  (define b (sample (Bernoulli p)))
  (define c (sample (Bernoulli p)))
  (+ a b c))

(struct ExecutionFinished (value))
(struct ExecutionAborted (continuation erp))
(struct future (continuation score))
(define (future<= a b)
  (<= (future-score a) (future-score b)))

(define (sample erp)
  (let/cc continuation
    (control:abort (ExecutionAborted continuation erp))))
;(define-syntax (sample stx)
;  (syntax-case stx ()
;    [(sample erp)
;     (with-syntax ([name (symbol->string (gensym))])
;       #'(_sample name erp))]))

(~define (run/pp ($ (future continuation score)))
  (set! *current-score* score)
  (control:prompt (ExecutionFinished (continuation))))

(define (factor! x)
  (set! *current-score* (+ *current-score* x)))

(define *current-score* null)
(define (explore f #:samples [samples +inf.0])
  (define unexplored-futures (make-max-heap future<=))
  (heap-add! unexplored-futures (future f 0))
  (define final-values (make-counter))
  (while (and (not (heap-empty? unexplored-futures)) (> samples 0))
    (define current-future (heap-pop! unexplored-futures))
    (match (run/pp current-future)
      [(ExecutionAborted continuation erp)
        (for ([value (.support erp)])
          (heap-add! unexplored-futures (future
            (lambda () (continuation value))
            (+ *current-score* (.score erp value)))))]
      [(ExecutionFinished value)
       (set! samples (- samples 1))
       ; (printf "Sample{value=~a, score=~a}\n" value *current-score*)
       (update-counter! final-values value (exp *current-score*))]))
  (sort (normalize-sum final-values) < #:key (lambda (x) (first x))))


  (let* ([results (explore (lambda () (Binomial 10 0.4)))])
    (printf "Result of exploring `Binomial` with ~a samples:\n" +inf.0)
    (histogram results)
    (displayln ""))
  