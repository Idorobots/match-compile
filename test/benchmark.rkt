#lang racket

(require "../src/utils.rkt")
(require (for-syntax "../src/combine.rkt"))

(define (run-racket v)
  (with-handlers
      ((any? (lambda (e) e)))
    (match v
      ((list 1 2 rest ...) rest)
      ((list 1 _ 'hurr) 'hurr)
      ((list 1 2 3) 123)
      ((list 1 (list 2 3 4) 5) 234)
      (#t 'boolean)
      ((list 1 2 "string") "text")
      (42 'number)
      ("string again" 23)
      ((list 1 42 3) 'nope)
      (value #:when (string? value) (string-length value))
      (_ 'default))))

(define-syntax (match-combine stx)
  (syntax-case stx ()
    ((match-combine expr cases ...)
     (datum->syntax stx
                    (compile-combine (syntax->datum #'expr)
                                     (syntax->datum #'(cases ...)))))))

(define (run-combine v)
  (with-handlers
      ((any? (lambda (e) e)))
    (match-combine v
                   ((1 2 . rest) rest)
                   ((1 _ 'hurr) 'hurr)
                   ((1 2 3) 123)
                   ((1 (2 3 4) 5) 234)
                   (#t 'boolean)
                   ((1 2 "string") "text")
                   (42 'number)
                   ("string again" 23)
                   ((1 42 3) 'nope)
                   ((: string? value) (string-length value))
                   (_ 'default)
                   )))

(define (iota from to)
  (if (> from to)
      '()
      (cons from
            (iota (+ 1 from) to))))

(define options
  '(1
    2
    3
    42
    50
   (1)
   (1 2)
   (1 2 3)
   (1 2 3 4)
   (1 2 3 4 5)
   #f
   #t
   "string"
   "another string"
   "string again"
   (1 "string" 2)
   (1 2 "string")
   (1 5 (hurr))
   (1 3 hurr)
   (1 2 durr)
   (1 4 herp)
   (1 (2 herp) derp)
   (1 (2 herp) hurr)
   (1 (2 3 4) 5)
   ((2 3 4) 5)
   (1 (2 3 4))
   (1 (2 4 3) 5)))

(define test-input
  (let ((n (length options)))
    (map (lambda (i)
           (list-ref options (random n)))
         (iota 0 1000000))))

(display "Racket match:  ")
(define racket-result (time (map run-racket test-input)))

(display "Combine match: ")
(define combine-result (time (map run-combine test-input)))
(unless (equal? racket-result combine-result)
  (displayln "Not equal!")
  (displayln test-input)
  (displayln racket-result)
  (displayln combine-result))
