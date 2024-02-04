#lang racket

(provide (all-defined-out))

(define-syntax handle
  (syntax-rules ()
    ((handle expr handler)
     (with-handlers (((lambda (e) #t)
                      handler))
       expr))))

(define (any? v)
  #t)
