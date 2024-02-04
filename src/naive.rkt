#lang racket

(provide compile-naive)

(define (compile-case value pattern cont)
  (cond ((eq? pattern '_)
         cont)
        ((symbol? pattern)
         `(let ((,pattern ,value))
            ,cont))
        ((or (number? pattern)
             (boolean? pattern)
             (string? pattern)
             (null? pattern))
         `(if (equal? ,value ',pattern)
              ,cont
              (raise "Match error")))
        ((and (pair? pattern)
              (eq? (car pattern) 'quote))
         `(if (equal? ,value ',(cadr pattern))
              ,cont
              (raise "Match error")))
        ((and (pair? pattern)
              (eq? (car pattern) ':))
         `(if (,(cadr pattern) ,value)
              ,(compile-case value
                             (caddr pattern)
                             cont)
              (raise "Match error")))
        ((pair? pattern)
         `(if (pair? ,value)
              ,(compile-case `(car ,value)
                             (car pattern)
                             (compile-case `(cdr ,value)
                                           (cdr pattern)
                                           cont))
              (raise "Match error")))
        (else
         (raise "Match error"))))

(define (compile-naive value cases)
  (foldr (lambda (c acc)
           `(handle ,(compile-case value (car c) (cadr c))
                    (lambda (_)
                      ,acc)))
         '(raise "Match error")
         cases))

;; Example
;; (compile-naive
;;  'foo
;;  '(((1 2 ?rest) (match-list ?rest))
;;    ((1 _ '(hurr)) (match-quote))
;;    ((1 2) (match-shortlist))
;;    (#t (match-boolean ?x))
;;    (42 (match-number ?y))
;;    ((: number? _) (match-any-number))
;;    (_ 'default-action)))
