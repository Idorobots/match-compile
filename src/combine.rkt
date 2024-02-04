#lang racket

(provide compile-combine)

(define (node-type n)
  (car n))

(define (node-param n)
  (cadr n))

(define (node-children n)
  (if (eq? (node-type n) 'fail)
      '()
      (caddr n)))

(define (match-fail)
  (list 'fail))

(define (action action)
  (list 'action action))

(define (match-wildcard then)
  (list 'wildcard then))

(define (match-binding variable then)
  (list 'binding variable then))

(define (match-value value then)
  (list 'value value then))

(define (match-type type then)
  (list 'type type then))

(define (access a then)
  (list 'access a then))

(define (switch-type cases)
  (list 'switch-type cases))

(define (switch-value cases)
  (list 'switch-value cases))

(define (ordered cases)
  (list 'ordered cases))

(define (compile-case pattern cont level)
  ;; FIXME The level is only needed to consistently combine the paths at a later phase.
  ;; FIXME This probably can be done better by introducing explicit nodes for constructors and handling those later.
  (cond ((eq? pattern '_)
         (match-wildcard cont))
        ((symbol? pattern)
         (match-binding pattern cont))
         ;; NOTE These should alawys produce a type & value match for each step, to make combining paths easier.
        ((number? pattern)
         (match-type 'number?
                     (match-value pattern cont)))
        ((boolean? pattern)
         (match-type 'boolean?
                     (match-value pattern cont)))
        ((string? pattern)
         (match-type 'string?
                     (match-value pattern cont)))
        ((null? pattern)
         (match-type 'null?
                     (match-value pattern cont)))
        ((pair? pattern)
         (cond ((eq? (car pattern) 'quote)
                (match-value (cadr pattern) cont))
               ((eq? (car pattern) ':)
                (match-type (cadr pattern)
                            (compile-case (caddr pattern)
                                          cont
                                          (+ 1 level))))
               (else
                (match-type 'pair?
                            ;; FIXME A workaround for the cdr down the line to have the full node.
                            (let ((e (string->symbol (format "tmp-par-binding-~a" level))))
                              (match-binding e
                                             (access `(car ,e)
                                                     (compile-case (car pattern)
                                                                   (access `(cdr ,e)
                                                                           (compile-case (cdr pattern)
                                                                                         cont
                                                                                         (+ 1 level)))
                                                                   (+ 1 level)))))))))
        (else
         (match-fail))))

(define (combine-paths cases)
  (define (combine-group group)
    (let ((t (node-type (car group)))
          (by-param (group-by node-param group)))
      (case t
        ((action wildcard)
         (if (> (length group) 1)
             (error "Redundant match cases:" group)
             (car group)))
        ((binding)
         (if (> (length by-param) 1)
             (error "Redundant match cases:" group)
             (match-binding (node-param (car group))
                            (combine-paths (map node-children group)))))
        ((access)
         (if (> (length by-param) 1)
             (error "This shouldn't happen, sigh..." group)
             (access (node-param (car group))
                     (combine-paths (map node-children group)))))
        ((type)
         (switch-type (map (lambda (g)
                             (list (node-param (car g))
                                   (combine-paths (map node-children g))))
                           by-param)))
        ((value)
         (switch-value (map (lambda (g)
                              (list (node-param (car g))
                                    (combine-paths (map node-children g))))
                            by-param)))
        (else
         (error "Bad group:" group)))))
  (let* ((by-type (group-by node-type cases))
         (grouped (map combine-group by-type)))
    (if (> (length grouped) 1)
        (ordered grouped)
        (car grouped))))

(define (compile-decision-tree value node)
  (case (node-type node)
    ((fail)
     '(raise "Match error"))
    ((action)
     (cadr node))
    ((wildcard)
     (compile-decision-tree value (cadr node)))
    ((binding)
     (let ((name (node-param node)))
       `(let ((,name ,value))
          ,(compile-decision-tree name (node-children node)))))
    ((access)
     (let ((name (gensym 'v)))
       ;; FIXME Ignoring the value as the path-compiler has already embedded the temporary value name in the nodes.
       `(let ((,name ,(node-param node)))
          ,(compile-decision-tree name (node-children node)))))
    ((value)
     `(if (equal? ,value ',(node-param node))
          ,(compile-decision-tree value (node-children node))
          (raise "Match error")))
    ((type)
     `(if (,(node-param node) ,value)
          ,(compile-decision-tree value (node-children node))
          (raise "Match error")))
    ((switch-value)
     (foldr (lambda (c acc)
              `(if (equal? ,value ',(car c))
                   ,(compile-decision-tree value (cadr c))
                   ,acc))
            '(raise "Match error")
            (cadr node)))
    ((switch-type)
     (foldr (lambda (c acc)
              `(if (,(car c) ,value)
                   ,(compile-decision-tree value (cadr c))
                   ,acc))
            '(raise "Match error")
            (cadr node)))
    ((ordered)
     (foldr (lambda (c acc)
              ;; FIXME This should check for a very specific error to be generated or, ideally, not rely on exceptions.
              `(handle ,(compile-decision-tree value c)
                       (lambda (_)
                         ,acc)))
            '(raise "Match error")
            (cadr node)))
    (else
     (error "Bad node: " node))))

(define (compile-combine value cases)
  (let* ((compiled (map (lambda (c)
                                 (compile-case (car c)
                                               (action (cadr c))
                                               0))
                               cases))
         (combined (combine-paths compiled))
         (translated (compile-decision-tree value combined)))
    translated))

;; Example
;; (compile-combine
;;  'foo
;;  '(((1 2 ?rest) (match-list ?rest))
;;    ((1 _ '(hurr)) (match-quote))
;;    ((1 2) (match-shortlist))
;;    (#t (match-boolean ?x))
;;    (42 (match-number ?y))
;;    ((: number? _) (match-any-number))
;;    (_ 'default-action)))
