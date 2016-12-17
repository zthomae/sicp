#lang scribble/manual

@require[scribble/lp]
@require[scribble/examples]
@require["eval.rkt"]

@title[#:version "" #:style 'toc]{Chapter 4}

@define[ev @make-eval[]]

@section[#:tag "c4e1"]{Exercise 4.1}

We can use the fact that @tt{let*} computes its results in left-to-right
order to force the order of evaluation:

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (list-of-values-ltr exp env)
   (if (no-operands? exps)
       '()
       (let* ((first (eval (first-operand exps) env))
              (rest (list-of-values (rest-operands exps) env)))
         (cons first rest))))
 
 (define (list-of-values-rtl exp env)
   (if (no-operands? exps)
       '()
       (let* ((rest (list-of-values (rest-operands exps) env))
              (first (eval (first-operand exps) env)))
         (cons first rest))))
 ]

@section[#:tag "c4e2"]{Exercise 4.2}

@tt{define} is a special form, but it is syntactically
identical to a function application. If Louis makes
@tt{eval} check whether the form matches that of a function
application before assignments, it will treat assignments
as calls to a function named @tt{define}, which is not
the correct behavior.

If, as suggested, we change the language so that all
function applications are denoted by a list whose first
element is the symbol @tt{call}, followed by the function
name and all the arguments, we would have to rewrite the
detection for function application as follows:

@verbatim{
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
}

And then @tt{eval} would look more like this:

@verbatim{
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) e nv))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))
}

@section[#:tag "c4e3"]{Exercise 4.3}

We can write the new version of @tt{eval} like this:

@verbatim{
(define (eval- exp env)
  (if (self-evaluating? exp)
      (real-exp exp)
      ((get 'eval (exp-type exp)) (real-exp exp) env)))

(define exp-type car)
(define real-exp cdr)
}

We would then install the various operations into the
table, e.g.:

@verbatim{
(put 'eval 'variable (lambda (exp env) (lookup-variable-value exp env)))
(put 'eval 'quoted (lambda (exp env) (text-of-quotation exp)))
...
}

@section[#:tag "c4e4"]{Exercise 4.4}

@tt{or} and @tt{and} can be implemented by direct evaluation in the
interpreter as follows:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (eval-and exps env)
   (define (iter rest)
     (cond ((null? rest) true)
           (((car rest)) (iter (cdr rest)))
           (else false)))
   (iter (map (lambda (exp) (lambda () (eval exp env))) exps)))
    
 (define (eval-or exps env)
   (define (iter rest)
     (cond ((null? rest) false)
           (((car rest) true))
           (else (iter (cdr rest)))))
   (iter (map (lambda (exp) (lambda () (eval exp env))) exps)))
 ]

Here, we follow the same pattern of creating a thunk evaluating to
the @tt{eval}'d form of each expression (wrapped in a thunk in order
to simulate the lazy evaluation of the arguments to these special
forms) and iterating through them as much as is necessary to find
the answer.

Alternatively, we could translate each of them into a set of nested
@tt{if} expressions:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (expand-and exps)
   (if (null? exps)
       true
       (make-if (car exps) (expand-and (cdr exps)) 'false)))
 (define (expand-or exps)
   (if (null? exps)
       false
       (make-if (car exps) 'true (expand-or (cdr exps)))))
 ]

Here, @tt{expand-and} and @tt{expand-or} are procedures which
return syntactic objects that will evaluate to the correct
result. The way that this can work is by passing the first
expression as the predicate to @tt{if}, and either resulting
in a constant value if we know the answer or an expression
computing the answer given the rest of the expressions if we
don't. The latter can be accomplished by recursively calling
the expansion procedure.

@section[#:tag "c4e5"]{Exercise 4.5}

The most natural place to add support for the alternate form
is in @tt{expand-clauses}. In the alternate case, we
reformulate the generated code to be an @tt{if} expression
wrapped in an immediately-invoked @tt{lambda} (since we
won't have @tt{let} expansion until the next exercise), so
we can reuse the computed value if it turns out to be truthy.

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (expand-clauses clauses)
   (if (null? clauses)
       'false
       (let ((first (car clauses))
             (rest (cdr clauses)))
         (cond ((cond-else-clause? first)
                (if (null? rest)
                    (sequence->exp (cond-actions first))
                    (error "ELSE clause isn't last  -- COND->IF"
                           clauses)))
               ((cond-alternate-form? first)
                (list (make-lambda '(v f) (list (make-if 'v '(f v) (expand-clauses rest))))
                      (cond-predicate first)
                      (cond-alternate-form-proc first)))
               (else
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))
 ]

We also rely on the following helper procedures:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (cond-alternate-form? clause)
   (eq? (car (cond-actions clause)) '=>))
 (define (cond-alternate-form-proc clause) (cadr (cond-actions clause)))
 ]

@section[#:tag "c4e6"]{Exercise 4.6}

We can transform @tt{let} expressions using the same pattern
as in the last problem. First, the requisite bindings:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (let? exp) (tagged-list? exp 'let))
 (define (let-bindings exp) (cadr exp))
 (define (let-body exp) (caddr exp))
 (define (let-names exp) (map car (let-bindings exp)))
 (define (let-values exp) (map cadr (let-bindings exp)))
 ]

And now, using them to construct an immediately-invoked lambda
expression:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (let->combination exp)
   (list (make-lambda (let-names exp) (list (let-body exp)))
         (let-values exp)))
]

We also need to add a line to @tt{eval}, dispatching on whether
we have a @tt{let} expression. This must come before normal
function application, as @tt{let} is a special form.

@verbatim{
...
((let? exp) (eval (let->combination exp) env))
...
}

@section[#:tag "c4e7"]{Exercise 4.7}

We can support @tt{let*} expressions by transforming them into
a series of nested @tt{let} expressions (one per binding).

@examples[
 #:eval ev #:label #f #:no-prompt
(define (make-let bindings . body)
  (append (list 'let bindings) body))

 (define (let*->nested-lets exp)
   (define (iter bindings)
     (make-let
      (list (car bindings))
      (if (null? (cdr bindings))
          (let-body exp)
          (iter (cdr bindings)))))
   (if (null? (let-bindings exp))
       (let-body exp)
       (iter (let-bindings exp))))
 ]

@section[#:tag "c4e8"]{Exercise 4.8}

In order to support the named @tt{let} form, we can check
whether the second element of the @tt{let} expression is a
list or not. If it is a list, then we are using a standard
@tt{let} expression. If it is a variable, then we have a
named @tt{let}.

First, we define a predicate for determining if an expression
(which is already assumed to be a @tt{let} expression of some
form) is a named let expression, and write new accessors
for it:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (named-let? exp) (variable? (cadr exp)))
 (define (named-let-name exp) (cadr exp))
 (define (named-let-bindings exp) (caddr exp))
 (define (named-let-parameters exp) (map car (named-let-bindings exp)))
 (define (named-let-initial-values exp) (map cadr (named-let-bindings exp)))
 (define (named-let-body exp) (cadddr exp))
 ]

Next we define the new version of @tt{let->combination},
which chooses to reduce a named let expression to a @tt{define}
(since we need to call this procedure recursively, it needs
a name, and this is how we know how to do this). I've also
defined a @tt{make-define} procedure in line with the other
@tt{make-} procedures. Note that this @tt{define} is wrapped
inside a @tt{let} with no bindings -- this is required for
the expression to be legal.

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (make-define binding val) (list 'define binding val))

 (define (let->combination exp)
   (if (named-let? exp)
       (make-let
        '()
        (make-define
         (named-let-name exp)
         (make-lambda (named-let-parameters exp) (list (named-let-body exp))))
        (cons (named-let-name exp) (named-let-initial-values exp)))
       (list (make-lambda (let-names exp) (list (let-body exp)))
             (let-values exp))))
 ]

@section[#:tag "c4e9"]{Exercise 4.9}

@bold{while}

@tt{while} is the most primitive iteration construct I've done
for this exercise, so I will define it first.

Example:

@verbatim{
(while (> x 0)
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
}

should translate to:

@verbatim{
(let ()
  (define while-iter
    (lambda (last-value)
      (if (> x 0)
          (while-iter
           (begin
             (displayln x)
             (set! x (- x 1))
             x))
          last-value)))
  (while-iter false))
}

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (while? exp) (tagged-list? exp 'while))
 (define (while-predicate exp) (cadr exp))
 (define (while-body exp) (caddr exp))

 (define (while->combination exp)
   (make-let
    '()
    (make-define 'while-iter
                 (make-lambda '(last-value)
                              (list (make-if (while-predicate exp)
                                             (list 'while-iter (while-body exp))
                                             'last-value))))
    '(while-iter false)))
 ]

@bold{until}

@tt{until} is a simple syntactic sugar around @tt{while}.

Example:

@verbatim{
(until (= x 0)
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
}

should translate to:

@verbatim{
(while (not (= x 0))
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
}

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (make-while predicate body) (list 'while predicate body))

 (define (until? exp) (tagged-list? exp 'until))
 (define (until-predicate exp) (cadr exp))
 (define (until-body exp) (caddr exp))

 (define (until->while exp)
   (make-while (list 'not (until-predicate exp)) (until-body exp)))
 ]

@bold{for}

@tt{for} is also reducible to @tt{while}, albeit in a more complex
way.

Example:

@verbatim{
(for (((x 0) (sum 0))
      (< x 5)
      (begin (set! x (+ x 1)) sum))
  (set! sum (+ sum x y)))
}

should translate to:

@verbatim{
(let ((x 0) (sum 0))
  (while (< x 5)
         (begin
           (set! sum (+ sum x y))
           (begin (set! x (+ x 1)) sum))))
}

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (for? exp) (tagged-list? exp 'for))
 (define (for-control exp) (cadr exp))
 (define (for-initialization exp) (car (for-control exp)))
 (define (for-predicate exp) (cadr (for-control exp)))
 (define (for-continue exp) (caddr (for-control exp)))
 (define (for-body exp) (caddr exp))

 (define (for->while exp)
   (make-let (for-initialization exp)
             (make-while (for-predicate exp)
                         (list 'begin (for-body exp) (for-continue exp)))))
 ]

@bold{TODO: Write explanations, and use larger example from scratch file}