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
 (define (let-body exp) (cddr exp))
 (define (let-names exp) (map car (let-bindings exp)))
 (define (let-values exp) (map cadr (let-bindings exp)))
 ]

And now, using them to construct an immediately-invoked lambda
expression:

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (let->combination exp)
   (list (make-lambda (let-names exp) (let-body exp))
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

It is fully legal to rewrite the @tt{let*} expression into
a series of nested @tt{let} expressions and add a rule
to @tt{eval} that will evaluate this, as in @tt{(eval (let*->nested-lets exp) env)}.
All that will happen is that @tt{eval} will be indirected
once more before ultimately simplifying to non-derived expressions
and evalauting them. This is no different than how @tt{let}
expressions are handled now.

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
         (make-lambda (named-let-parameters exp) (named-let-body exp)))
        (cons (named-let-name exp) (named-let-initial-values exp)))
       (list (make-lambda (let-names exp) (let-body exp))
             (let-values exp))))
 ]

@section[#:tag "c4e9"]{Exercise 4.9}

@bold{while}

@tt{while} is the most primitive iteration construct I've done
for this exercise, so I will define it first.

A @tt{while} expression has two parts:

@itemlist[
 @item{A predicate, determining whether to execute the body or not}
 @item{A body expression that is evaluated multiple times}
 ]

I will allow for the expression to return a value (which will be
what the body evaluated to in the last time it was evaluated), but
the primary purpose of a @tt{while} loop (and of all of the
iteration constructs we are defining here) is to do mutations.

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

@tt{until} is a simple syntactic sugar around @tt{while} where
the predicate is negated. Therefore, we can translate it directly
to a @tt{while} expression.

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
way. The @tt{for} loop contains a body as well as a sequence
of three control expressions:

@itemlist[
 @item{A sequence of initializing bindings creating scoped definitions
       with initial values}
 @item{A predicate determining whether to evaluate the body}
 @item{A continuing expression to be evaluated after the body, before
       testing the predicate again}
 ]

The @tt{for} loop is introduced to account for the pattern
seen above, where a value is incremented or decremented at
the end of a @tt{while} loop body. It is easy to write a
@tt{while} loop and forget this, accidentally leaving yourself
with an infinite loop; furthermore, for certain kinds of
iterations, it is likely the case that the @tt{for} loop
demonstrates intent more precisely, since only some kinds
of iterations make more sense when described in this way.

A @tt{for} loop can be reduced to a @tt{while} whose body
is comprised of the @tt{for} loop body followed by the
@tt{for} loop continuing expressions and whose predicate
is the same, all inside a @tt{let} expression creating
the bindings of the @tt{for} loop's initialization sequence.

Like a @tt{while} expression, the @tt{for} expression also
results in a value even though it is mostly intended for
side-effectful computing. Since the @tt{while} loop evaluates
to the last execution of the body, since the last-executed
expression in the body is the continuing expression, the
last evaluation of the continuing expression of the @tt{for}
expression is the final result. I will be using that below
to avoid having to define bindings outside of the @tt{for}
expression in my examples, but the value of the @tt{for}
expression is not its main purpose.

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

@bold{A longer example}

Consider the nested @tt{for} expression below:

@verbatim{
(for (((x 0) (sum 0))
       (< x 5)
       (begin (set! x (+ x 1)) sum))
  (for (((y 0))
         (< y 5)
         (begin (set! y (+ y 1)) y))
    (set! sum (+ sum x y))))
}

Due to how we have constructed the continuing expressions,
the end result of this expression should be the final
@tt{sum}.

If we reduce both @tt{for} expressions to @tt{while}
expressions, we end up with

@verbatim{
(let ((x 0) (sum 0))
  (while (< x 5)
         (begin
           (let ((y 0))
             (while (< y 5)
                    (begin
                      (set! sum (+ sum x y))
                      (begin (set! y (+ y 1)) y))))
           (begin (set! x (+ x 1)) sum))))
}

And if we reduce both of these @tt{while} expressions,
we end up with

@verbatim{
(let ((x 0) (sum 0))
  (let ()
    (define while-iter
      (lambda (last-value)
        (if (< x 5)
            (while-iter
             (begin
               (let ((y 0))
                 (let ()
                   (define while-iter
                     (lambda (last-value)
                       (if (< y 5)
                           (while-iter
                            (begin
                              (set! sum (+ sum x y))
                              (begin (set! y (+ y 1)) y)))
                           last-value)))
                   (while-iter false)))
               (begin
                 (set! x (+ x 1))
                 sum)))
            last-value)))
    (while-iter false)))
}

We can actually evaluate that and see that we get
the correct answer:

@examples[
 #:eval ev #:label #f
 (let ((x 0) (sum 0))
   (let ()
     (define while-iter
       (lambda (last-value)
         (if (< x 5)
             (while-iter
              (begin
                (let ((y 0))
                  (let ()
                    (define while-iter
                      (lambda (last-value)
                        (if (< y 5)
                            (while-iter
                             (begin
                               (set! sum (+ sum x y))
                               (begin (set! y (+ y 1)) y)))
                            last-value)))
                    (while-iter false)))
                (begin
                  (set! x (+ x 1))
                  sum)))
             last-value)))
     (while-iter false)))
 ]

One may argue that these looping constructs are not
necessary. However, if you observe the difference in the
clarity of the @tt{for} expressions compared to what is
ultimately evaluated, it is clear that there is, in fact, a
use for these. That said, I have committed a sleight of hand
here, as there is a much more straightforward solution to
the given problem. Consider what the sum actually is:

@verbatim{
  (0 + 0) + (0 + 1) + (0 + 2) + (0 + 3) + (0 + 4)
+ (1 + 0) + (1 + 1) + (1 + 2) + (1 + 3) + (1 + 4)
+ (2 + 0) + (2 + 1) + (2 + 2) + (2 + 3) + (2 + 4)
+ (3 + 0) + (3 + 1) + (3 + 2) + (3 + 3) + (3 + 4)
+ (4 + 0) + (4 + 1) + (4 + 2) + (4 + 3) + (4 + 4)
}

It is clear that each value is added to the sum
@tt{10} times, and thus we can trivially compute
it with

@examples[
 #:eval ev #:label #f
 (* 10 (+ 1 2 3 4))
 ]

It is true that this is only because the actual work being
done by the above @tt{for} expression is trivial and
actually not taking advantage of the main power of the
construct -- that is, concisely being able to describe
iterative side effects. But this wouldn't be the first time
that an imperative construct was used when a simpler option
exists. This is why it's important to use these iterative
constructs for what they are actually best for --
side-effectful computing that can't be concisely stated as a
straightforward evaluation.

@section[#:tag "c4e10"]{Exercise 4.10}

@bold{I'm going to come back to this one when I have a fun idea}

@section[#:tag "c4e11"]{Exercise 4.11}

One of the reasons that the approach using separate lists for
variables and values in the environment worked so nicely is
that a natural representation containing pointers to these
lists fell out of it. If we imagine the bindings instead as
a single list of binding pairs (where the value can itself
also be a list), our data representation seems more elegant,
but we lose that level of indirection that made the mutations
easy to write. To deal with this, we can instead define a
frame as having a @tt{head} element in front, such that the
@tt{cdr} of this frame is a reliable pointer to the list
of bindings.

@examples[
 #:eval ev #:label #f #:no-prompt
 (define the-empty-environment-2 '(head))

 (define (lookup-variable-value-2 var env)
   (define (env-loop env)
     (define (scan bindings)
       (cond ((null? bindings) (env-loop (enclosing-environment env)))
             ((eq? var (caar bindings)) (cdar bindings))
             (else (scan (cdr bindings)))))
     (if (eq? env the-empty-environment-2)
         (error "Unbound variable" var)
         (let ((frame (first-frame env)))
           (scan (cdr frame)))))
   (env-loop env))

 (define (extend-environment-2 bindings base-env)
   (cons (cons 'head bindings) base-env))

 (define (add-binding-to-frame-2! var val frame)
   (if (null? (cdr frame))
       (set-cdr! frame (cons var val))
       (begin
         (set-cdr! (cdr frame) (cons (cadr frame) (cddr frame)))
         (set-car! (cdr frame) (cons var val)))))

 (define (define-variable-2! var val env)
   (let ((frame (first-frame env)))
     (define (scan bindings)
       (cond ((null? bindings)
              (add-binding-to-frame-2! var val frame))
             ((eq? var (caar bindings))
              (set-car! (car bindings) val))
             (else (scan (cdr bindings)))))
     (scan (cdr frame))))

 (define (set-variable-2! var val env)
   (define (env-loop env)
     (define (scan bindings)
       (cond ((null? bindings)
              (env-loop (enclosing-environment env)))
             ((eq? var (caar bindings))
              (set-cdr! (car bindings) val))
             (else (scan (cdr bindings)))))
     (if (eq? env the-empty-environment-2)
         (error "Unbound variable -- SET!" var)
         (let ((frame (first-frame env)))
           (scan (cdr frame)))))
   (env-loop env))
]

@section[#:tag "c4e12"]{Exercise 4.12}

If we generalize @tt{lookup-variable-value}, @tt{set-variable-value!},
and @tt{define-variable!} fully, we can see that the common pattern
in each is that we loop through one frame in the environment, running
one procedure if we find a binding with a given variable name, and
running a different procedure if we reach the end of the current frame
without finding it. In the case of @tt{lookup-variable-value} and
@tt{set-variable-value!}, the latter procedure is the same iterative
loop, but it doesn't need to be so.

Based on the behaviors of these procedures, we can (somewhat
arbitrarily) decide that the procedure executed if we find a
binding should take the lists of variables and values as its
parameters, and that the procedure to be performed if the
frame doesn't contain the binding we're looking for only
needs to take the current environment.

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (search-env if-found if-not-found)
   (lambda (var env)
     (define (scan vars vals)
       (cond ((null? vars) (if-not-found env))
             ((eq? var (car vars)) (if-found vars vals))
             (else (scan (cdr vars) (cdr vals)))))
     (if (eq? env the-empty-environment)
         (error "Unbound variable" var)
         (let ((frame (first-frame env)))
           (scan (frame-variables frame)
                 (frame-values frame))))))

 (define (lookup-variable-value-alt val env)
   ((search-env
     (lambda (vars vals) (car vals))
     (lambda (env) (lookup-variable-value-alt val (enclosing-environment env))))
    val env))

 (define (set-variable-value-alt! var val env)
   ((search-env
     (lambda (vars vals) (set-car! vals val))
     (lambda (env) (set-variable-value-alt! var val (enclosing-environment env))))
    var env))

 (define (define-variable!-alt var val env)
   ((search-env
     (lambda (vars vals) (set-car! vals val))
     (lambda (env) (add-binding-to-frame! var val (first-frame env))))
    var env))
 ]

This arguably makes it clearer what the semantics of these
procedures are -- in particular, whether they search beyond
the first frame of the environment or not.

@section[#:tag "c4e13"]{Exercise 4.13}

I believe that unbinding variables is already a bad idea,
and would not choose to include the feature. I am also
inclined to believe that allowing variables in parent
environments to be unbound is an even more dangerous idea --
imagine if a binding in a closure was destroyed somewhere
far away in a way that you didn't expect. If you were
relying on this behavior, it may be very tricky to discover
what exactly has gone wrong.

On the other hand, we need to be careful with restricting
functionality simply because we believe it is dangerous.
Even it actually being dangerous is not necessarily an
argument for disallowing it. There is a tradeoff between
preventing the unexpected or unwanted and allowing the
unforeseen but necessary.

For now, I'm going to make the decision to only allow
unbinding variables in the current frame, even though I have
misgivings about it.

There is a bit of subtlety in defining @tt{make-unbound!},
because we need to modify @tt{vars} and @tt{vals} such that
the binding is question is  no longer present. This is easy
to do if the list has more than one element in it, but if
there is only one then we are stuck -- no use of @tt{set-car!}
and @tt{set-cdr!} can turn this actual pair into @tt{nil}.
So, we instead perform iterate through such that the current
variable is the second in the list, and seed the initial
lists with dummy head elements to allow for all of the
bindings to be deleted. In other words, the base case is now
a one-element list, not an empty one. Unfortunately, this
means we can't use @tt{search-env} from the last exercise.

@examples[
 #:eval ev #:label #f #:no-prompt
 (define (make-unbound! var env)
   (define (scan vars vals)
     (cond ((null? (cdr vars))
            (error "Cannot unbind non-bound variable" var))
           ((eq? var (cadr vars))
            (begin
              (set-cdr! vars (cddr vars))
              (set-cdr! vals (cddr vals))))
           (else (scan (cdr vars) (cdr vals)))))
   (if (eq? env the-empty-environment)
       (error "Cannot unbind non-bound variable" var)
       (let ((frame (first-frame env)))
         (scan (cons 'vars (frame-variables frame))
               (cons 'vals (frame-values frame))))))
 ]

@section[#:tag "c4e14"]{Exercise 4.14}

The reason that @tt{map} can't be installed as a primitive
is that the primitive implementation requires the function
being mapped with to be pure procedures as they would be
represented in the underlying Scheme. Neither our primitive
nor our compound procedures meet this criteria -- the
compount procedures use entirely our own representation,
while primitive procedures are tagged.
