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

@racketblock[
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
]

And then @tt{eval} would look more like this:

@racketblock[
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
]

@section[#:tag "c4e3"]{Exercise 4.3}

We can write the new version of @tt{eval} like this:

@racketblock[
(define (eval- exp env)
  (if (self-evaluating? exp)
      (real-exp exp)
      ((get 'eval (exp-type exp)) (real-exp exp) env)))

(define exp-type car)
(define real-exp cdr)
]

We would then install the various operations into the
table, e.g.:

@racketblock[
(put 'eval 'variable (lambda (exp env) (lookup-variable-value exp env)))
(put 'eval 'quoted (lambda (exp env) (text-of-quotation exp)))
...
]

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

@racketblock[
(while (> x 0)
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
]

should translate to:

@racketblock[
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
]

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

@racketblock[
(until (= x 0)
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
]

should translate to:

@racketblock[
(while (not (= x 0))
       (begin
         (displayln x)
         (set! x (- x 1))
         x))
]

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

@racketblock[
(for (((x 0) (sum 0))
      (< x 5)
      (begin (set! x (+ x 1)) sum))
  (set! sum (+ sum x y)))
]

should translate to:

@racketblock[
(let ((x 0) (sum 0))
  (while (< x 5)
         (begin
           (set! sum (+ sum x y))
           (begin (set! x (+ x 1)) sum))))
]

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

@racketblock[
(for (((x 0) (sum 0))
       (< x 5)
       (begin (set! x (+ x 1)) sum))
  (for (((y 0))
         (< y 5)
         (begin (set! y (+ y 1)) y))
    (set! sum (+ sum x y))))
]

Due to how we have constructed the continuing expressions,
the end result of this expression should be the final
@tt{sum}.

If we reduce both @tt{for} expressions to @tt{while}
expressions, we end up with

@racketblock[
(let ((x 0) (sum 0))
  (while (< x 5)
         (begin
           (let ((y 0))
             (while (< y 5)
                    (begin
                      (set! sum (+ sum x y))
                      (begin (set! y (+ y 1)) y))))
           (begin (set! x (+ x 1)) sum))))
]

And if we reduce both of these @tt{while} expressions,
we end up with

@racketblock[
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

@section[#:tag "c4e15"]{Exercise 4.15}

Suppose we have a procedure @tt{halts} which determines
whether the one-argument procedure @tt{p} terminates when
given the input @tt{a}. Suppose we then had the given example
program:

@racketblock[
(define (run-forever) (run-forever))

(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))
]

Now suppose we do as they suggest and evaluate @tt{(try try)}.
This will either call @tt{run-forever} if @tt{(halts? try try)}
returns true -- that is, if @tt{(try try)} does not run forever --
or will halt if it does run forever. This is a contradiction, and
guarantees that implementing @tt{halts?} is impossible.

@section[#:tag "c4e16"]{Exercise 4.16}

@tt{lookup-variable-value} should be modified to look like
this (modified to check the value after the binding has been
found, only returning successfully if it is assigned):

@racketblock[
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val '*unassigned*)
                   (error "Use of unassigned variable" var)
                   val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))
]

The basic strategy for defining @tt{scan-out-defines} will
be as follows:

@itemlist[
@item{Grab all of the definition at the start of the
      procedure body. (It is illegal to have internal definitions
      after any non-definition expressions)}
@item{If there are no internal definitions, return the body}
@item{Zip each of the variables of the internal definition in a list with @tt{'*unassigned*}}
@item{Create @tt{set!} expressions for each of the definitions}
@item{Make a @tt{let} expression with our bindings and the @tt{set!} expressions
      appended with the original body}
]

To do this, we will first define helper procedures @tt{partition} and @tt{make-set}:

@racketblock[
(define (partition pred lst)
  (define (go acc rest)
    (if (null? rest)
        (cons (reverse acc) '())
        (let ((x (car rest))
              (xs (cdr rest)))
          (if (pred x)
              (go (cons x acc) xs)
              (cons (reverse acc) rest)))))
  (go '() lst))

(define (make-set var val)
  (list 'set! var val))
]

The definition of @tt{scan-out-defines} is as follows:

@racketblock[
(define (scan-out-defines procedure-body)
  (let* ((define-partition (partition definition? procedure-body))
         (defines (car define-partition))
         (body (cdr define-partition)))
    (if (null? defines)
        body
        (let ((unassigned-bindings
               (map (lambda (d) (list (definition-variable d) ''*unassigned*)) defines))
              (set-expressions
               (map (lambda (d) (make-set (definition-variable d) (definition-value d))) defines)))
          (list (append (list 'let unassigned-bindings) (append set-expressions body)))))))
]

One thing to note is that the return value is wrapped in a
list. I got this wrong before. Also of note is the fact
that the unassigned value is passed as @tt{''*unassigned*}.
I also got this wrong initially, but it is very important --
otherwise this is taken to be the variable @tt{*unassigned*}, which
of course has no value and is not what we want to do.

I believe that @tt{make-procedure} is the best place to
transform the body into our new normalized form -- it seems
better to me that having the body in the form we want is
best guaranteed when the procedure object is defined, rather
than (hopefully!) normalizing it when it is accessed.

@section[#:tag "c4e17"]{Exercise 4.17}

@bold{TODO}

@section[#:tag "c4e18"]{Exercise 4.18}

@bold{TODO: Why wouldn't the definition work in both?}

@section[#:tag "c4e19"]{Exercise 4.19}

Consider the following:

@racketblock[
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
]

What should the result be? We are given a few options:

@itemlist[

@item{16, because @tt{b} is computed to be 11 (using the
definition of @tt{a} as 1) and @tt{a} is then reset to 5}

@item{No value -- because the internal definition of @tt{a}
would be lifted and unassigned at the time the value of
@tt{b} is computed}

@item{20, because the internal definition are truly
simultaneous and @tt{a} has the value of 5}

]

I believe the first option is a violation of the semantics
of the language as I understand them, which state that
internal definitions are computed simultaneously. But the
meaning of "simultaneously" is tricky, as supporting the
third option requires us to traverse the definitions to
compute the dependencies between them to attempt to reorder
them such that they can be computed sequentially (as they
must be in practice).

Alternatively, if the definition of @tt{b} (which references
that of @tt{a}) were only computed at the point when it is
used, which takes place after the sequence of internal
definitions, then the use of the internal definition of
@tt{a} would almost naturally fall out. However, this would
not be in line with Scheme's strict evaluation.

It's not clear to me that there is a simple way to decide
whether a set of internal definitions can be reordered such
that they can be evaluated sequentially. For a counterexample,
consider the following:

@racketblock[
(let ((a 1))
  (define b a)
  (define a b)
  (+ a b))
]

By the intuition of strictly sequential definitions, one
could imagine what this might do. But given that the
internal definitions are done "simultaneously", this
expression isn't meaningful at all.

In light of this, allowing the procedure to evaluate to an
error due to the referencing of an unassigned value seems
to me to be a reasonable thing to do.

@section[#:tag "c4e20"]{Exercise 4.20}

Consider the given use of @tt{letrec}:

@racketblock[
(define (f x)
  (letrec ((even?
            (lambda (n)
              (if (= n 0)
                  true
                  (odd? (- n 1)))))
           (odd?
            (lambda (n)
              (if (= n 0)
                  false
                  (even? (- n 1))))))
    <rest of body of f>))
]

This is equivalent to the following:

@racketblock[
(define (f x)
  (let ((even? '*unassigned*)
        (odd? '*unassigned*))
    (set! even?
          (lambda (n)
            (if (= n 0)
                true
                (odd? (- n 1)))))
    (set! odd?
          (lambda (n)
            (if (= n 0)
                false
                (even? (- n 1)))))
    <rest of body of f>))
]

We can write a transformation procedure similar to
@tt{scan-out-defines} as follows:

@racketblock[
(define (letrec->nested-let exp)
  (let ((names (let-names exp))
        (body (let-body exp)))
    (let ((unassigned-bindings
           (map (lambda (n) (list n '*unassigned)) names))
          (set-expressions
           (map (lambda (p) (make-set (car p) (cdr p))) (let-bindings exp))))
      (append (list 'let unassigned-bindings) (append set-expressions body)))))
]

Louis is wrong because, if using a standard @tt{let}
expression, the definition of @tt{even?} will fail because
@tt{odd?} is not yet present in the environment, meaning
that the attempt to set the initial value will error on
trying to reference the value of this binding.

@bold{TODO: Environment diagram}

@section[#:tag "c4e21"]{Exercise 4.21}

First off, let's verify that the given expression in fact
evaluates the factorial of 10:

@examples[
#:eval ev #:label #f
(letrec ((fact
          (lambda (n)
            (if (= n 1)
                1
                (* n (fact (- n 1)))))))
  (fact 10))

((lambda (n)
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1)))))))
 10)
]

This is using a trick known as the Y combinator to call a
function recursively without giving it an explicit binding
-- that is, the "name" is supplied by passing the function
as an argument. One way of describing the "trick" is that
the name of the function is preserved by passing the function
to itself -- that way, the function will always have a name
for itself.

Now let's suppose we have the following function to check
whether a value is even:

@examples[
#:eval ev #:label #f
(define (f x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (- n 1))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (- n 1))))
  (even? x))
(f 1)
(f 100)
]

We can write an equivalent version of this without using
internal definitions by following the lead of the above and
passing both @tt{even?} and @tt{odd?} to themselves (even
though only one of them will need to be executed directly,
the other will be needed if the base case is not yet reached).

Following the template, we can arrive at the following:

@examples[
#:eval ev #:label #f
(define (f x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))
(f 1)
(f 100)
]

@section[#:tag "c4e22"]{Exercise 4.22}

Adding support for @tt{let} expressions in this new
analyzing interpreter is nearly trivial, because previously
we supported them by transforming them into a form we
already supported -- namely, that of an immediately invoked
anonymous function.

@racketblock[
(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ;; this is the new line
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))
]

I say almost trivial because @tt{scan-out-defines} does not work
immediately like this, since the procedure has already been analyzed
at the time when it is called. @bold{TODO: Remove this limitation...}

@section[#:tag "c4e23"]{Exercise 4.23}

Consider the two implementations of @tt{analyze-sequence}:

@racketblock[
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))
]

@racketblock[
(define (analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE")
    (lambda (env) (execute-sequence procs env)))))
]

Suppose we have a body with a single expression. In the
first variant, @tt{loop} will immediately hit the base case
and return that (analyzed) expression, to be executed
directly when being evaluated. In the second variant, the
base case in @tt{execute-sequence} will similarly
immediately be hit when the body is evaluated. The key
difference is that this happens during execution and not
analysis, as the text says.  In the presence of repeated
execution, this is a bit suboptimal.

In the case where the body has two expressions, the second
variant will still defer all work in the analysis
phase. During evaluation, it will iterate through thef
sequence of expressions, evaluating each in turn. The first
variant, however, will construct a new procedure that, when
given an environment, will first call the first expression
and then the second. (In the general case, it will chain
these together, by using the sequenced execution of the first
two expressions as the new "first expression" in the next
@tt{loop} iteration.) Again, the second variant is suboptimal
when a procedure is evaluated more than once.

Another thing to note is that, by doing a list traversal at
evaluation time, the second variant also has to repeatedly
check for the end of the list. This is not the case when
evaluating the sequence in the first variant, as the new
composed procedure calls exactly what it needs to
unconditionally.

@section[#:tag "c4e24"]{Exercise 4.24}

@bold{TODO}

@section[#:tag "c4e25"]{Exercise 4.25}

Suppose we have the following procedure for computing factorials:

@racketblock[
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))
]

Ignoring the fact that this doesn't compute the factorial of
@tt{0}, this won't work at all with strict evaluation. The
arguments to the @tt{unless} function will unconditionally
be evaluated, meaning that the recursive call to
@tt{factorial} will always be evaluated and the procedure
will never terminate. With lazy evaluation, we will only
force the evaluation of the recursive call in the case where
@tt{n} is not equal to @tt{1}.

@section[#:tag "c4e26"]{Exercise 4.26}

Suppose that Ben Bitdiddle responds to this by saying that
@tt{unless} could still be defined as a special form when
evaluating in applicative order. He is correct: You could
define @tt{unless} to be a syntactic transformation to
the equivalent @tt{if} expression that swaps the result
expressions:

@examples[
#:eval ev #:label #f
(define-syntax unless
  (syntax-rules ()
    ((_ condition alternative consequent)
     (if condition
         consequent
         alternative))))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 10)
]

We're lucky to be able to define special forms ourselves
in Lisp, but this is not universally true. In many languages,
the choice of evaluation strategy is something you can only
get around by the crudest means, if at all.

In fact, one of the reasons why macros in Lisp are so useful
is because they let us simulate lazy evaluation. One could
easily argue that using lazy evaluation would be a reasonable
substitute for macros in many cases. This would also have
the benefit of allowing contructions like @tt{unless} to be
first-class values -- note that it is impossible to pass
@tt{unless}, @tt{if}, or any other special form as an argument,
for example, because they aren't actually values. If you try
to evaluate them by themselves, you'll get a syntax error.

@section[#:tag "c4e27"]{Exercise 4.27}

Let's run through the given session in the lazy REPL to
see an example of how mutation and lazy evaluation can
interact with one another strangely. Suppose we begin
with the following:

@racketblock[
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)
]

Now we do the following:

@racketblock[
(define w (id (id 10)))
]

One thing to note is that, when a definition is evaluated,
the value being assigned is evaluated. So even before
we've used the value @tt{w}, the expression of its value
has been evaluated. This means that one @tt{id}, the outer
one, has been evaluated, and @tt{count} has been incremented
once for it.

But what about the inner @tt{id} expression? Remember that
the argument to @tt{id} is simply passed through -- in other
words, @tt{(id 10)} would evaluated to @tt{10}. However,
that expression has not been evaluated yet, because the
value of @tt{w} hasn't been forced yet. Therefore, the
value of @tt{count} at this point is @tt{1}.

After we evaluated @tt{w} (which results in @tt{10}, as we
should have expected), @tt{count} is @tt{2}. Since the
value of @tt{w} was memoized, every new evaluation of @tt{w}
will result in no change to @tt{count}.

@section[#:tag "c4e28"]{Exercise 4.28}

One of the changes we made to the evaluator was to make
@tt{eval} use @tt{actual-value} instead of @tt{eval} on
the operator when evaluating an application. This is
necessary, as the book says, because the operator needs to
be forced before being passed to the @tt{apply} function
before it can be distinguished as a primitive or compound
procedure and correctly applied. A simple test case is
as follows:

@racketblock[
(define (f g x) (g x))
(g + 1)
]

@tt{+} will be a thunk when passed to the @tt{apply} method,
and therefore both @tt{primitive-procedure?} and
@tt{compound-procedure?}  will be false, causing an error as
it does not appear to be a procedure at all.

@section[#:tag "c4e29"]{Exercise 4.29}

A simple program that performs much more slowly without memoization:

@racketblock[
(define (fib n)
  (cond ((= n 0) 1)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(define (square x) (* x x))

(square (fib 10))
]

Now consider the following program:

@racketblock[
(define count 0)

(define (id x)
  (set! count (+ count 1))
  x)

(define (square x) (* x x))

(square (id 10))

count
]

With memoization, the result is @tt{1} -- @tt{(id 10)} gets
evaluated, and increments @tt{count}, only once, even though
@tt{square} references its value twice. However, without
memoization, @tt{count} is actually @tt{2}, because @tt{x} is
referenced by name twice in the body of @tt{square}.

@section[#:tag "c4e30"]{Exercise 4.30}

Consider the @tt{for-each} procedure:

@racketblock[
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))
]

Ben is calling is like so, and claims that its side effects are
performed:

@racketblock[
(for-each (lambda (x) (newline) (display x))
          (list 57 321 88))
]

He very well may be right, because these functions are
not being passed side-effectful operation as arguments.

However, I would claim that this only works correctly given
certain assumptions about how the @tt{newline} and
@tt{display} functions are implemented in our
evaluator. Remember that the only difference between our
original evaluator and this one is that compound procedures
are non-strict in their arguments. This leaves open the case
where side-effectful operations could be passed as arguments
to functions, in which case they well may not be
forced. Consider the two following functions:

@racketblock[
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))
]

With our current evaluator, @tt{(p1 1)} creates the
@tt{cons} cell @tt{(1 . 2)} and @tt{(p2 1)} does not. With
Cy's proposed change, they both do. This is because @tt{p1}
evaluates its side effect as one of a sequence of
expressions in the function definition, while with @tt{p2}
the side-effecting computation binds to a new value @tt{e}
that is itself evaluated, but not forced, by the inner
procedure @tt{p}, leaving @tt{x} unchanged.

Cy is right that, if the given @tt{for-each} example works
in the original evaluator, it will continue to work in
his -- if @tt{eval} is enough to force a side effect, then
@tt{actual-value} will not change this. However, it would not
be right to say that all @tt{for-each} calls will be handled
identically. Consider the following:

@racketblock[
(define count 0)

(define (id x) (set! count (+ count 1)) x)

(define (perform x) x 'ok)

(for-each (lambda (x) (perform (id x))) '(1 2 3))

count
]

Under the original evaluator, @tt{count} will still be
@tt{0}, but with Cy's change, it will be @tt{3}.

Laziness and side effects being mixed makes me very
uncomfortable, but at the moment, I think Cy's change
might be more predictable for thinking about when (or
whether) side effects occur.

@section[#:tag "c4e31"]{Exercise 4.31}

Suppose we want to give the user the option to choose
whether function parameters are strict or lazy, and
whether lazy parameters are memoized or not. This
is proposed with an example of new @tt{define} syntax:

@racketblock[
(define (f a (b lazy) c (d lazy-memo))
  ...)
]

However, I think an unstated assumption is that this
feature would also apply to anonymous functions (not
in the least because @tt{define}s are evaluated by
transforming their bodies into @tt{lambda}s). So,
the way that we are going to proceed is as follows:

@itemlist[
@item{The functions for creating procedures will not change.
          All the information about how the parameters are
          supposed to be handled are in the parameter lists.}
@item{A new function for getting argument values will be created
        for compound procedures. This will expect the arguments
        to be a list of lists, where the first element is the
        expression and the second is the type dictating how lazily
        it is to be evaluated. This is one of @tt{'strict},
        @tt{'lazy}, or @tt{'lazy-memo}.}
@item{Since the parameter lists of the procedure store the types
            of each parameter (or nothing, if the parameter is strict),
            we can zip along it and pair each argument expression with
            the type very easily.}
]

The code changes are as follows. First, our new procedures:

@racketblock[
(define (delay-it-memo exp env)
  (list 'thunk-memo exp env))

(define (memo-thunk? obj)
  (tagged-list? obj 'thunk-memo))

(define (is-strict-param? p) (symbol? p))
(define (is-lazy-param? p) (and (pair? p) (eq? (parameter-type p) 'lazy)))
(define (is-lazy-memo-param? p) (and (pair? p) (eq? (parameter-type p) 'lazy-memo)))

(define (parameter-name p) (if (pair? p) (car p) p))
(define (parameter-type p) (if (pair? p) (cadr p) 'strict))

(define (zip-with-types parameters arguments)
  (define (loop params args)
    (if (null? params)
        '()
        (cons (list (car args) (parameter-type (car params)))
              (loop (cdr params) (cdr args)))))
  (loop parameters arguments))

(define (argument-exp exp) (car exp))
(define (argument-type exp) (cadr exp))

(define (list-of-mixed-arg-values exps env)
  (if (no-operands? exps)
      '()
      (let* ((first (first-operand exps))
             (exp (argument-exp first))
             (type (argument-type first))
             (next-value
              (cond ((eq? type 'strict) (actual-value exp env))
                    ((eq? type 'lazy) (delay-it exp env))
                    ((eq? type 'lazy-memo) (delay-it-memo exp env))
                    (else (error "Unknown laziness type -- LIST-OF-MIXED-ARG-VALUES" first)))))
        (cons next-value (list-of-mixed-arg-values (rest-operands exps)
                                                   env)))))
]

Next, we change the evaluation strategy for compound
procedures in @tt{apply}. The only difference is in
the first two arguments passed to @tt{extend-environment}:

@racketblock[
(define (apply procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (let ((args (list-of-arg-values arguments env)))
           (if (any? (map error? args))
               error-value
               (apply-primitive-procedure
                procedure
                args))))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (map parameter-name (procedure-parameters procedure))
           (list-of-mixed-arg-values (zip-with-types (procedure-parameters procedure) arguments) env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
]

Lastly, @tt{force-it} needs to force memoized and non-memoized
thunks differently:

@racketblock[
(define (force-it obj)
  (cond ((memo-thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        ((thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj)))
        (else obj)))
]

This is all that's required to make it possible to specify
the laziness of any parameter to a function.
