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
                (cond ((cond-alternate-form? first)
                       (error "ELSE clause isn't allowed in alternate form" clauses))
                      ((null? rest) (sequence->exp (cond-actions first)))
                      (else (error "ELSE clause isn't last -- COND->IF" clauses))))
               ((cond-alternate-form? first)
                (let ((rest-expanded (expand-clauses rest)))
                  (list (make-lambda '(v f) (list (make-if 'v '(f v) rest-expanded)))
                        (cond-predicate first)
                        (cond-alternate-form-proc first))))
               (else
                (let ((rest-expanded (expand-clauses rest)))
                  (make-if (cond-predicate first)
                           (sequence->exp (cond-actions first))
                           rest-expanded)))))))
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
     (if (null? (cdr bindings))
         (apply make-let (cons bindings (let-body exp)))
         (make-let (list (car bindings)) (iter (cdr bindings)))))
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
 (define (named-let-body exp) (cdddr exp))
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
           (apply-primitive-procedure
            procedure
            args)))
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

@section[#:tag "c4e32"]{Exercise 4.32}

One thing you can do when @tt{cons} is lazy in both arguments
that you couldn't do with the streams from earlier is do
computations on the "spine" of a list -- that is, ask for
questions such as the length -- without evaluating any of
the values in that list. That means that you could have a
list entirely filled with expression dividing by zero and
compute the length of it anyway. This may be a contrived
example, but you could also imagine a situation where the
values in a list are expensive to compute, and now having a
general way to handle that list without forcing any of the
list values that you don't need to use.

An obvious counterpoint is that you can already do this
without lazy evaluation, as long as you manually "thunk" the
list items yourself. That would allow you to control when
the list items are evaluated, but naively wrapping the
values in functions of no arguments would not allow you
to memoize the computed values. So you'll have to define a
slightly more sophisticated representation that mutates
itself to return the computed result once it has been
evaluate, even though this facility could be supported
directly by the language without too much trouble.

@section[#:tag "c4e33"]{Exercise 4.33}

For the time being, quoted expressions will be turned into
lazy lists via a syntactic transformation. It gets installed
via a modified case in @tt{eval}:

@racketblock[
((quoted? exp)
 (let ((rest (cadr exp)))
   (if (list? rest)
       (eval (transformed-quotation rest) env)
       rest)))
]

@tt{transformed-quotation} is not a very difficult function --
essentially, it's a left fold (or an @tt{accumulate}) over the
quoted expression that @tt{quote}s every atom in the list (so as
not to evaluate them) or recursively calls itself on lists. The
expression that ends up being constructed is that of a list being
contructed via nested @tt{cons}es.

@racketblock[
(define (transformed-quotation exp)
  (accumulate (lambda (next acc)
                (list 'cons
                      (if (list? next) (transformed-quotation next) (list 'quote next))
                      acc))
              nil
              exp))
]

@section[#:tag "c4e34"]{Exercise 4.34}

@bold{I have not solved this yet. This is an outline of what I would like to do.}

To put aside an issue right at the start, I'm not going to
do anything special to "safely" print infinitely (or merely
very) long lists, because I don't think it's an
interpreter's job to do this for me. The correct solution,
if this were a real interpreter, would be to support
interrupting the printing with a standard shell interrupt.

I think the most natural way to handle this issue is to
interpret the printing of a lazy list as fully forcing it.
Obviously we can't force lazy lists everywhere -- they
wouldn't be lazy lists anymore -- but it seems strange
to me that a list would be evaluated (in full, due to the
above) only for the purposes of printing.

@tt{cons}, @tt{car}, and @tt{cdr} should all be memoizing.
This is especially important for @tt{cons}, as there
would be little benefit to forcing the list expression if
it were not.

This behavior should also be fully transparent and hidden
from the user. That is, augmenting our environment with
new helper procedures should not be allowed. I believe that
this implies that we have to create a new procedure type
that can only be installed in our environment with internal
procedures.

@section[#:tag "c4e35"]{Exercise 4.35}

I'm choosing that @tt{an-integer-between} should be inclusive
with the lower bound and exclusive on the right. I'm also
treating any case where the first argument is not lower than
the second as a failure, because then a natural solution falls
out:

@racketblock[
(define (an-integer-between i j)
  (require (< i j))
  (amb i (an-integer-between (+ i 1) j)))
]

@section[#:tag "c4e36"]{Exercise 4.36}

Consider the proposed procedure for generating all
Pythagorean triples:

@racketblock[
(define (pythagorean-triples)
  (let ((i (an-integer-starting-from 1))
        (j (an-integer-starting-from i))
        (k (an-integer-starting-from j)))
    (require (= (+ (* i i) (* j j)) (* k k)))
    (list i j k)))
]

Using @tt{an-integer-starting-from} in place of
@tt{an-integer-between} in the given procedure for computing
Pythagorean triples would not suffice because, like with the
infinite streams seen earlier, we would only be able to
produce Pythagorean triples starting from a fixed @tt{i} and
@tt{j}. This is because @tt{an-integer-starting-from}
produces an infinite number of values, and our backtracking
will always have us try again with the next value of
@tt{k}. Like before, we need to interleave these values.

However, the kind of interleaving we did in the streams
chapter isn't going to work here, because the only decision
we can revisit is the last one we've made. We need to
constrain the inner loops while allowing the outer loop to
generate toward infinity.

Another way of thinking about this is that we need to find a
global ordering for all Pythagorean triples that is
parametized on a single value. A natural choice for this is
the value of @tt{k} -- by definition, we know that the
values of @tt{i} and @tt{j} have to be integers between
@tt{1} and @tt{k}, and we can generate them sequentially as
in the correct procedure for generating bounded Pythagorean
triples. Since the nondeterministic expressions generating
@tt{i} and @tt{j} values are guaranteed to fail, we know
that all possible values of @tt{k} will eventually be
considered, and that all Pythagorean triples will eventually
be found.

@racketblock[
(define (pythagorean-triples)
  (let ((k (an-integer-starting-from 1)))
    (let ((i (an-integer-between 1 k)))
      (let ((j (an-integer-between i k)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
]

@section[#:tag "c4e37"]{Exercise 4.37}

Consider the two following procedures for computing
Pythagorean triples with high and low bounds:

@racketblock[
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))
]

@racketblock[
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
]

The former method is naive and straightforward, iterating
through all of the possibilities for @tt{i}, @tt{j}, and
@tt{k} and testing whether the equality holds. This is very
inefficient, requiring on the order of @tt{O(n^3)}
operations.

The alternative solution is more clever. First, it acts on
the fact that it isn't necessary to test more than a single
potential @tt{k} value after @tt{i} and @tt{j} are fixed --
there's only one sum to their squares, and the square root
of that is either an integer or not. Doing this cuts a power
of @tt{n} off of the time complexity.

However, in order for this to work, you need to know when to
stop testing values. However, this is also easily done -- we
know that the maximum value of @tt{k} cannot be greater than
the square root of the high bound. Alternatively, as
expressed in the algorithm above, @tt{k^2} cannot be greater
than @tt{high^2}. (My only complaint about the above is that
I would calculate @tt{hsq} before @tt{i}).  Then, all that
needs to be done is checking whether the potential @tt{k^2}
values are no greater than this upper bound.

We can see that the second algorithm does test every valid
@tt{i} and @tt{j}, and that this means that all possible
triples with @tt{i} and @tt{j} within bounds are checked. We
can also see that no possible triples outside of the bounds
will be considered -- any value of @tt{k} such that @tt{k^2}
is greater than @tt{high^2} is not accepted. Therefore, we
can trust that the second algorithm is correct, while also
being significantly more efficient than the first.

@section[#:tag "c4e38"]{Exercise 4.38}

There are five solutions to the modified problem:

@verbatim{
;;; Amb-Eval input:
(multiple-dwelling-2)

;;; Starting a new problem
;;; Amb-Eval value:
((baker 1) (cooper 2) (fletcher 4) (miller 3) (smith 5))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
((baker 1) (cooper 2) (fletcher 4) (miller 5) (smith 3))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
((baker 1) (cooper 4) (fletcher 2) (miller 5) (smith 3))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
((baker 3) (cooper 4) (fletcher 2) (miller 5) (smith 1))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(multiple-dwelling-2)
}

@section[#:tag "c4e39"]{Exercise 4.39}

Changing the order in which the restrictions are checked
does not change the answer(s) that are found. This is
intuitively obvious -- the same values are tested, and the
set of restrictions that are checked is the same, so all
the sets of values that satisfy all of the restrictions
must be the same no matter what order they are checked in.

However, the order that the restrictions are checked in
@italic{does} have an effect on the amount of time it may
take to find an answer. The biggest optimization to be found
is in moving the distinctness requirement to the end. The
logic for this move is that this operation is vastly more
expensive to check than any of the other requirements,
and therefore should be tested as few times as possible. If
we exhaust the rest of requirements first, then the number
of times that @tt{distinct?} is called is as low as it could
possibly be.

For a more limited optimization, consider the fact that one
of the first checks is that @tt{cooper} is not equal to
@tt{1}. @tt{cooper} is the second value generated, which
means that three other sets need to be tested to exhaustion
before @tt{cooper} gets a passing value.

For arbitrary programs, finding the optimal order to test
requirements in may be very difficult. However, the order
that the requirements for participants are being tested in
in this program is (almost) the same as the order in which
those participants were assigned values. If, before the
distinctness check, we simply reversed the order in which
the requirements were checked, we would do much better,
and if we moved the requirement for @tt{fletcher} and
@tt{cooper} not to be within one of each other to be after
the check that @tt{miller} is not above @tt{cooper}, we
would be even better at finding the one solution to the
problem quickly.

@section[#:tag "c4e40"]{Exercise 4.40}

There are @tt{5^5 = 3125} non-unique assignments of
floors to residents and only @tt{5! = 120} unique
assignments. This means that the vast majority
of the assignments that we will test, even ignoring
the other requirements in the problem, will be failures.
In addition to reducing the number of times we test
for distinctness, we should be reducing the search space
by testing our conditions as each assignment is generated,
rather than after we have constructed a full assignment.

Borrowing the order of the requirements from the original
solution (minus @tt{distinct?}, which is moved to the
end), we arrive at the following program:

@racketblock[
(define (multiple-dwelling-3)
  (let ((baker (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (let ((cooper (amb 1 2 3 4 5)))
      (require (not (= cooper 1)))
      (let ((fletcher (amb 1 2 3 4 5)))
        (require (not (= fletcher 5)))
        (require (not (= fletcher 1)))
        (require (not (= (abs (- fletcher cooper)) 1)))
        (let ((miller (amb 1 2 3 4 5)))
          (require (> miller cooper))
          (let ((smith (amb 1 2 3 4 5)))
            (require (not (= (abs (- smith fletcher)) 1)))
            (require
             (distinct? (list baker cooper fletcher miller smith)))
            (list (list 'baker baker)
                  (list 'cooper cooper)
                  (list 'fletcher fletcher)
                  (list 'miller miller)
                  (list 'smith smith))))))))
]

We can see that it generates the correct answer:

@verbatim{
;;; Amb-Eval input:
(multiple-dwelling-3)

;;; Starting a new problem
;;; Amb-Eval value:
((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(multiple-dwelling-3)
}

@section[#:tag "c4e41"]{Exercise 4.41}

The nondeterminism created by @tt{amb} can also be
represented by lists (or streams) of values that we can
"select" by @tt{filter}ing and @tt{flatmap}ping over them.
Thus, the values in the list represent all of the possible
values that a binding could have, and @tt{flatmap}ping over
this list is akin to selecting an arbitrary one of them.
(Obviously, the selection is not really arbitrary, because
lists are ordered, but the answer will still be correct.)

We can use a mechanical transformation of @tt{amb} and @tt{
 require} operations into @tt{flatmap} and @tt{filter}
sequences to translate the version of this program from
@secref{c4e40}. Note as well the similarities to the solution
to the eight queens problem from @secref{c2e42}.

@examples[
 #:eval ev #:hidden
 (define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

 (define (flatmap proc seq)
   (accumulate append nil (map proc seq)))

 (define (filter predicate sequence)
   (cond ((null? sequence) nil)
         ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

 (define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))
 ]

@examples[
 #:label #f #:eval ev #:no-prompt
 (define (multiple-dwelling-4)
   (let* ((pick-floor (list 1 2 3 4 5))
          (baker-choices (filter (lambda (baker) (not (= baker 5))) pick-floor)))
     (flatmap
      (lambda (baker)
        (let ((cooper-choices (filter (lambda (cooper) (not (= cooper 1))) pick-floor)))
          (flatmap
           (lambda (cooper)
             (let ((fletcher-choices (filter (lambda (fletcher)
                                               (and (not (= fletcher 5))
                                                    (not (= fletcher 1))
                                                    (not (= (abs (- fletcher cooper)) 1))))
                                             pick-floor)))
               (flatmap
                (lambda (fletcher)
                  (let ((miller-choices (filter (lambda (miller) (> miller cooper)) pick-floor)))
                    (flatmap
                     (lambda (miller)
                       (let ((smith-choices (filter (lambda (smith) (not (= (abs (- smith fletcher)) 1))) pick-floor)))
                         (filter (lambda (result) (distinct? (map cadr result)))
                                 (map (lambda (smith)
                                        (list (list 'baker baker)
                                              (list 'cooper cooper)
                                              (list 'fletcher fletcher)
                                              (list 'miller miller)
                                              (list 'smith smith)))
                                      smith-choices))))
                     miller-choices)))
                fletcher-choices)))
           cooper-choices)))
      baker-choices)))
 ]

@examples[
 #:label #f #:eval ev
 (pretty-display (multiple-dwelling-4))
 ]

Because there is only one solution to the puzzle, it doesn't
matter so much whether we use stream or lists of choices.
However, lazy evaluation more accurately matches the
semantics of @tt{amb} than do strict lists.

@section[#:tag "c4e42"]{Exercise 4.42}

We're going to solve the puzzle logically and with an
@tt{amb} program.

First, a restatement of the five statements, rephrased in a
more normalized fashion:

@itemlist[
 @item{Kitty was in second. Betty was in third.}
 @item{Ethel was in first. Joan was in second.}
 @item{Joan was in third. Ethel was in fifth.}
 @item{Kitty was in second. Mary was in fourth.}
 @item{Mary was in fourth. Betty was in first.}
 ]

Because of the final two statements, either Mary was in
fourth place @bold{and} Kitty was not in second place @bold{
 and} Betty was not in first place, @bold{or} Mary was not in
fourth place @bold{and} Kitty was in second place @bold{and}
Betty was in first place. However, based on the second
statement, we know that either Ethel was in first place or
Joan was in second. This means that it cannot be the case
that Kitty was in second place nor that Betty was in first,
so we can conclude that Mary was in fourth place.

Because we know that Kitty was not in second place, we can
conclude from the first statement that Betty was in third
place.

Because we know that Betty was in third place, we can
conclude from the third statement that Ethel was in fifth.

Because we know that Ethel was in fifth place, we can
conclude from the second statement that Joan was in second.

At this point, we have only one person and one place left.
Kitty must have come in first place. The final ordering is
thus

@itemlist[
 #:style 'ordered
 @item{Kitty}
 @item{Joan}
 @item{Betty}
 @item{Mary}
 @item{Ethel}
]

With that in mind, let's write an @tt{amb} program to compute
this.

The first construct we'll need is a way of encoding the fact
that exactly one of the claims in each statement are true --
the @tt{xor} operation, in other words. Defining this in terms
of @tt{and} and @tt{xor} is typical, but we have neither of
these in our interpreter, so we'll define it slightly differently:

@racketblock[
 (define (xor a b)
   (cond (a (not b))
         (else b)))
 ]

Then, using the same @tt{require} and @tt{distinct?} functions
defined earlier, we can write our @tt{amb} program. Here is one
expression:

@racketblock[
 (define (liars)
   (let ((betty (amb 1 2 3 4 5))
         (ethel (amb 1 2 3 4 5))
         (joan (amb 1 2 3 4 5))
         (kitty (amb 1 2 3 4 5))
         (mary (amb 1 2 3 4 5)))
     (require (xor (= kitty 2) (= betty 3)))
     (require (xor (= ethel 1) (= joan 2)))
     (require (xor (= joan 3) (= ethel 5)))
     (require (xor (= kitty 2) (= mary 4)))
     (require (xor (= mary 4) (= betty 1)))
     (require (distinct? (list betty ethel joan kitty mary)))
     (list (list 'betty betty)
           (list 'ethel ethel)
           (list 'joan joan)
           (list 'kitty kitty)
           (list 'mary mary))))
 ]

Running our program, we see that the one (and only) solution
to the puzzle is the same as the one we found:

@verbatim{
;;; Amb-Eval input:
(liars)

;;; Starting a new problem
;;; Amb-Eval value:
((betty 3) (ethel 5) (joan 2) (kitty 1) (mary 4))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(liars)
}

@section[#:tag "c4e43"]{Exercise 4.43}

Solving this logically, Gabrielle's father is either Hall or
Downing. (It can't be Parker, because then Gabrielle's
father, Parker, would own a yacht named after Parker's
daughter -- a father can't have a yacht named after their
actual daughter.) If Gabrielle's father is Downing, then we
have a problem -- Downing owns the yacht named after Sir
Barnacle's daughter. So we can conclude that Gabrielle's
father is Hall, and Dr. Parker's daughter is Rosalind, and
then Downing's daughter is Lorna. The answer to the question
is thus Downing. We have an answer -- we just need to verify
it with an amb program.

The first (and most important) step to solving this problem
is deciding on how you will represent the facts such that
you can write efficient predicates on top of them. What
makes this somewhat tricky is that the relationships are
traversed in both directions -- in particular, the fact that
Gabrielle's father owns the yacht that is named after Dr.
Parker's daughter is tricky, because it requires traversing
from a known daughter to an unknown father and them from a
known father to an unknown daughter. A simple representation
is probably not going to allow for this bidirectional
traversal, so you will either be forced to use a complex
representation or find a way around this.

In this case, the representation I've chosen is a set of
bindings from each father to a pair of children, the first
of which is his daughter and the second of which he named
his yacht after. I've chosen this because the father is the
the unifying link between daughters and yachts, and I think
it will help to keep those facts together (rather than, say,
spread across multiple similarly-named variables).

In order to make an assertion based on Gabrielle's father,
I use a small hack: I happen to know every person who can
be her father, so I can use conditional logic to select
which yacht needs to be named after Dr. Parker's daughter.

One again, I'm going to intersperse @tt{amb} expressions
with @tt{require}ments based on the results of those
expressions, to reduce the amount of useless backtracking.
I'm also going to make one immediate simplification and make
sure not to allow Parker to be Gabrielle's daughter -- given
the condition we've already talked about, this is obviously
nonsense, and it would force you to write an expression (and
do @tt{amb} generation for) a possibility that is known to
be against the rules. I've also asserted immediately that
Dr. Parker's yacht is the Mary, because 4 of 5 yachts are
assigned in the problem statement, and writing an @tt{amb}
expression with a single argument is silly.

Here is a solution for the original problem:

@racketblock[
 (define (daughters)
   (define daughter car)
   (define (yacht p) (car (cdr p)))
   (let ((moore (list 'mary 'lorna))
         (barnacle (list 'melissa 'gabrielle))
         (hall (list (amb 'lorna 'gabrielle) 'rosalind))
         (downing (list (amb 'rosalind 'lorna 'gabrielle) 'melissa)))
     (require (not (eq? (daughter hall) (daughter downing))))
     (let ((parker (list (amb 'rosalind 'lorna) 'mary)))
       (require (not (eq? (daughter hall) (daughter parker))))
       (require (not (eq? (daughter downing) (daughter parker))))
       (if (eq? (daughter hall) 'gabrielle)
           (require (eq? (yacht hall) (daughter parker)))
           (require (eq? (yacht downing) (daughter parker))))
       (list (list 'moore moore)
             (list 'barnacle barnacle)
             (list 'hall hall)
             (list 'downing downing)
             (list 'parker parker)))))
 ]

When we run this, we can see that there is only one
solution, and it's identical to the one derived logically:

@verbatim{
;;; Starting a new problem
;;; Amb-Eval value:
ok

;;; Amb-Eval input:
(daughters)

;;; Starting a new problem
;;; Amb-Eval value:
((moore (mary lorna)) (barnacle (melissa gabrielle)) (hall (gabrielle rosalind)) (downing (lorna melissa)) (parker (rosalind mary)))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(daughters)
}

Supposing instead we were not told that Mary Ann's last name
is Moore. We can modify the original problem by selecting
Moore's daughter with the @tt{amb} expression selecting
between his possible daughters (namely, everyone except
Lorna), and inserting Mary Ann's name into the @tt{amb}
expressions for every possible father in addition to Moore
-- Hall and Downing, since we know that Parker owns the Mary
Ann. In the end, this allows for Mary Ann and Gabrielle to
switch between being Moore and Hall's daughter and for
Rosalind and Lorna to switch between being Downing and
Parker's daughters.

@racketblock[
 (define (daughters-variant)
   (define daughter car)
   (define (yacht p) (car (cdr p)))
   (let ((barnacle (list 'melissa 'gabrielle))
         (moore (list (amb 'mary 'rosalind 'gabrielle) 'lorna))
         (hall (list (amb 'mary 'lorna 'gabrielle) 'rosalind)))
     (require (not (eq? (daughter moore) (daughter hall))))
     (let ((downing (list (amb 'mary 'rosalind 'lorna 'gabrielle) 'melissa)))
       (require (not (eq? (daughter moore) (daughter downing))))
       (require (not (eq? (daughter hall) (daughter downing))))
       (let ((parker (list (amb 'rosalind 'lorna) 'mary)))
         (require (not (eq? (daughter moore) (daughter parker))))
         (require (not (eq? (daughter hall) (daughter parker))))
         (require (not (eq? (daughter downing) (daughter parker))))
         (cond ((eq? (daughter moore) 'gabrielle) (require (eq? (yacht moore) (daughter parker))))
               ((eq? (daughter hall) 'gabrielle) (require (eq? (yacht hall) (daughter parker))))
               (else (require (eq? (yacht downing) (daughter parker)))))
         (list (list 'moore moore)
               (list 'barnacle barnacle)
               (list 'hall hall)
               (list 'downing downing)
               (list 'parker parker))))))
 ]

@verbatim{
;;; Amb-Eval input:
(daughters-variant)

;;; Starting a new problem
;;; Amb-Eval value:
((moore (mary lorna)) (barnacle (melissa gabrielle)) (hall (gabrielle rosalind)) (downing (lorna melissa)) (parker (rosalind mary)))

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
((moore (gabrielle lorna)) (barnacle (melissa gabrielle)) (hall (mary rosalind)) (downing (rosalind melissa)) (parker (lorna mary)))

;;; Amb-Eval input:
try-again

;;; There are no more values of
(daughters-variant)
}

@section[#:tag "c4e44"]{Exercise 4.44}

We can reuse a lot of the procedures already written for the
eight-queens problem. In fact, the general approach will be
the same: We will iteratively generate new positions
nondeterministically, then @tt{require} the new position to
be safe with respect to all of the others.

Here is an entire program for solving the eight queens
problem. The data representation of the position is a list
of row placements, just like before. A few simple functions
that we haven't used in an @tt{amb} session before are
brought along as well.

@racketblock[
 (define (enumerate-interval low high)
   (if (> low high)
       '()
       (cons low (enumerate-interval (+ low 1) high))))

 (define (any? xs)
   (cond ((null? xs) false)
         ((car xs) true)
         (else (any? (cdr xs)))))

 (define (list-ref xs i)
   (if (= i 0)
       (car xs)
       (list-ref (cdr xs) (- i 1))))

 (define (or x y)
   (if x
       x
       y))

 (define (on-diagonal? i ri j rj)
   ;; assumption: i < j
   (let ((d (- j i)))
     (or
      ;; case 1: diagonal up
      (= rj (+ ri d))
      ;; case 2: diagonal down
      (= ri (+ rj d)))))

 (define (map f xs)
   (if (null? xs)
       xs
       (cons (f (car xs)) (map f (cdr xs)))))

 (define (safe? k positions)
   (define (conflicts? positions i j)
     (let ((ri (list-ref positions i))
           (rj (list-ref positions j)))
       (or
        (= ri rj)
        (on-diagonal? i ri j rj))))
   (not
    (any?
     (map (lambda (i) (conflicts? positions 0 i))
          (enumerate-interval 1 (- k 1))))))

 (define (eight-queens)
   (define (create-position) (amb 1 2 3 4 5 6 7 8))
   (define (iter k positions)
     (if (> k 8)
         positions
         (let* ((next-position (create-position))
                (possible-positions (cons next-position positions)))
           (require (safe? k possible-positions))
           (iter (+ k 1) possible-positions))))
   (iter 1 '()))
 ]

We can see that this generates valid solutions:

@verbatim{
;;; Amb-Eval input:
(eight-queens)

;;; Starting a new problem 
;;; Amb-Eval value:
(4 2 7 3 6 8 5 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(5 2 4 7 3 8 6 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(3 5 2 8 6 4 7 1)
}

In fact, we can do one better than this. There's no reason
why we have to only solve the problem for a board size of
eight. The number eight only appears in two places: To allow
the generation of new positions to place queens into every
row, and to detect the completion of the board. We can
parametize both of these fairly simply.

@racketblock[
 (define (n-queens n)
   (define (create-position) (an-element-of (enumerate-interval 1 n)))
   (define (iter k positions)
     (if (> k n)
         positions
         (let* ((next-position (create-position))
                (possible-positions (cons next-position positions)))
           (require (safe? k possible-positions))
           (iter (+ k 1) possible-positions))))
   (iter 1 '()))
 ]

This even generates the same solutions as the previous when
the board size is eight, but it can do even more:

@verbatim{
;;; Amb-Eval input:
(n-queens 8)

;;; Starting a new problem 
;;; Amb-Eval value:
(4 2 7 3 6 8 5 1)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(5 2 4 7 3 8 6 1)

;;; Amb-Eval input:
(n-queens 2)

;;; Starting a new problem 
;;; There are no more values of
(n-queens 2)

;;; Amb-Eval input:
(n-queens 4)

;;; Starting a new problem 
;;; Amb-Eval value:
(3 1 4 2)

;;; Amb-Eval input:
try-again

;;; Amb-Eval value:
(2 4 1 3)

;;; Amb-Eval input:
try-again

;;; There are no more values of
(n-queens 4)
}

Keeping in mind the way that lists can represent
nondeterministic selection, this solution is very similar to
the one presented back in chapter 2. However, the new
evaluator has made it much easier to express this solution.

@section[#:tag "c4e45"]{Exercise 4.45}

I've chosen to represent the five different parses of the
sentence via the actual representations found by an @tt{amb}
interpreter, because this happens to be an unambiguous
representation that can be understood readily and verified
by parsing the sentence yourself. Here are the
interpretations, followed by brief descriptions of how the
placement of the prepositional phrases colors the meaning of
the sentence:

@racketblock[
 (sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb-phrase
    (verb-phrase
     (verb lectures)
     (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
    (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
   (prep-phrase (prep with) (simple-noun-phrase) (article the) (noun cat))))
 ]

Here, there are three nested verb phrases. The first,
"lectures to the student", is qualified by the lecturing
being in the class, and this is qualified even further by
the fact that the lecturing is with the cat.

@racketblock[
 (sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
   (prep-phrase
    (prep in)
    (noun-phrase
     (simple-noun-phrase (article the) (noun class))
     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
 ]

Here, the class is a part of a larger noun phrase, adding
the qualification that the class has a cat. The lecturing is
now taking place "in the class with the cat".

@racketblock[
 (sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb-phrase
    (verb lectures)
    (prep-phrase
     (prep to)
     (noun-phrase
      (simple-noun-phrase (article the) (noun student))
      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
   (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))
 ]

Here, "the student in the class" is a larger noun phrase
which is the target of the lecturing. The professor is also
lecturing with the cat.

@racketblock[
 (sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (noun-phrase
      (simple-noun-phrase (article the) (noun student))
      (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
     (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat)))))))
 ]

Here, "the student in the class with the cat" is a single
compound noun phrase, and the noun phrases are nested such
that the student has the cat.

@racketblock[
 (sentence
  (simple-noun-phrase (article the) (noun professor))
  (verb-phrase
   (verb lectures)
   (prep-phrase
    (prep to)
    (noun-phrase
     (simple-noun-phrase (article the) (noun student))
     (prep-phrase
      (prep in)
      (noun-phrase
       (simple-noun-phrase (article the) (noun class))
       (prep-phrase
        (prep with)
        (simple-noun-phrase (article the) (noun cat)))))))))
 ]

Here, "the student in the class with the cat" is also a
single compound noun phrase, but the nesting of these
phrases is different -- now, "the class with the cat" is
what the student is in.

@section[#:tag "c4e46"]{Exercise 4.46}

Our parsing program relies on operands being evaluated in
left-to-right order because our parsing procedure modifies a
single global input list. Consider the following function
that was given in the chapter:

@racketblock[
 (define (parse-preopositional-phrase)
   (list 'prep-phrase
         (parse-word prepositions)
         (parse-noun-phrase)))
 ]

If the operands were evaluated in, say, right-to-left order,
then it would try to parse a noun phrase (thus expecting to
remove nouns from the input source) before the preposition
itself. This would parse a different set of sentences than
we expect.

It would be possible to write this function to behave
correctly with right-to-left operand evaluation, but it
might be somewhat confusing -- at the very least, the
operand order corresponds with the actual order that the
segments will be found in the sentence. However, if the
order of evaluation for operands were
implementation-defined, it would not be possible to write
this interpreter in a portable fashion.

@section[#:tag "c4e47"]{Exercise 4.47}

Compare Louis Reasoner's @tt{parse-verb-phrase} with the
original:

@racketblock[
 (code:comment @#,elem{Original})
 (define (parse-verb-phrase)
   (define (maybe-extend verb-phrase)
     (amb verb-phrase
          (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
   (maybe-extend (parse-word verbs)))

 (code:comment @#,elem{Louis Reasoner})
 (define (parse-verb-phrase)
   (amb (parse-word verbs)
        (list 'verb-phrase
              (parse-verb-phrase)
              (parse-prepositional-phrase))))
 ]

As stated, the main difference is that Louis chose to inline
the calls to @tt{parse-verb-phrase} and thereby eliminate
@tt{maybe-extend}. However, this is very critically wrong,
and will lead to an infinite loop.

Consider what will happen when this encounters a phrase that
doesn't start with a verb, as might happen naturally during
the exploration and backtracking process. First it will call
@tt{(parse-word verbs)}, which will predictably fail. At
this point it will try the second case, which requires
evaluating a recursive call to @tt{(parse-verb-phrase)}. The
first thing this will do is call @tt{(parse-word verbs)}
again, which will fail again and cause another recursive
call. We will continue to recurse without processing another
word.

The correct variant presented in the text sidesteps this
problem by moving the parsing of a verb outside of the
optional extension with prepositional phrases. If there is
no verb, the function will fail immediately, and
backtracking can proceed normally.

As a follow-up question, we are asked to consider what
will happen if the order of the @tt{amb} expression is
changed as so:

@racketblock[
 (define (parse-verb-phrase)
   (amb (list 'verb-phrase
              (parse-verb-phrase)
              (parse-prepositional-phrase))
        (parse-word verbs)))
 ]

This @bold{really} doesn't work -- the infinite recursion
will happen immediately.

@section[#:tag "c4e48"]{Exercise 4.48}

For this exercise, I'm going to be adding some support for
adjectives and adverbs, as well as simple conjunctions of
the two.

I'm going to be following a simple rule for parsing adverbs:
The adverb must appear after the verb it modifies. In
reality, placing adverbs is much more complicated than this,
but I'm not going to be exploring that at this time.

In the simplest form, instead of parsing just a verb, we are
either parsing a verb or a verb followed by an adverb. We'll
still label this a @tt{verb-phrase}.

@racketblock[
 (define adverbs '(adverb quickly slowly immediately lazily))

 (define (parse-verb-phrase-with-adverb)
   (list 'verb-phrase
         (parse-word verbs)
         (parse-word adverbs)))
 
 (define (parse-verb-phrase)
   (define (maybe-extend verb-phrase)
     (amb verb-phrase
          (maybe-extend (list 'verb-phrase
                              verb-phrase
                              (parse-prepositional-phrase)))))
   (maybe-extend (amb (parse-word verbs)
                      (parse-verb-phrase-with-adverb))))
 ]

Adjectives, on the other hand, appear before the nouns they
modify in English. A slight variant on the above will allow
noun phrases to include adjectives:

@racketblock[
 (define adjectives '(adjective big small red green blue))

 (define (parse-noun-phrase-with-adjective)
   (list 'noun-phrase
         (parse-word articles)
         (parse-word adjectives)
         (parse-word nouns)))

 (define (parse-noun-phrase)
   (define (maybe-add-prep-phrase noun-phrase)
     (amb noun-phrase
          (maybe-add-prep-phrase (list 'noun-phrase
                                       noun-phrase
                                       (parse-prepositional-phrase)))))
   (maybe-add-prep-phrase (amb (parse-simple-noun-phrase)
                               (parse-noun-phrase-with-adjective))))
 ]

While there are multiple types of conjunctions in English,
I'm going to focus on adding conjunctions to of adverbs onto
adverbs and adjectives onto adjectives. I'll allow the
number of conjunctions to be unlimited (even though this
probably won't apply much in practice). We can write
functions that are essentially similar to those which added
prepositional phrases:

@racketblock[
 (define (parse-adverb-phrase)
   (define (maybe-add-conjunction adverb-phrase)
     (amb adverb-phrase
          (maybe-add-conjunction (list 'conjunction
                                       adverb-phrase
                                       (parse-word conjunctions)
                                       (parse-word adverbs)))))
   (maybe-add-conjunction (parse-word adverbs)))
             
 (define (parse-adjective-phrase)
   (define (maybe-add-conjunction adjective-phrase)
     (amb adjective-phrase
          (maybe-add-conjunction (list 'conjunction
                                       adjective-phrase
                                       (parse-word conjunctions)
                                       (parse-word adjectives)))))
   (maybe-add-conjunction (parse-word adjectives)))
 ]

However, you'll notice that adding a sequence of
conjunctions of a single type of word is an essentially
generic operation -- in fact, these two procedures are
almost identical. We can factor out the commonality by
pulling out the parsing of the specific type of word into a thunk:

@racketblock[
 (define (parse-with-conjunctions parser)
   (define (maybe-add-conjunction acc)
     (amb acc
          (maybe-add-conjunction (list 'conjunction
                                       acc
                                       (parse-word conjunctions)
                                       (parser)))))
   (maybe-add-conjunction (parser)))

 (define (parse-adjective-phrase)
   (parse-with-conjunctions (lambda () (parse-word adjectives))))

 (define (parse-adverb-phrase)
   (parse-with-conjunctions (lambda () (parse-word adverbs))))
 ]

Even though it might have been less obvious, I've chosen to
parametize the entire parser instead of just the word list
to use.

@section[#:tag "c4e49"]{Exercise 4.49}

The simplest way to write a generating interpreter is to
rewrite the procedures we already have so that they
eventually call down into a procedure that selects a word
from a word list, rather than parsing one. That would entail
writing the following new code:

@racketblock[
 (define (generate)
   (list 'sentence
         (generate-noun-phrase)
         (generate-verb-phrase)))

 (define (generate-word word-list)
   (list (car word-list) (an-element-of (cdr word-list))))
 
 (define (generate-prepositional-phrase)
   (list 'prep-phrase
         (generate-word prepositions)
         (generate-noun-phrase)))

 (define (generate-simple-noun-phrase)
   (list 'simple-noun-phrase
         (generate-word articles)
         (generate-word nouns)))

 (define (generate-noun-phrase)
   (define (maybe-add-prep-phrase noun-phrase)
     (amb noun-phrase
          (maybe-add-prep-phrase (list 'noun-phrase
                                       noun-phrase
                                       (generate-prepositional-phrase)))))
   (maybe-add-prep-phrase (amb (generate-simple-noun-phrase)
                               (generate-noun-phrase-with-adjective))))

 (define (generate-noun-phrase-with-adjective)
   (list 'noun-phrase
         (generate-word articles)
         (generate-adjective-phrase)
         (generate-word nouns)))

 (define (generate-verb-phrase)
   (define (maybe-add-prep-phrase verb-phrase)
     (amb verb-phrase
          (maybe-add-prep-phrase (list 'verb-phrase
                                       verb-phrase
                                       (generate-prepositional-phrase)))))
   (maybe-add-prep-phrase (amb (generate-word verbs)
                               (generate-verb-phrase-with-adverb))))

 (define (generate-verb-phrase-with-adverb)
   (list 'verb-phrase
         (generate-word verbs)
         (generate-adverb-phrase)))

 (define (generate-with-conjunctions generator)
   (define (maybe-add-conjunction acc)
     (amb acc
          (maybe-add-conjunction (list 'conjunction
                                       acc
                                       (generate-word conjunctions)
                                       (generator)))))
   (maybe-add-conjunction (generator)))

 (define (generate-adjective-phrase)
   (generate-with-conjunctions (lambda () (generate-word adjectives))))

 (define (generate-adverb-phrase)
   (generate-with-conjunctions (lambda () (generate-word adverbs))))
 ]

I don't love this idea. The rules of sentence structure are
now encoded twice -- once for parsing, and again for
generating. If the rules change, almost identical changes
need to be made to each of these. And if we do anything else
with these rules, the problem will get even worse.

The only reason that all of these procedures needed to be
defined again is because they eventually call down into a
single function that needs to be different. However, the API
to this function is always the same -- it takes a word list,
and returns something that will be used to construct a
larger result. Even the combination of the word type and the
word that is either selected or found is currently being
duplicated. I believe that we can do better than this.

Instead of having separate functions for generating and
parsing, we're going to have a single set of "traversal"
functions which are parametized on the function that will be
applied to the root word lists. For ease of use, we're also
going to define a helper function for creating word
traversal functions that provides the structure of creating
a list of the word list type followed by the result, as well
as removing the word list type from the list before calling
the specialized traversal function. (This eliminates the
need to remember to use @tt{(cdr word-list)} once you're
actually looking for words. Without knowing what else might
be done here, this might be a mistake, but it would be
trivial to write a variant function that doesn't do this if
one so desired.) @tt{parse-word} and @tt{generate-word} are
then defined in terms of this function.

Lastly, we have a single @tt{traverse-sentence} function
which encodes the requirement that sentences are composed of
a noun phrase and a verb phrase. @tt{parse} and @tt{generate}
are defined in terms of this.

Here is the entire new implementation:

@racketblock[
 (define (traverse-sentence f)
   (list 'sentence
         (traverse-noun-phrase f)
         (traverse-verb-phrase f)))

 (define (traverse-word f)
   (lambda (word-list)
     (list (car word-list) (f (cdr word-list)))))

 (define parse-word
   (traverse-word
    (lambda (words)
      (require (not (null? *unparsed*)))
      (require (memq (car *unparsed*) words))
      (let ((found-word (car *unparsed*)))
        (set! *unparsed* (cdr *unparsed*))
        found-word))))

 (define generate-word
   (traverse-word an-element-of))

 (define (parse input)
   (set! *unparsed* input)
   (let ((result (traverse-sentence parse-word)))
     (require (null? *unparsed*))
     result))

 (define (generate)
   (traverse-sentence generate-word))

 (define (traverse-prepositional-phrase f)
   (list 'prep-phrase
         (f prepositions)
         (traverse-noun-phrase f)))

 (define (traverse-simple-noun-phrase f)
   (list 'simple-noun-phrase
         (f articles)
         (f nouns)))

 (define (traverse-noun-phrase f)
   (define (maybe-add-prep-phrase noun-phrase)
     (amb noun-phrase
          (maybe-add-prep-phrase (list 'noun-phrase
                                       noun-phrase
                                       (traverse-prepositional-phrase f)))))
   (maybe-add-prep-phrase (amb (traverse-simple-noun-phrase f)
                               (traverse-noun-phrase-with-adjective f))))

 (define (traverse-noun-phrase-with-adjective f)
   (list 'noun-phrase
         (f articles)
         (traverse-adjective-phrase f)
         (f nouns)))

 (define (traverse-verb-phrase f)
   (define (maybe-add-prep-phrase verb-phrase)
     (amb verb-phrase
          (maybe-add-prep-phrase (list 'verb-phrase
                                       verb-phrase
                                       (traverse-prepositional-phrase f)))))
   (maybe-add-prep-phrase (amb (f verbs)
                               (traverse-verb-phrase-with-adverb f))))

 (define (traverse-verb-phrase-with-adverb f)
   (list 'verb-phrase
         (f verbs)
         (traverse-adverb-phrase f)))

 (define (traverse-with-conjunctions f basis)
   (define (maybe-add-conjunction acc)
     (amb acc
          (maybe-add-conjunction (list 'conjunction
                                       acc
                                       (f conjunctions)
                                       (basis)))))
   (maybe-add-conjunction (basis)))

 (define (traverse-adjective-phrase f)
   (traverse-with-conjunctions f (lambda () (f adjectives))))

 (define (traverse-adverb-phrase f)
   (traverse-with-conjunctions f (lambda () (f adverbs))))
 ]

@section[#:tag "c4e50"]{Exercise 4.50}

The simplest way to implement the new @tt{ramb} special form
is as a syntactic transformation into an @tt{amb}. You could
do this even before analyzation. However, this would be
critically wrong -- once analyzed, the selection of the
choices would be fixed. This means that if, for example, you
used this @tt{ramb} in a function, the selection order would
be randomized at the time of the definition of the function,
but every invocation of the function would use the same
list. This won't work.

What will work is deferring the shuffling until the
evaluation of the form. This can be done by writing a slight
variant of @tt{analyze-amb} that will shuffle the choices
@italic{inside} the returned function. This means that you
can use @tt{ramb} inside a function and have it provide
different random values every time, just as I think anyone
would want.

Inserting @tt{ramb} handling into @tt{analyze} is straightforward:

@racketblock[
(define (analyze exp)
  (cond (code:comment @#,elem{...})
        ((ramb? exp) (analyze-ramb exp)) (code:comment @#,elem{Added})
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

 (define (analyze-ramb exp)
   (let ((cprocs (map analyze (ramb-choices exp))))
     (lambda (env succeed fail)
       (define (try-next choices)
         (if (null? choices)
             (fail)
             ((car choices) env
                            succeed
                            (lambda () (try-next (cdr choices))))))
       (try-next (shuffle cprocs)))))
 ]

There are many ways to write @tt{shuffle}, but one way
relies on using a @tt{random} function (which is already
provided in my environment). This generates a random integer
between @tt{0} and the (positive) argument you provide it.
We can use this by continually generating random indices of
values to take from the list, in the end achieving a random
ordering.

@racketblock[
 (define (shuffle xs)
   (define (extract xs idx)
     (define (inner acc rest idx)
       (if (= idx 0)
           (cons (car rest) (append acc (cdr rest)))
           (inner (cons (car rest) acc) (cdr rest) (- idx 1))))
     (inner '() xs idx))
   (define (inner acc rest len)
     (if (= len 0)
         acc
         (let* ((idx (random len))
                (next (extract rest idx)))
           (inner (cons (car next) acc) (cdr next) (- len 1)))))
   (inner '() xs (length xs)))
 ]

@section[#:tag "c4e51"]{Exercise 4.51}

Implementing @tt{permanent-set!} is actually easier than
implementing @tt{set!} was, because it no longer needs to
wrap the innermost failure continuation with code that sets
the value back to what it was before.

First, we have the trivial detection and accessor functions.
@tt{permanent-set?} is added as a case to @tt{analyze}
before @tt{application?}, just like usual:

@racketblock[
 (define (permanent-assignment? exp)
   (tagged-list? exp 'permanent-set!))
 (define (permanent-assignment-variable exp) (cadr exp))
 (define (permanent-assignment-value exp) (caddr exp))
 ]

And here is how we analyze permanent set expressions.

@racketblock[
 (define (analyze-permanent-assignment exp)
   (let ((var (permanent-assignment-variable exp))
         (vproc (analyze (permanent-assignment-value exp))))
     (lambda (env succeed fail)
       (vproc env
              (lambda (val fail2)
                (set-variable-value! var val env)
                (succeed 'ok fail2))
              fail))))
 ]

Compare this to @tt{analyze-assignment} -- you'll see that
it is a strictly simpler variant:

@racketblock[
 (define (analyze-assignment exp)
   (let ((var (assignment-variable exp))
         (vproc (analyze (assignment-value exp))))
     (lambda (env succeed fail)
       (vproc env
              (lambda (val fail2)
                (let ((old-value (lookup-variable-value var env)))
                  (set-variable-value! var val env)
                  (succeed 'ok
                           (lambda ()
                             (set-variable-value! var old-value env)
                             (fail2)))))
              fail))))
 ]

@section[#:tag "c4e52"]{Exercise 4.52}

An @tt{if-fail} expression takes two arguments. The first is
an expression that is always going to be evaluated. If it
successfully evaluates to a value @tt{v}, then the result of
the @tt{if-fail} expression as a whole is @tt{v}. However,
if evaluation fails, then the result of the entire
expression will be the result of evaluating the second
argument. If the evaluation of the alternative fails, then
the evaluation of the entire expression fails.

First, the boilerplate:

@racketblock[
 (define (if-fail? exp) (tagged-list? exp 'if-fail))
 (define (if-fail-value exp) (cadr exp))
 (define (if-fail-alternative exp) (caddr exp))
 ]

Implementing @tt{if-fail} is a little tricker than most of
the special forms we've written so far, because it requires
constructing a new failure continuation and adding it to the
right place (alongside a few other failure continuations
being juggled around).

The one tricky bit is that the new failure continuation
needs to be a thunk -- if you forget this thunk wrapping,
then you will always evaluate the alternative case. If the
main evaluation succeeds, you will get very confusing
results.

This is probably easier to understand by looking at the
code:

@racketblock[
 (define (analyze-if-fail exp)
   (let* ((vproc (analyze (if-fail-value exp)))
          (aproc (analyze (if-fail-alternative exp))))
     (lambda (env succeed fail)
       (vproc env
              (code:comment @#,elem{If the evaluation of the predicate succeeds})
              (lambda (val fail2)
                (succeed val fail2))
              (code:comment @#,elem{If the evaluation of the predicate fails})
              (lambda ()
                (aproc env
                       (lambda (val fail3)
                         (succeed val fail3))
                       fail))))))
 ]

@section[#:tag "c4e53"]{Exercise 4.53}

We are asked to determine the result of evaluating the
following:

@racketblock[
(let ((pairs '()))
  (if-fail (let ((p (prime-sum-pair '(1 3 5 8) '(20 35 110))))
             (permanent-set! pairs (cons p pairs))
             (amb))
           pairs))
 ]

It's important to keep two different levels of failure
continuations straight. The failure continuation constructed
by the @tt{if-fail} expression that leads to @tt{pairs}
being returned is only evaluated if the entire value
expression fails. However, the @tt{(amb)} call inside the
@tt{let} body is not going to immediately cause the entire
expression evaluation to fail -- it's merely going to
backtrack far enough for @tt{prime-sum-pair} to select
another pair, which, after being selected, will be
permanently added to @tt{pairs}. The expression as a whole
will only fail when all of the pairs have been evaluated,
meaning that this is one way to collect all of the prime sum
pairs that will be found.

The three prime sum pairs for these lists are @tt{(3, 20)},
@tt{(3, 113)}, and @tt{(8, 35)}. Given that the first list
controls the outermost iteration, and that both lists are
iterated in order, the pairs will be discovered in this
order, and therefore @italic{prepended} in this order. This
leads to a final result of

@tt{'((8 35) (3 113) (3 20))}

@section[#:tag "c4e54"]{Exercise 4.54}

If we were going to add @tt{require} as a special form
(which, although not strictly necessary, would have some
benefit -- @tt{require} is so ubiquitous that it makes sense
to predefine it in the interpreter in some way), we would
essentially have to evaluate the predicate and immediately
call the relevant failure continuation if the predicate is
not true. This is already evident in the partial source for
@tt{analyze-require} that is provided for this exercise:

@racketblock[
 (define (analyze-require exp)
   (let ((pproc (analyze (require-predicate exp))))
     (lambda (env succeed fail)
       (pproc env
              (lambda (pred-value fail2)
                (if ???
                    ???
                    (succeed 'ok fail2)))
              fail))))
 ]

We can already see that the predicate passed to @tt{if}
should be true if the predicate is false, because the @tt{
 succeed} case is in the alternative position. The only thing
that we need to be careful of is that we call the correct
failure continuation if the predicate is false. This is
relatively straightforward, though -- we should call @tt{
 fail2}, otherwise @tt{amb} expressions in the predicate will
not behave correctly. If we called the outermost @tt{fail}
continuation immediately, then an expression like
@racket[(require (> 1 (amb 0 1 2 3)))] would fully terminate
without recognizing that some of the selectable values would
pass the predicate. This is different from how the original
@tt{require} would behave, as that would continue to search
for a solution (Note: strictly speaking, this implementation
is already different than that of the procedure @tt{
 require}, because the successful return value is @tt{'ok}
instead of @tt{false}.)

Here is the completed function:

@racketblock[
 (define (analyze-require exp)
   (let ((pproc (analyze (require-predicate exp))))
     (lambda (env succeed fail)
       (pproc env
              (lambda (pred-value fail2)
                (if (not pred-value)
                    (fail2)
                    (succeed 'ok fail2)))
              fail))))
 ]

@section[#:tag "c4e55"]{Exercise 4.55}

@bold{All people supervised by Ben Bitdiddle}

@racketblock[(supervisor ?x (Bitdiddle Ben))]

@bold{The names and jobs of all people in the accounting division}

@racketblock[(job ?x (accounting . ?y))]

@bold{The names and addresses of all people who live in Slumerville}

I'm going to treat address as specifying the street name and
number, which are the contents of the list following the
name of the city in a valid address (e.g.
@racket[(Slumerville (Pine Tree Road) 80)]). With that in mind,

@racketblock[(address ?x (Slumerville . ?y))]

@section[#:tag "c4e56"]{Exercise 4.56}

@bold{The names of all people who are supervised by Ben Bitdiddle, together with their addresses}

@racketblock[(and (supervisor ?person (Bitdiddle Ben))
                  (address ?person ?address))]

@bold{All people whose salary is less than Ben Bitdiddle's, together with their salary and Ben Bitdiddle's salary}

@racketblock[(and (salary (Bitdiddle Ben) ?bitdiddle-salary)
                  (salary ?person ?person-salary)
                  (lisp-value > ?bitdiddle-salary ?person-salary))]

@bold{All people who are supervised by someone who is not in the computer division, together with the supervisor's name and job}

I'm going to interpret the division as not being a part of
the name of the job (i.e., the job for
@racket[(computer programmer)] is @racket[(programmer)]).

@racketblock[(and (supervisor ?subordinate ?supervisor)
                  (not (job ?supervisor (computer . ?supervisor-job))))]