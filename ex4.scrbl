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