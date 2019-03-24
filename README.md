# SICP Solutions

**Officially hosted on [GitLab](http://zthomae.gitlab.io/sicp/)**

These are my solutions to the exercises in the
[Structure and Interpretation of Computer Programs](https://mitpress.mit.edu/sicp/full-text/book/book.html)
written with [Scribble](http://docs.racket-lang.org/scribble/). Occasionally
I make progress on these.

These solutions are not complete: I have not reached the end of the book, nor
have I done every exercise I've seen so far. I intend to finish them all at
some point, but during my first pass through the book, I can and will skip
some.

Since I'm doing these for personal enlightenment, I reserve the right to do
something other than what the exercise specifically asks for if I think I
would learn more by doing so.

## Setup

In order to build the solutions, you need the [SICP package](https://github.com/sicp-lang/sicp).
A working version of this library has been included in the `sicp` subdirectory. This
package is installed automatically when generating the output HTML.

To build the solution documentation, run the following:

```bash
$ make
```

To run the (very few) tests on the scratch files and generate coverage data, run the following:

```bash
$ make cover
```
