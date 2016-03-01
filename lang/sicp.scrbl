#lang scribble/doc
@; $Id: sicp.scrbl,v 1.55 2012-06-25 00:32:21 user Exp $
@(require scribble/manual)

@title[#:version "1.17"]{@bold{SICP} Support for DrRacket}
@author{Neil Van Dyke}

@section[#:tag "introduction"]{Introduction}

@emph{NOTE: This package is looking for a volunteer to take over development.
The most important need is for someone to work through SICP and fix anything
that would be a problem.  The second need is to incorporate Soegaard's code
into this package, so that we can avoid confusing install-time error messages.
Please contact @link["http://www.neilvandyke.org/"]{Neil Van Dyke} (neil AT
neilvandyke DOT org).}

This package implements support in PLT's
@link["http://www.racket-lang.org/"]{DrRacket} for Abelson and Sussman's
@link["http://mitpress.mit.edu/sicp/"]{@emph{Structure and Interpretation of
Computer Programs}} (@as-index{SICP}) 2nd Edition textbook, for students who
wish to use DrRacket's tools for the SICP programming assignments.

Note that this package is not a project of the PLT organization, nor of the
SICP authors.  PLT has a related textbook, @link["http://htdp.org/"]{@emph{How
to Design Programs}} (@as-index{HtDP}), with a different emphasis and a
different pedagogic approach.  A comparison of the two textbooks is beyond the
scope of this document.  A 2004 paper by some PLT members,
``@link["http://www.ccs.neu.edu/scheme/pubs/jfp2004-fffk.pdf"]{The Structure
and Interpretation of the Computer Science Curriculum}'' [PDF], provides a PLT
perspective on the differences.

The official Web page for this package is:
@link["http://www.neilvandyke.org/racket-sicp/"]{http://www.neilvandyke.org/racket-sicp/}

This package also uses Sperber and Soegaard's Picture Language implementation
by @tt{require}-ing from PLaneT.  (We have tentative plans to merge the two,
time permitting.)

@section[#:tag "installation"]{Installation}

Installing this package consists simply of @tt{require}-ing it from the
Internet-based PLaneT software repository, and then restarting DrRacket.  The
steps in detail for how a beginner may do this through DrRacket are:

@itemize[

@item{0. If you have a previous verson of this package from PLaneT installed,
remove it.  If the version is 1.9 or later, go to the @onscreen{Interactions}
subwindow in @onscreen{SICP} language, and evaluate the command
@scheme[(uninstall-sicp)].  If you have an version older than 1.9 installed,
use the @tt{planet show} and @tt{planet remove} operating system command-line
commands to remove it.}

@item{1. Make sure that the computer connected to the Internet.}

@item{2. Start DrRacket.}

@item{3. Change the DrRacket Language to use @bold{#lang}.  Specifically,
Specifically, select @menuitem["Language" "Choose Language..."] to get the
@onscreen{Choose Language} dialog, then select @onscreen{Use the language
declared in the source} or (in older versions) @onscreen{Module}, and click
@onscreen{OK}.}

@item{4. In the top @onscreen{Definitions} subwindow, change the contents to
read:

@verbatim[
"#lang planet neil/sicp\n"
]}

@item{5. Click the @onscreen{Run} toolbar button.}

@item{6. Wait for the @onscreen{>} prompt to appear in the
@onscreen{Interactions} subwindow, which may take a few minutes, due to
compiling PLT documentation.  Then quit DrRacket and start it again.}

]

@section[#:tag "usage"]{Usage}

There are two separate ways to specify use of the SICP Language in DrRacket:

@itemize[

@item{Change the Language to @onscreen{SICP}, such as by using
@menuitem["Language" "Choose Language..."].  This is the normal way.}

@item{When using the @onscreen{Module} language, begin the Scheme file with the
line:

@verbatim[
"#lang planet neil/sicp\n"
]}

]

This package uses the
@link["http://planet.plt-scheme.org/display.ss?package=sicp.plt&owner=soegaard"]{soegaard/sicp}
PLaneT package by Mike Sperber and Jens Axel Soegaard to implement the SICP
Picture Language.  Note that you should @emph{not} use the @tt{require} form
shown in the documentation for that package -- this package provides the SICP
Picture Language implicitly.

@section[#:tag "history"]{History}

Note that PLaneT package version numbers of @tt{neil/sicp} are used, rather
than maintaning separate version numbers.

@itemize[

@item{Version 1.17 -- 2012-06-24 -- Updated installation documentation for recent DrRacket version.  Updated note about seeking new maintainer.}

@item{Version 1.16 -- 2011-09-30 -- @tt{info.rkt} changes.  Documentation
changes.  Fix to message in @tt{uninstall-sic}.}

@item{Version 1.15 -- 2011-05-14 -- Run now prints the values of expressions in
the Definitions window (requested by Brian Sniffen).}

@item{Version 1.14 -- 2011-05-02 -- Changed names and URLs to reflect migration
from PLT Scheme to Racket.}

@item{Version 1.13 -- 2009-05-28 -- Fixed SICP streams support.  Changed
@tt{read-square-bracket-as-paren} to @tt{#t}.  Consolidated tests thus far into
file @tt{test.ss}.}
     
@item{Version 1.12 -- 2009-05-17 -- Added @tt{rogers} painter, using publicity
photo of Mister Rogers.}
              
@item{Version 1.11 -- 2009-05-17 -- Fixed recent bug in @tt{check-expect} and
@tt{check-expect-approx}.  Started @tt{test-ch-2.ss}.}

@item{Version 1.10 -- 2009-05-17 -- Fixed packaging problem and documentation
problem.}
     
@item{Version 1.9 -- 2009-05-17 -- Made @tt{cons-stream} use immutable
pairs.  (Thanks to Vinay Sachdev for reporting the @tt{cons-stream} problem
affecting SICP sec. 3.5.1.)  Added documentation about installing prior
versions before installing.  Added @tt{uninstall-sicp} procedure.  Changed
@tt{info.ss} to permit multiple versions to be installed simultaneously.  Added
@tt{sicp-small.png} icon.  Improved installation documentation.}

@item{Version 1.8 -- 2009-05-16 -- Added @tt{random} procedure for SICP
Exercise 1.22.  Finished first pass of @tt{test-ch-1.ss}.}

@item{Version 1.7 -- 2009-05-15 -- Added @tt{check-expect} syntax, for test
suites.  Added @tt{inc} and @tt{dec} procedures for SICP sec. 1.2.1
Exercises. Added @tt{random} procedure for SICP sec. 1.2.6.  When using this
package from a normal PLT collection, rather than from PLaneT, the language
name is now @onscreen{SICP} rather than @onscreen{SICP (Non-PLaneT)}. Added
partial test suite for SICP ch. 1.  Minor documentation changes.}

@item{Version 1.6 -- 2009-05-05 -- Installation documentation change.}

@item{Version 1.5 -- 2009-05-05 -- Installation documentation change.}

@item{Version 1.4 -- 2009-05-05 -- Changes to work correctly with PLaneT and
cause fewer conflicts from mixing multiple versions in @tt{PLTCOLLECTS} and
PLaneT cache at once.}

@item{Version 1.3 -- 2009-05-04 -- PLaneT packaging corrections and addition
of index entries.}

@item{Version 1.2 -- 2009-05-04 -- PLaneT packaging correction.}

@item{Version 1.1 -- 2009-05-04 -- PLaneT packaging corrections.  Also
switched to using PLaneT package version numbers.}

@item{Version 1.0 -- 2009-05-04 -- Documentation added.  First public
release.}

@item{No Version -- 2009-04 -- Initial version by Neil Van Dyke.  Uses Sperber
and Soegaard's Picture Language implementation by @tt{require}-ing from PLaneT.
Non-public distribution.}

]

@section[#:tag "legal"]{Legal}

Copyright (c) 2009--2011 Neil Van Dyke.  This program is Free Software; you can
redistribute it and/or modify it under the terms of the GNU Lesser General
Public License as published by the Free Software Foundation; either version 3
of the License (LGPL 3), or (at your option) any later version.  This program
is distributed in the hope that it will be useful, but without any warranty;
without even the implied warranty of merchantability or fitness for a
particular purpose.  See http://www.gnu.org/licenses/ for details.
