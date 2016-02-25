solutions: solutions.scrbl ex1.scrbl ex2.scrbl ex3.scrbl eval.rkt
	rm -rf solutions/
	raco scribble --htmls --dest . solutions.scrbl

push: solutions
	git subtree push --prefix solutions origin gh-pages

.PHONY: push
