docs: docs.scrbl ex1.scrbl ex2.scrbl ex3.scrbl eval.rkt
	rm -rf docs/
	raco scribble --htmls --dest . docs.scrbl
