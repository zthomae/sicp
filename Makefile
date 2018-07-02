docs: docs.scrbl ex1.scrbl ex2.scrbl ex3.scrbl ex4.scrbl eval.rkt
	rm -rf docs/
	raco scribble --htmls --dest . docs.scrbl
cover: scratch/4/amb.rkt scratch/4/amb-test.rkt
	raco cover scratch/4/amb.rkt scratch/4/amb-test.rkt
