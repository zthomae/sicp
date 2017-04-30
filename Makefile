public: public.scrbl ex1.scrbl ex2.scrbl ex3.scrbl ex4.scrbl eval.rkt
	rm -rf public/
	raco scribble --htmls --dest . public.scrbl
