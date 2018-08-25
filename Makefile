.PHONY: deps cover

scribblings := $(wildcard *.scrbl)
test_files := $(shell find scratch/ -type f -name '*-test.rkt')
scratch_files := $(patsubst %-test.rkt,%.rkt, $(test_files))

public: deps $(scribblings) eval.rkt
	rm -rf public/
	raco scribble --htmls --dest . public.scrbl
deps:
	raco pkg install --auto --skip-installed sicp cover mock mock-rackunit
cover: deps $(test_files) $(scratch_files)
	raco cover $(test_files) $(scratch_files)
