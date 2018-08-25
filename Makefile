.PHONY: deps test_deps cover

dependencies = sicp
test_dependencies = cover mock mock-rackunit

pkg_install = raco pkg install --auto --skip-installed

scribblings := $(wildcard *.scrbl)
test_files := $(shell find scratch/ -type f -name '*-test.rkt')
scratch_files := $(patsubst %-test.rkt,%.rkt, $(test_files))

public: deps $(scribblings) eval.rkt
	rm -rf public/
	raco scribble --htmls --dest . public.scrbl
deps:
	$(pkg_install) $(dependencies)
test-deps:
	$(pkg_install) $(test_dependencies)
cover: deps test-deps $(test_files) $(scratch_files)
	raco cover $(test_files) $(scratch_files)
