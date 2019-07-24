.PHONY: deps test_deps cover

test_dependencies = cover mock mock-rackunit

pkg_install = raco pkg install --auto --skip-installed

scribblings := $(wildcard *.scrbl)
test_files := $(shell find scratch/ -type f -name '*-test.rkt')
scratch_files := $(patsubst %-test.rkt,%.rkt, $(test_files))

docs: deps $(scribblings) eval.rkt
	rm -rf docs/
	raco scribble --htmls --dest . docs.scrbl
deps:
	cd sicp; $(pkg_install)
test-deps:
	$(pkg_install) $(test_dependencies)
cover: deps test-deps $(test_files) $(scratch_files)
	raco cover $(test_files) $(scratch_files)
