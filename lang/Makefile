TARBALL = racket-sicp.tgz
PLANET_FILES = info.rkt main.rkt sicp-tool.rkt sicp.scrbl sicp-small.png test.rkt uninstall.rkt lang/reader.rkt rogers.jpg Makefile

PLTCOLLECTS = $(CURDIR:/sicp=):

default: compile

clean:
	/bin/rm *~ lang/*~ 2> /dev/null || exit 0
	/bin/rm -r compiled lang/compiled doc 2> /dev/null || exit 0

compile:
	PLTCOLLECTS="$(PLTCOLLECTS)" setup-plt -l sicp

planet: clean compile
	if [ -e sicp ] ; then echo "ERROR: REMOVE sicp" ; false ; else true ; fi
	mkdir sicp
	tar cf - $(PLANET_FILES) | (cd sicp && tar xvpf -)
	planet create sicp
	rm -rf sicp

planet-links:
	-planet unlink neil sicp.plt 1 15
	planet link neil sicp.plt 1 16 $(CURDIR)

view:
	firefox file:doc/sicp/index.html

tarball: $(TARBALL)

$(TARBALL):
	tar cvfz $(TARBALL) $(PLANET_FILES)
