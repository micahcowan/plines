.hs.$(OBJEXT):
	$(HC) -c $<

bin_PROGRAMS = plines
plines_SOURCES = PlinesMain.hs OptParse.hs OptBuiltins.hs
EXTRA_SCRIPTS = procopts.pl
BUILT_SOURCES = OptBuiltins.hs
EXTRA_SOURCES = options.txt

HC = ghc
LINK = $(HC) -o $@

OptBuiltins.hs: procopts.pl options.txt
	./procopts.pl
