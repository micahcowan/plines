.hs.$(OBJEXT):
	$(HC) -c $<

bin_PROGRAMS = plines
plines_SOURCES = PlinesMain.hs OptParse.hs OptBuiltins.hs
dist_noinst_SCRIPTS  = procopts.pl
BUILT_SOURCES  = OptBuiltins.hs
dist_noinst_DATA = options.txt

HC = ghc
LINK = $(HC) -o $@

OptBuiltins.hs: procopts.pl options.txt
	./procopts.pl
