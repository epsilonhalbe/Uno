all :
	ghc --make -Wall Uno.hs

clean :
	rm -f Uno tUno *.hi *.o report.html

test :
	ghc --make -Wall tUno.hs
	./tUno

prove :
# cpan App::Prove
	prove --exec make test


provehtml :
# cpan App::Prove::Plugin::HTML
	prove -P HTML=outfile:report.html --exec make test
