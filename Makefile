all :
	ghc --make -Wall Uno.hs

clean :
	rm -f Uno tUno tColor *.hi *.o report.html

test :
	ghc --make -Wall tUno.hs
	ghc --make -Wall tColor.hs

	./tUno
	./tColor

prove :
# cpan App::Prove
	prove --exec make test


provehtml :
# cpan App::Prove::Plugin::HTML
	prove -P HTML=outfile:report.html --exec make test
