SCRAPEREXEC=start_scraper
TESTEXEC=test_analyzer
INCL=-I src/ -thread -linkpkg -package core,async,uri,yojson,cohttp.async,graphics

default: build

run-scraper: build
	bin/$(SCRAPEREXEC)

hist-scraper: build
	bin/hist_scraper ${ARGS}

tests: build
	bin/$(TESTEXEC)

build:
	cd src && ocamlbuild -use-ocamlfind $(SCRAPEREXEC).native
	mv src/$(SCRAPEREXEC).native bin/$(SCRAPEREXEC)
	cd src/ && ocamlbuild -use-ocamlfind $(TESTEXEC).native
	mv src/$(TESTEXEC).native bin/$(TESTEXEC)
	cd src/ && ocamlbuild -use-ocamlfind test_historical_scraper.native
	mv src/test_historical_scraper.native bin/hist_scraper

clean:
	rm -rf src/*~ *~
	rm src/*.cm*	
