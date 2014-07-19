SCRAPEREXEC=start_scraper
INCL=-I src/ -thread -linkpkg -package core,async,uri,yojson,cohttp.async

default: build

run-scraper:
	bin/start_scraper

build:
	ocamlfind ocamlc -c $(INCL) src/scraper.mli src/scraper.ml
	ocamlfind ocamlc $(INCL) src/scraper.ml src/start_scraper.ml -o bin/$(SCRAPEREXEC)
	make clean

clean:
	rm src/*.cm*	
	rm -rf _build/
	rm -rf src/*~ *~
