EXEC=trader
INCL=-I src/ -thread -linkpkg -package core,async,uri,yojson,cohttp.async

default: build

run:
	bin/trader

build:
	ocamlfind ocamlc -c $(INCL) src/scraper.mli src/scraper.ml
	ocamlfind ocamlc $(INCL) src/*.ml -o bin/$(EXEC)
	make clean

clean:
	rm src/*.cm*	
	rm -rf _build/
	rm -rf src/*~ *~
