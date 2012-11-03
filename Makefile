
JSPP 		= cpp -P -CC
JSLINT		= build/jslint.sh
JSC		= hastec \
			-O2 \
			--debug \
			--out=main.js \
			--with-js=src-js/animator.jspp,lib/processing/processing.js,lib/jquery/jquery.js
JSC_RELEASE    	= hastec \
			-O2 \
			--out=main.js \
			--with-js=src-js/animator.jspp,lib/processing/processing.js,lib/jquery/jquery.js
CLOSURE		= googleclosure
BROWSER  	= Google Chrome

FLAGS  	    	= -DENABLE_TYPE_CHECKS=1
MAIN	 	= main

PAPERJS_URL	= http://paperjs.org/downloads/paperjs-nightly.zip
PROCESSING_URL	= http://cloud.github.com/downloads/processing-js/processing-js/processing-1.4.1-api.min.js
DOMREADY_URL	= http://domready.googlecode.com/files/domready.js
JQUERY_URL	= http://code.jquery.com/jquery-1.8.2.min.js
JSLINT_URL	= https://raw.github.com/douglascrockford/JSLint/master/jslint.js

all: 		debug
debug:  	post reload
release: 	post optimize reload

jspp:
	$(JSPP) $(FLAGS) src-js/animator.js src-js/animator.jspp;

lint: 		jspp
	$(JSLINT) src-js/animator.jspp;

build: 		lint
	$(JSC) \
		src/Animator/Animation.hs \
		src/Animator/Prelude.hs \
		src/Animator/Internal/Prim.hs \
		$(MAIN).hs && \
	perl -pi -e 's/window.onload = (function.*);/jQuery(document).ready($$1);/g' main.js;

post:		build
	rm -f `find . -d -name "*.jspp"`
	rm -f `find . -d -name "*.core*"`
	rm -f `find . -d -name "*.hi*"`
	rm -f `find . -d -name "*.mjs*"`
	rm -f `find . -d -name "*.clo*"`
	rm -f `find . -d -name "*.jsmod*"`
	rm -f `find . -d -name "*.o*"`
	rm -rf Animator

optimize:
	$(CLOSURE) \
		--language_in ECMASCRIPT5 \
		--compilation_level SIMPLE_OPTIMIZATIONS \
		--js 			 $(MAIN).js \
		--js_output_file $(MAIN).jsopt; \
		rm $(MAIN).js; \
		mv $(MAIN).jsopt $(MAIN).js;

reload:
	sh build/reload.sh $(BROWSER)

clean:
	rm main.js

haddock:
	cabal haddock --hyperlink-source

server-start:
	(python -m SimpleHTTPServer 5566 &) > /dev/null 2>&1

server-stop:
	killall python

update-lib: 	update-paperjs update-processing update-domready update-jquery update-jslint

update-paperjs:
	rm -rf lib/paperjs; \
	mkdir -p lib/paperjs; \
	cd lib; \
	curl $(PAPERJS_URL) > paperjs.zip; \
	cd paperjs; \
	unzip ../paperjs.zip; \
	cd ..; \
	rm -f paperjs.zip; \
	cd ..;

update-processing:
	rm -rf lib/processing; \
	mkdir -p lib/processing; \
	cd lib/processing; \
	curl $(PROCESSING_URL) > processing.js; \
	cd ..;

update-domready:
	rm -rf lib/domready; \
	mkdir -p lib/domready; \
	cd lib/domready; \
	curl $(DOMREADY_URL) > domready.js; \
	cd ..;

update-jquery:
	rm -rf lib/jquery; \
	mkdir -p lib/jquery; \
	cd lib/jquery; \
	curl $(JQUERY_URL) > jquery.js; \
	cd ..;

update-jslint:
	rm -rf lib/jslint; \
	mkdir -p lib/jslint; \
	cd lib/jslint; \
	curl $(JSLINT_URL) > jslint.js; \
	cd ..;


.PHONY: 	build
