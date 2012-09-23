
# IMPORTS = $(shell ./libs.hs)
# UHC 	 = uhc                                   
# LIBS	 = 
# COMPILER = ${UHC} ${IMPORTS} --import-path=$LIBS$ -tjs -O,2

# COMPILER = uhc -tjs -O,2 --import-path src --import-path uhcjs
PRE_COMPILER = hastec \
	--libinstall
COMPILER = hastec \
	--debug \
	--out=main.js \
	--with-js=lib/processing/processing.js,animator.js

# BROWSER  = Google Chrome
BROWSER  = Firefox
MAIN=main
 
all: build post reload

pre:
	$(PRE_COMPILER) \
		src/Animator/Animation.hs   \
		src/Animator/Random.hs      \
		src/Data/AdditiveGroup.hs   \
		src/Data/AffineSpace.hs     \
		src/Data/Basis.hs           \
		src/Data/Cross.hs           \
		src/Data/Derivative.hs      \
		src/Data/LinearMap.hs       \
		src/Data/List/NonEmpty.hs   \
		src/Data/Maclaurin.hs       \
		src/Data/MemoTrie.hs        \
		src/Data/NumInstances.hs    \
		src/Data/Semigroup.hs       \
		src/Data/VectorSpace.hs     \
		src/Data/Void.hs            \
		src/Numeric/Natural.hs;

build:
	$(COMPILER) $(MAIN).hs \
		src/Animator/Animation.hs   \
		src/Animator/Random.hs; \
    perl -pi -e 's/window.onload = (function.*);/animator_ready($$1);/g' main.js;

post:
	rm -f `find . -d -name "*.core*"`
	rm -f `find . -d -name "*.hi*"`
	rm -f `find . -d -name "*.mjs*"`
	rm -f `find . -d -name "*.clo*"`
	rm -f `find . -d -name "*.jsmod*"`
	rm -f `find . -d -name "*.o*"`
 
FILE=$(MAIN).js
optimize:              
	googleclosure \
		--language_in ECMASCRIPT5 \
		--compilation_level SIMPLE_OPTIMIZATIONS \
		--js 			 $(FILE) \
		--js_output_file $(FILE)_opt; \
		rm $(FILE); \
		mv $(FILE)_opt $(FILE);

.PHONY reload:
	sh reload.sh $(BROWSER)

update-libraries: update-paperjs update-processing update-closure-library

PAPERJS_URL=http://paperjs.org/downloads/paperjs-nightly.zip
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

PROCESSING_URL=http://cloud.github.com/downloads/processing-js/processing-js/processing-1.4.1-api.min.js
update-processing:
	rm -rf lib/processing; \
	mkdir -p lib/processing; \
	cd lib/processing; \
	curl $(PROCESSING_URL) > processing.js; \
	cd ..;

CLOSURE_LIBRARY_URL=http://closure-library.googlecode.com/files/closure-library-20120710-r2029.zip
update-closure-library:
	rm -rf lib/closure-library; \
	mkdir -p lib/closure-library; \
	cd lib; \
	curl $(CLOSURE_LIBRARY_URL) > closure-library.zip; \
	cd closure-library; \
	unzip ../closure-library.zip; \
	cd ..; \
	rm -f closure-library.zip; \
	cd ..;

DOMREADY_URL=http://domready.googlecode.com/files/domready.js
update-domready:
	rm -rf lib/domready; \
	mkdir -p lib/domready; \
	cd lib/domready; \
	curl $(DOMREADY_URL) > domready.js; \
	cd ..;


server-start:
	(python -m SimpleHTTPServer 5566 &) > /dev/null 2>&1

server-stop:
	killall python

clean:
	rm -f `find . -d -name "*.js*"`


