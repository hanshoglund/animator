
# IMPORTS = $(shell ./libs.hs)
# UHC 	 = uhc                                   
# LIBS	 = 
# COMPILER = ${UHC} ${IMPORTS} --import-path=$LIBS$ -tjs -O,2

COMPILER = uhc -tjs -O,2 --import-path src --import-path uhcjs
# BROWSER  = Google Chrome
BROWSER  = Firefox
MAIN=main
 
all: build post reload

build:
	$(COMPILER) $(MAIN).hs

post:
	rm -f `find . -d -name "*.core*"`
	rm -f `find . -d -name "*.hi*"`
	rm -f `find . -d -name "*.mjs*"`
 
FILE=$(MAIN).js
optimize:              
	googleclosure \
		--language_in ECMASCRIPT5 \
		--compilation_level WHITESPACE_ONLY \
		--js 			 $(FILE) \
		--js_output_file $(FILE)_opt; \
		rm $(FILE); \
		mv $(FILE)_opt $(FILE);

.PHONY reload:
	sh reload.sh $(BROWSER)

clean:
	rm -f `find . -d -name "*.js*"`


