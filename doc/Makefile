DOCBOOK_HTML  = /usr/share/sgml/docbook/stylesheet/dsssl/modular/html
DOCBOOK_PRINT = /usr/share/sgml/docbook/stylesheet/dsssl/modular/print
SRC = $(PWD)/src

.PHONY: html tex man

default: html man QUICKSTART
# 'tex' still not working

html: html/book1.htm
tex: tex/findlib.dvi


html/book1.htm: src/*.sgml src/*.mod QUICKSTART.xml common.xml config.xml
	mkdir -p html
	cd html; \
	rm -f *.htm*; \
	jade -t sgml -D$(DOCBOOK_HTML) -D$(SRC) findlib.sgml; \
	true
	readme -html QUICKSTART.xml >html/quickstart.html
	touch html/TIMESTAMP

src/findlib_reference.xml: src/*.sgml src/*.mod
	sgml2xml -D$(DOCBOOK_HTML) -D$(SRC) \
		findlib_reference.sgml >src/findlib_reference.xml ; \
	true

man: src/findlib_reference.xml
	mkdir -p man
	cd man; \
	rm -f *.[0-9]; \
	db2man <../src/findlib_reference.xml

tex/findlib.tex: src/*.sgml src/*.mod
	mkdir -p tex
	cd tex;
	jade -t tex -D$(DOCBOOK_PRINT) -D$(SRC) findlib.sgml; \
	true

tex/findlib.dvi: tex/findlib.tex
	cd tex;
	jadetex findlib.tex

.SUFFIXES: .xml .sgml

.sgml.xml: 
	sx -xndata $< >$@; true



clean:
	rm -rf html man tex
	rm -f src/*.xml

distclean:
	rm -f src/*.xml
	rm -f src/*~
	rm -f *~

QUICKSTART: QUICKSTART.xml
	readme -text QUICKSTART.xml >QUICKSTART
