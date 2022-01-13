DOCNAME ?= main

MAIN = $(DOCNAME).tex
SRC = $(glob *.tex)

.PHONY: all
all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(MAIN) $(SRC)
	tectonic $(MAIN)

.PHONY: watch
watch: $(MAIN) $(SRC)
	echo $(MAIN) | entr tectonic /_

.PHONY: clean
clean:
	rm $(DOCNAME).pdf
