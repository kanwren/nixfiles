DOCNAME ?= main

MAIN = $(DOCNAME).tex
SRC = $(glob *.tex)

.PHONY: all
all: $(DOCNAME).pdf

$(DOCNAME).pdf: $(MAIN) $(SRC)
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -jobname="$(DOCNAME)" -use-make "$(MAIN)"

.PHONY: watch
watch: $(MAIN) $(SRC)
	latexmk -pvc -pdf -pdflatex="pdflatex -interaction=nonstopmode" -jobname="$(DOCNAME)" -use-make "$(MAIN)"

.PHONY: clean
clean:
	latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -jobname="$(DOCNAME)" -use-make "$(MAIN)" -CA

.PHONY: install
install:
	mkdir -pv ${out}/nix-support
	cp $(DOCNAME).pdf ${out}/nix-support
