# Makefile for creating the R package hiker

PKGNAME := hiker
PKGVERS = $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" ./$(PKGNAME)/DESCRIPTION)
PKGTAR = $(PKGNAME)_$(PKGVERS).tar.gz
TEXCMD := pdflatex
RFILES := Allclasses.R Allgenerics.R score.R hiker.R data.R HikerMethods.R PtbbMethods.R
DFILES := SP500.rda

all: deps tex pdf pkg check
tex: $(PKGNAME).tex
pdf: $(PKGNAME).pdf

deps:
	Rscript -e 'if (!require("devtools")) install.packages("devtools")'
	Rscript -e 'if (!require("Rnoweb")) install.packages("Rnoweb_1.1.tar.gz", repos = NULL, type="source")'

$(PKGNAME).tex: $(PKGNAME).Rnw
	Rscript -e 'library(Rnoweb); noweb("$(PKGNAME).Rnw", tangle = FALSE)'

$(PKGNAME).pdf: $(PKGNAME).tex
	$(TEXCMD) $<
	$(TEXCMD) $<
	bibtex $(PKGNAME).aux
	$(TEXCMD) $<
	$(TEXCMD) $<

pkg: $(PKGNAME).Rnw
	Rscript -e 'library(Rnoweb); noweb("$(PKGNAME).Rnw", weave = FALSE)'
# creating package skeleton
	if [ ! -d "$(PKGNAME)" ]; then mkdir $(PKGNAME);  fi
	if [ ! -d "$(PKGNAME)/R" ]; then mkdir $(PKGNAME)/R;  fi
	if [ ! -d "$(PKGNAME)/data" ]; then mkdir $(PKGNAME)/data;  fi
	if [ ! -d "$(PKGNAME)/inst" ]; then mkdir $(PKGNAME)/inst;  fi
	if [ ! -d "$(PKGNAME)/inst/doc" ]; then mkdir $(PKGNAME)/inst/doc;  fi
# handling R files
	find ./$(PKGNAME)/R/ -type f -delete
	mv DESCRIPTION.R $(PKGNAME)/DESCRIPTION
	mv $(RFILES) $(PKGNAME)/R/
	cp $(DFILES) $(PKGNAME)/data/
# handling man files
	if [ ! -d "$(PKGNAME)/man" ]; then mkdir $(PKGNAME)/man;  fi
	find ./$(PKGNAME)/man/ -type f -delete
	Rscript -e 'library(devtools); devtools::document(pkg = "./$(PKGNAME)")'
# handling pdf file (literate programming)
	cp $(PKGNAME).pdf $(PKGNAME)/inst/doc/$(PKGNAME).pdf
# building the source tarball
	R CMD build $(PKGNAME)

check: pkg
	R CMD check $(PKGTAR)

clean:
	$(RM) -r $(PKGNAME).Rcheck/
	$(RM) $(PKGNAME).aux $(PKGNAME).log $(PKGNAME).out $(PKGNAME).bbl $(PKGNAME).blg


