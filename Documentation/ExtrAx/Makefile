PAPER = ExtrAx
TEX = $(wildcard *.tex)
BIB = ./ExtrAx.bib
FIGS = $(wildcard img/*.pdf)

.PHONY: all clean

all: $(PAPER).pdf

$(PAPER).pdf: $(TEX) $(BIB) $(FIGS) 
	echo $(FIGS)
	pdflatex $(PAPER)
	bibtex $(PAPER)
	pdflatex $(PAPER)
	pdflatex $(PAPER)

clean:
		rm -f *.aux *.bbl *.blg *.log *.out $(PAPER).pdf *~ Manual.toc Manual.dvi *.toc

cr: $(PAPER).pdf
	pdftops $(PAPER).pdf -level3
	mv $(PAPER).pdf $(PAPER).src.pdf
	ps2pdf14 -dPDFSETTINGS=/prepress -dEmbedAllFonts=true $(PAPER).ps
	cp $(PAPER).pdf $(PAPER).cr.pdf
