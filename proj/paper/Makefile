
all: paper

tex=pdflatex -shell-escape

paper: *.tex *.bib *.cls
	$(tex) paper.tex
	bibtex paper
	$(tex) paper.tex
	$(tex) paper.tex

clean:
	rm -f *.aux *.bbl *.blg *.log *.pdf *.pyg
