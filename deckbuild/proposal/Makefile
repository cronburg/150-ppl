
PDFS=proposal.pdf

all: $(PDFS)

proposal.pdf: proposal.tex
	pdflatex proposal.tex
	pdflatex proposal.tex

clean:
	rm -f *.aux *.log $(PDFS)

