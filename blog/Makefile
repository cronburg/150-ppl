
FN=blog-handout
all: $(FN).pdf

TEXARGS="-shell-escape"

$(FN).pdf: $(FN).tex
	pdflatex $(TEXARGS) $<
	pdflatex $(TEXARGS) $<

clean:
	rm -f $(FN).pdf *.aux *.log *.out

