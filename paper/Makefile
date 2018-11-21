type=lhs
file=Paper

main: $(file).lhs zippers.lhs ags.lhs
	lhs2TeX --poly $(file).lhs > $(file).tex
	pdflatex $(file).tex
	bibtex $(file)
	if grep 'There were undefined references' $(file).log ; then \
		bibtex $(file); pdflatex $(file).tex ; fi
	while grep 'Rerun to get cross-references right.' $(file).log ; do \
		pdflatex $(file).tex || { rm -f $*.aux $*.idx && false ; } ; done

talk:
	lhs2TeX --poly talk.lhs > talk.tex
	pdflatex talk.tex


bib:
	lhs2TeX --poly $(file).lhs > $(file).tex
	pdflatex $(file).tex
	bibtex $(file)

clean:
	rm -rf *.aux *.log *.bbl *.bak *.ptb *.blg *.out *.spl *.acn
	rm -rf *.acr *.alg *.glg *.glo *.gls *.idx *.ilg *.ind *.lof
	rm -rf paper.tex *.toc *.ist *.lot paper.pdf