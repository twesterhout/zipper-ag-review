# NOTE(twesterhout): This Makefile is **not** supported officially, because it
# uses your system's lhs2TeX and different formatting. If you manage to build a
# PDF locally using this Makefile, don't be surprised when
#    a) the document you produced locally differs from the one you see on the
#       project's website;
#    b) your build fails on Travis CI and I reject your PR.
# So use this Makefile at your own risk!!

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
