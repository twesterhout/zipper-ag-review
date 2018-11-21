#!/bin/bash

#
# This file can be used as an example of how to build the PDF locally.
#

set -e

_build_using_latexrun()
{
	latexrun -O "$1" "$2"
}

_build_using_pdflatex()
{
	declare -r output_dir="$1"
	declare -r src_file="$2"
	declare -r basename="${src_file%.tex}"
	pdflatex -interaction nonstopmode -output-directory "${output_dir}" "${src_file}"
	pdflatex -interaction nonstopmode -output-directory "${output_dir}" "${src_file}"
	bibtex "${output_dir}/${basename}.aux"
	pdflatex -interaction nonstopmode -output-directory "${output_dir}" "${src_file}"
	pdflatex -interaction nonstopmode -output-directory "${output_dir}" "${src_file}"
	mv -v "${output_dir}/${basename}.pdf" "."
}

_build()
{
	[[ $# -eq 2 ]] || { printf "Usage: _build <output_dir> <tex_file>"; return 1; }
	if ! which latexrun; then
		_build_using_latexrun "$1" "$2"
	else
		_build_using_pdflatex "$1" "$2"
	fi
}


stack --no-terminal exec --cwd paper -- lhs2TeX Paper.lhs --tt --set=abstract_only -o Paper.tex
pushd paper/
mkdir -pv _tex_build/
_build _tex_build Paper.tex
popd
rm -v paper/Paper.tex
mv -v paper/Paper.pdf .
rm -rv paper/_tex_build

