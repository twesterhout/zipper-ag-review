# zipper-ag-review [![Travis Status](https://travis-ci.com/twesterhout/zipper-ag-review.svg?branch=master)](https://travis-ci.org/twesterhout/zipper-ag-review)[![License](https://img.shields.io/badge/license-BSD3-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)[![PDF](https://img.shields.io/badge/pdf-online-blue.svg?style=flat)](https://twesterhout.github.io/zipper-ag-review)

Source code of the review paper on using functional zippers for embedding of attribute grammars in Haskell.

## Getting started

* [Install `stack`](https://docs.haskellstack.org/en/stable/README).

* Download the repo
  ```bash
  git clone https://github.com/twesterhout/zipper-ag-review.git
  cd zipper-ag-review
  ```

* Let stack download and install the appropriate version of GHC
  ```bash
  stack setup
  ```

* Compile the code
  ```bash
  stack build
  ```

And you are good to go. Execute `stack exec paper` to run a few tests/examples.

If you would like to build the PDF locally, you need `lhs2TeX` (>= 1.21) to
produce the `.tex` file and `pdflatex` + `bibtex` to compile it. `lhs2TeX`
can be installed using `stack`:
```bash
stack install lhs2tex-1.22 # or 1.21
```
`pdflatex` and `bibtex` are probably best installed using your distribution's
package manager. After that executing `./build-pdf.sh` should do the trick, but
if it doesn't, feel free to tweak it a bit.

## Contributing

Comments, bug reports, and, especially, pull requests are *very welcome*! All PRs
are checked using Travis CI and a successful build there is a requirement for
being accepted. The only other thing I ask for is if you have written a piece
of Haskell code, please, do format it using
[`brittany`](https://github.com/lspitzner/brittany).

