# infereportR 0.1.0

* The adapter for precomputed DescrTab2 tables (`import_descrtab2()`,
  `render_descrtab2()`) is removed, and DescrTab2 is no longer suggested.
  DescrTab2 inspired the suite and is acknowledged and cited; the tests it
  computes come from other packages, which infercalcR uses as optional
  originals next to its own base-R implementations.
* The README and every vignette now acknowledge and cite DescrTab2
  (Institute of Medical Biometry, Heidelberg University), which deeply
  inspired the suite, the Methodenberatung pages of the University of
  Zurich and G*Power; the README also cites R and the packages used.
* Added three vignettes, "Getting started with infereportR", "Comparison
  families, adjusted p-values and plot annotations" and "Descriptive
  tables", and a pkgdown site at <https://cttir.github.io/infereportR/>,
  built and deployed to GitHub Pages by a GitHub Actions workflow.
* The repository is public. The README describes installation from GitHub,
  and the R CMD check workflow reads infercalcR with the standard workflow
  token instead of a separate access token.
* Prepared for a first CRAN submission, which has to wait until
  infercalcR is on CRAN.
* The maintainer address is now raban.heller@uni-ulm.de.
* DESCRIPTION drops the `Remotes` field, requires infercalcR 0.1.0 or
  later, gains a fuller Description and declares `Language: en-US`;
  `inst/WORDLIST` records the technical terms used in the documentation.
* The GitHub Actions check installs infercalcR explicitly, adds R-devel and R oldrel-1 on Linux, and explains what is
  missing when it cannot read infercalcR.
* Add a CTTIR v5 hex logo in the statistical inference color shared with
  the other inference packages; `tools/branding/render.R` draws it in R.
* Added plain tables, deterministic text and descriptions through infercalcR's
  shared result contract, with full-precision analytical fields and common
  formatting across displays.
* Added explicit-coordinate ggplot inference layers, facet-key checks,
  coordinate guards, and preserved adjustment-family metadata.
* Added installed-package base-R core and backend integration checks.
