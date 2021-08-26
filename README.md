# What is a CFF?

CFF stands for Citation File Format, a scientific standard that facilitates the gathering of software metadata for citation purposes.

For more information, please visit:

- The Citation File Format homepage: https://citation-file-format.github.io/
- [Detailed CFF documentation](https://github.com/citation-file-format/citation-file-format/blob/master/README.md)
- [An online CITATION.cff file initializer](https://citation-file-format.github.io/cff-initializer-javascript/)

# RCFF

This package follows up on a [proposal shared on the citation-file-format GitHub page](https://github.com/citation-file-format/citation-file-format/issues/110#issue-648726798)
by providing tools to convert between the standardized Citation File Format (CFF) and the structure of an R package DESCRIPTION file.

# Installation and Usage

This package is still very early in development, but you can still install it and take it for a spin at your own risk.

Install the development version of RCFF with

```r
remotes::install_github("ocbe-uio/RCFF")
```

and run

```r
r2cff()
```

To see part of the package's description file converted to CFF. You can also provide the path (absolute or relative) to some other DESCRPTION file to test it.

# Contributing

This package is licensed under the GPL, all contributions are welcome. To submit a bug or a feature request, please use [this form](https://github.com/ocbe-uio/RCFF/issues/new). To contribute code, please use [Pull Requests](https://github.com/ocbe-uio/RCFF/pulls).

# Badges

[![CI status](https://github.com/ocbe-uio/RCFF/workflows/R-CMD-check/badge.svg)](https://github.com/ocbe-uio/RCFF/actions)
[![](https://img.shields.io/github/last-commit/ocbe-uio/RCFF.svg)](https://github.com/ocbe-uio/RCFF/commits/S4)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)