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

```
r2cff()
```

To see part of the package's description file converted to CFF. You can also provide the path (absolute or relative) to some other DESCRPTION file to test it.
