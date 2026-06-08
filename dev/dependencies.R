# Development dependencies (not sourced at runtime).
#
# This file exists so that `renv` records the development tooling in
# `renv.lock`. Because these packages are not referenced from the package's R
# code, renv would not otherwise detect them. Listing them here means
# `renv::restore()` installs the full toolchain (devtools, pkgdown, vignette
# builders, ...) alongside the package's runtime dependencies.
#
# See: https://rstudio.github.io/renv/articles/faq.html#how-do-i-tell-renv-about-a-dependency

library(devtools)    # also pulls usethis, pkgbuild, pkgload, remotes, rcmdcheck, sessioninfo, ...
library(roxygen2)    # documentation
library(pkgdown)     # documentation website
library(testthat)    # unit tests
library(knitr)       # vignette engine
library(rmarkdown)   # vignette / README rendering
