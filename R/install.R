#!/usr/bin/env Rscript

library(devtools)
library(withr)

# If an argument is provided, it is interpreted as the directory
# in which to install the library. Otherwise, it will be installed
# in the default location
lib_dir = commandArgs(trailingOnly=TRUE)[1]

install.t0 = function() {
  # withr has to be within a function
  if (!is.na(lib_dir)) {
    withr::local_libpaths(new=lib_dir, action="prefix")
  }
  install()
}

install.t0()
