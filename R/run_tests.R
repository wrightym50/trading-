#!/usr/bin/env Rscript

library(testthat)

source("R/t0.R")

result = devtools::test(reporter="check")
