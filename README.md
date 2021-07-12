
# epistack

<!-- badges: start -->
[![Codecov test coverage](https://codecov.io/gh/GenEpi-GenPhySE/epistack/branch/master/graph/badge.svg)](https://codecov.io/gh/GenEpi-GenPhySE/epistack?branch=master)
[![R-CMD-check](https://github.com/GenEpi-GenPhySE/epistack/workflows/R-CMD-check/badge.svg)](https://github.com/GenEpi-GenPhySE/epistack/actions)
<!-- badges: end -->

<img src="inst/extdata/example_vizbi_bos_epistack2.png" width="450" alt="Example of an epistack output">

The `epistack` package main objective is the visualizations of stacks 
of genomic tracks (such as, but not restricted to, ChIP-seq, ATAC-seq,
DNA methyation or genomic conservation data)
centered at genomic regions of interest. `epistack` needs three 
different inputs:

- a genomic score objects, such as ChIP-seq coverage or DNA methylation values, 
provided as a `GRanges` (easily obtained from `bigwig` or `bam` files)
- a list of feature of interest, such as peaks or transcription start sites,
provided as a `GRanges` (easily obtained from `gtf` or `bed` files)
- a score to sort the features, such as peak height or gene expression value


## Installation

You can install the current version of epistack from `R` using :

``` r
remotes::install_github("GenEpi-GenPhySE/epistack")
```

## Documentation

A (draft) version of `epistack` vignette can be read [here](https://gdevailly.github.io/using_epistack.html).


