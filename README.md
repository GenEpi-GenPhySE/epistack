
# epistack

<!-- badges: start -->
<!-- badges: end -->

The `epistack` package main objective is the visualizations of stacks 
of genomic tracks (such as, but not restricted to, ChIP-seq or
DNA methyation data)
centered at genomic regions of interest. `epistack` needs three 
different inputs:

- a genomic score objects, such as ChIP-seq coverag or DNA methylation values, 
provided as a `GRanges` (easily obtained from `bigwig` or `bam` files)
- a list of feature of interest, such as peaks or transcription start sites,
provided as a `GRanges` (easily obtained from `gtf` or `bed` files)
- a score to sort the features, such as peak hight of gene expression value


## Installation

You can install the current version of epistack from `R` using :

``` r
remotes::install_github("GenEpi-GenPhySE/epistack")
```


