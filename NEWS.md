epistack v1.1.3 (2021-12-08)
+ changes in epistack.R Rscript CLI
+ plotAverageProfile() now support 95% confidence interval 
(parameter `error_type`).

epistack v1.1.2 (2021-11-27)
+ addition of an Rscript script in inst/
+ new `main` parameter in plotEpistack()
+ plotBinning() `Bins` label in horizontal orientation

epistack v1.1.1 (2021-11-17)
+ epistack now mostly use RangesSummarizedExperiment objects instead of GRanges
objects (but backward compatibility with GRanges is ensured).

epistack v0.99.3 (2021-10-19)
+ plotAverageProfile() now has the parameter `error_type`, value: sd or sem,
default: sd (was previously sem)

epistack v0.99.2 (2021-10-15)
+ sessionInfo() at the end of the vignette
+ edits in NEWS, README and the vignette

epistack v0.99.1 (2021-10-01)
+ Initial Bioconductor submission
