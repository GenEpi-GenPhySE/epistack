# epistack 1.5.4 (2022-04-05)
+ CITATION added

# epistack 1.5.3 (2022-04-04)
+ plotAverageProfile's reversed_z_order is now exposed in plotEpistack
+ 95% confidence interval is now the default in plotEpistack
+ plotEpistck's legends are now also displayed as y-axis title in
plotAverageProfile plots

# epistack 1.5.2 (2022-04-03)
+ changing the default bin colors
+ `bin_palette` in `plotEpistack()`, `plotBoxMetric()`, 
and `plotAverageProfile()`,  and `palette` in `plotBinning()` can now be 
vectors of colors instead of palette functions
+ `tints` now accept palette functions and list of palette functions,
in addition to colors.

# epistack 1.5.1 (2022-03-30)
+ vignette improvements (thanks to Isabelle Stevant)
+ documentation improvments: multiple zlims are possible if provided as a list

# epistack 1.1.3 (2021-12-08)
+ changes in epistack.R Rscript CLI
+ plotAverageProfile() now support 95% confidence interval 
(parameter `error_type`).

# epistack 1.1.2 (2021-11-27)
+ addition of an Rscript script in inst/
+ new `main` parameter in plotEpistack()
+ plotBinning() `Bins` label in horizontal orientation

# epistack 1.1.1 (2021-11-17)
+ epistack now mostly use RangesSummarizedExperiment objects instead of GRanges
objects (but backward compatibility with GRanges is ensured).

# epistack 0.99.3 (2021-10-19)
+ plotAverageProfile() now has the parameter `error_type`, value: sd or sem,
default: sd (was previously sem)

# epistack 0.99.2 (2021-10-15)
+ sessionInfo() at the end of the vignette
+ edits in NEWS, README and the vignette

# epistack 0.99.1 (2021-10-01)
+ Initial Bioconductor submission
