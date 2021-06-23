## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  cache = FALSE
)

## ----examplepath--------------------------------------------------------------
path_reads <- c(
    read1 = "https://raw.githubusercontent.com/Bioconductor/CSAMA2016/master/lab-5-chipseq/EpigeneticsCSAMA/inst/bedfiles/H3K27ac_rep1_filtered_ucsc_chr6.bed",
    read2 = "https://raw.githubusercontent.com/Bioconductor/CSAMA2016/master/lab-5-chipseq/EpigeneticsCSAMA/inst/bedfiles/H3K27ac_rep2_filtered_ucsc_chr6.bed",
    input = "https://raw.githubusercontent.com/Bioconductor/CSAMA2016/master/lab-5-chipseq/EpigeneticsCSAMA/inst/bedfiles/ES_input_filtered_ucsc_chr6.bed"
)

path_peaks <- c(
    peak1 = "https://raw.githubusercontent.com/Bioconductor/CSAMA2016/master/lab-5-chipseq/EpigeneticsCSAMA/inst/bedfiles/Rep1_peaks_ucsc_chr6.bed",
    peak2 = "https://raw.githubusercontent.com/Bioconductor/CSAMA2016/master/lab-5-chipseq/EpigeneticsCSAMA/inst/bedfiles/Rep2_peaks_ucsc_chr6.bed"
)

## ----peak_loading, message=FALSE----------------------------------------------
library(rtracklayer)
peaks <- lapply(path_peaks, import)

## ----peak_merge, message=FALSE------------------------------------------------
merged_peaks <- union(peaks[[1]], peaks[[2]])

scores_rep1 <- double(length(merged_peaks))
scores_rep1[findOverlaps(peaks[[1]], merged_peaks, select = "first")] <- peaks[[1]]$score

scores_rep2 <- double(length(merged_peaks))
scores_rep2[findOverlaps(peaks[[2]], merged_peaks, select = "first")] <- peaks[[2]]$score

peak_type <- ifelse(
    scores_rep1 != 0 & scores_rep2 != 0, "Both", ifelse(
        scores_rep1 != 0, "Rep1 only", "Rep2 only"
    )
)

mcols(merged_peaks) <- DataFrame(scores_rep1, scores_rep2, peak_type)
merged_peaks$mean_scores <- apply((mcols(merged_peaks)[, c("scores_rep1", "scores_rep2")]), 1, mean)
merged_peaks <- merged_peaks[order(merged_peaks$mean_scores, decreasing = FALSE), ]
rm(scores_rep1, scores_rep2, peak_type)

merged_peaks

## ----read_loading-------------------------------------------------------------
reads <- lapply(path_reads, import)

## ----coverage_matrices, message=FALSE-----------------------------------------
summary(width(merged_peaks))

library(EnrichedHeatmap)

coverage_matrices <- lapply(
    reads,
    function(x) {
        normalizeToMatrix(
            x,
            resize(merged_peaks, width = 1, fix = "center"),
            extend = 5000, w = 250, 
            mean_mode = "coverage"
        )
    }
)

colnames(coverage_matrices[[1]]) <- paste("Rep1" , seq_len(ncol(coverage_matrices[[1]])), sep = "_")
colnames(coverage_matrices[[2]]) <- paste("Rep2" , seq_len(ncol(coverage_matrices[[2]])), sep = "_")
colnames(coverage_matrices[[3]]) <- paste("Input", seq_len(ncol(coverage_matrices[[3]])), sep = "_")

xlabs <- c("-5kb", "peak center", "+5kb")

## ----ready_to_plot------------------------------------------------------------
mcols(merged_peaks) <- cbind(
    mcols(merged_peaks), 
    coverage_matrices[[1]], 
    coverage_matrices[[2]],
    coverage_matrices[[3]]
)

## ----peak_plot----------------------------------------------------------------
library(epistack)

plotEpistack(
    merged_peaks,
    patterns = c("^Rep1_", "^Rep2_", "^Input_"),
    tints = c("dodgerblue", "firebrick1", "grey"), 
    titles = c("Rep1", "Rep2" , "Input"),
    x_labels = xlabs,
    zlim = c(0, 4), ylim = c(0, 4), 
    metric_col = "mean_scores", metric_title = "Peak score", metric_label = "log10(score)",
    metric_transfunc = function(x) log10(x),
    npix_height = 300
)

## ----peak_plot_bin------------------------------------------------------------

merged_peaks$bin <- merged_peaks$peak_type

plotEpistack(
    merged_peaks,
    patterns = c("^Rep1_", "^Rep2_", "^Input_"),
    tints = c("dodgerblue", "firebrick1", "grey"), 
    titles = c("Rep1", "Rep2" , "Input"),
    x_labels = xlabs,
    zlim = c(0, 4), ylim = c(0, 4), 
    metric_col = "mean_scores", metric_title = "Peak score", metric_label = "log10(score)",
    metric_transfunc = function(x) log10(x),
    bin_palette = colorRampPalette(c("darkorchid1", "dodgerblue", "firebrick1")),
    npix_height = 300
)


## ----peak_plot_bin2-----------------------------------------------------------
merged_peaks <- merged_peaks[order(
  merged_peaks$bin, merged_peaks$mean_scores,
  decreasing = c(TRUE, FALSE), method = "radix"
), ]

plotEpistack(
    merged_peaks,
    patterns = c("^Rep1_", "^Rep2_", "^Input_"),
    tints = c("dodgerblue", "firebrick1", "grey"), 
    titles = c("Rep1", "Rep2" , "Input"),
    x_labels = xlabs,
    zlim = c(0, 4), ylim = c(0, 4), 
    metric_col = "mean_scores", metric_title = "Peak score", metric_label = "log10(score)",
    metric_transfunc = function(x) log10(x),
    bin_palette = colorRampPalette(c("darkorchid1", "dodgerblue", "firebrick1")),
    npix_height = 300
)


## ----example2_tss-------------------------------------------------------------
load(
    system.file("extdata", "chr21_test_data.RData",
                package = "EnrichedHeatmap"),
    verbose = TRUE
)

tss <- promoters(genes, upstream = 0, downstream = 1)
tss$gene_id <- names(tss)

tss

## ----expression_data----------------------------------------------------------
expr <- data.frame(
    gene_id = names(rpkm),
    expr = rpkm
)

epidata <- addMetricAndArrangeGRanges(
    tss, expr,
    gr_key = "gene_id",
    order_key = "gene_id", order_value = "expr"
)

epidata

## ----adding_bins--------------------------------------------------------------
epidata <- epistack:::addBins(epidata, nbins = 5)
epidata

## ----signal_extraction--------------------------------------------------------
methstack <- normalizeToMatrix(
    meth, tss, value_column = "meth",
    extend = 5000, w = 250, mean_mode = "absolute"
)
colnames(methstack) <- paste("meth" , seq_len(ncol(methstack)), sep = "_")

h3k4me3stack <- normalizeToMatrix(
    H3K4me3, tss, value_column = "coverage",
    extend = 5000, w = 250, mean_mode = "coverage"
)
colnames(h3k4me3stack) <- paste("H3K4me3", seq_len(ncol(h3k4me3stack)), sep = "_")

mcols(epidata) <- cbind(
    mcols(epidata), methstack, h3k4me3stack
)

## ----example2_plotting--------------------------------------------------------
plotEpistack(
    epidata,
    patterns = c("^meth_", "^H3K4me3"),
    tints = c("tomato", "springgreen"),
    titles = c("DNA methylation", "H3K4me3"),
    zlim = list(c(0, 1), c(0, 25)), ylim = list(c(0, 1), c(0, 50)),
    x_labels = c("-5kb", "TSS", "+5kb"),
    metric_col = "expr", metric_title = "Gene expression",
    metric_label = "log10(RPKM + 1)",
    metric_transfunc = function(x) log10(x + 1),
    npix_height = 300
)

