#!/usr/bin/env Rscript

# Command Line Interface for the {epistack} package
# Not feature-complete yet

# package installation and loading ----------------
if(!requireNamespace("epistack", quietly = TRUE)) {
    remotes::install_github("GenEpi-GenPhySE/epistack")
}
if(!requireNamespace("rtracklayer", quietly = TRUE)) {
    BiocManager::install("rtracklayer")
}
if(!requireNamespace("EnrichedHeatmap", quietly = TRUE)) {
    BiocManager::install("EnrichedHeatmap")
}
if(!requireNamespace("optparse", quietly = TRUE)) {
    install.packages("optparse")
}
if(!requireNamespace("data.table", quietly = TRUE)) {
    install.packages("data.table")
}

suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(epistack))

# options -----------------
option_list <- list(
    make_option(
        c("-a", "--anchors"),
        help = "Path to the anchor files. Currently supported format:
                    \n\tMACS .narrowPeak
                "),
    make_option(
        c("-b", "--bound"),
        help = "Path to the ChIP-seq bound coverage (bigwig)"
    ),
    make_option(
        c("-i", "--input"),
        help = "Path to the ChIP-seq input coverage (bigwig)"
    ),
    make_option(
        c("-p", "--png"),
        help = "Path of the output png file.",
        default = "epistack.png"
    ),
    make_option(
        c("-e", "--extend"),
        help = "Extend arround anchor by this number of base pairs.",
        default = 2500L
    ),
    make_option(
        c("-r", "--reference"),
        help = "One of:
            \n\t'center' as in middle of the region (i.e. gene center)
            \n\t'start' as in begin of the region with regards to the strand
        (i.e. transcription start sites)
        ",
        default = "center"
    ),
    make_option(
        c("-g", "--group"),
        help = "Number of groups (bins) to plot average profiles.
        Default to NULL",
        default = NULL, type = "integer"
    ),
    make_option(
        c("-y", "--ylim"),
        help = "Y-limits for the average profile plot(s).
        Default to 1",
        default = 1
    ),
    make_option(
        c("-z", "--zlim"),
        help = "Z upper limit for the stack profile plot(s).
        Default to 1.",
        default = 1
    ),
    make_option(
        c("-t", "--title"),
        help = "Title of the figure.",
        default = NULL
    ),
    make_option(
        c("-m", "--maxpeaks"),
        help = "The maximum number of peaks to be plotted.",
        default = NULL, type = "integer"
    ),
    make_option(
        c("-f", "--errortype"),
        help = "Error type for the average profiles. One of: sd, sem, ci95.
         Default: ci95.",
        default = "ci95",
    ),
    make_option(
        c("-c", "--cpu"),
        help = "Number of cores.
        Increases speed at the cost of higher RAM usage."
    ),
    make_option(
        c("-v", "--verbose"), action = "store_true",
        default = FALSE,
        help = "Print progress messages."
    )
)

opt <- parse_args(OptionParser(
    option_list = option_list,
    description = "{epistack} CLI
    make nice heatmaps from processed files in a single CLI call."
))

# uncomment next line for debugging
# message(dput(opt))

# script body ----------------
if (opt$verbose) {
    message("Parsing files...", appendLF = FALSE)
}
if (grepl("tsv$", opt$anchors)) {
    anchors <- data.table::fread(opt$anchors)
    anchors <- with(
        anchors, 
        GenomicRanges::GRanges(
            chromosome_name,
            IRanges::IRanges(median_tss),
            strand = strand,
            mcols = anchors[, c("ensembl_gene_id", "gene_biotype")]
    ))
} else {
    anchors <- rtracklayer::import(opt$anchors)
}
bigwig <- parallel::mclapply(
    list(bound = opt$bound, input = opt$input),
    rtracklayer::import,
    mc.cores = opt$cpu
)

if (opt$verbose) {
    message(" done!")
    message("Processing...", appendLF = FALSE)
}
anchors <- anchors[order(
    anchors$score,
    decreasing = TRUE,
    na.last = TRUE
), ]
if (!is.null(opt$maxpeaks) && length(anchors) > opt$maxpeaks) {
    anchors  <-  anchors[seq(1L, opt$maxpeaks, by = 1L), ]
}
ranchors <- switch(
    opt$reference,
    "center" = GenomicRanges::resize(anchors, width = 1, fix = "center"),
    "start" = GenomicRanges::promoters(anchors, upstream = 0, downstream = 0)
)
assays <-  parallel::mclapply(
    bigwig,
    function(x) EnrichedHeatmap::normalizeToMatrix(
        x,
        ranchors,
        value_column = "score", mean_mode = "coverage",
        extend = opt$extend, w = opt$extend / 40
    ),
    mc.cores = opt$cpu
)
rm(bigwig)
dfp <- SummarizedExperiment::SummarizedExperiment(
    rowRanges = anchors,
    assays =  assays
)
rm(anchors, ranchors, assays)
if (!is.null(opt$group)) {
    dfp <- addBins(dfp, nbins = opt$group)
}

if (opt$verbose) {
    message(" done!")
    message("Plotting...", appendLF = FALSE)
}
png(opt$png, width = 1000, height = 1000)
plotEpistack(
    dfp,
    assays = c("bound", "input"),
    titles = c("Bound", "Input"),
    legends = "FPKM", main = opt$title,
    tints = c("firebrick1", "grey"),
    x_labels = c("-2.5kb", "peak center", "+2.5kb"),
    metric_col = "score", metric_label = "Peak scores",
    ylim = c(0, opt$ylim), zlim = c(0, opt$zlim),
    n_core = opt$cpu,
    error_type = opt$errortype,
    cex = 1.6, cex.main = 2.4
)
invisible(dev.off())

if (opt$verbose) {
    message(" done!")
    message(paste("Job completed for file:", opt$bound))
}
