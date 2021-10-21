library(EnrichedHeatmap)
library(SummarizedExperiment)
load(
    system.file("extdata", "chr21_test_data.RData",
                package = "EnrichedHeatmap"),
    verbose = TRUE
)
tss <- promoters(genes, upstream = 0, downstream = 1)
tss$gene_id <- names(tss)
tss

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
# epidata <- addBins(epidata, nbins = 5)
epidata

methstack <- normalizeToMatrix(
    meth, epidata, value_column = "meth",
    extend = 5000, w = 250, mean_mode = "absolute"
)
colnames(methstack) <- paste("meth" , seq_len(ncol(methstack)), sep = "_")

h3k4me3stack <- normalizeToMatrix(
    H3K4me3, epidata, value_column = "coverage",
    extend = 5000, w = 250, mean_mode = "coverage"
)
colnames(h3k4me3stack) <- paste("H3K4me3", seq_len(ncol(h3k4me3stack)), sep = "_")

testdata <- SummarizedExperiment(
    assays = list(meth = methstack, h3k4me3 = h3k4me3stack),
    rowData = epidata
)

# plot epistack ---------
patterns <- c("meth", "h3k4me3")
rse <- testdata

rseb <- addBins(rse)
