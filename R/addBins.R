
#' addBins
#'
#' @param gr a gRanges object
#' @param nbins a number corresponding to
#' @param bin a vector on number
#'
#' @return a gRanges with a new "bin" column
#'
#' @import rtracklayer
#'
#' @examples
#' data <- transform(data, log2fc_line = as.numeric(log2fc_line))
#' mat <- data[data$adjpval_line <= 0.01, c("gene", "log2fc_line", "gene_type")]
#' TSS <- promoters(TSS, upstream = 0, downstream = 1)
#' my_tss <- addMetricAndArrangeGRanges(TSS, mat, order_key = "gene", order_value = "log2fc_line")
#' my_tss <- addBins(my_tss)
#' my_tss

addBins <- function(gr, nbins = 5, bin = NULL){

    if(is.null(bin)) {
        gr$bin = as.numeric(cut(seq_along(gr), nbins))
    } else {
        gr$bin = bin
    }

    gr
}


