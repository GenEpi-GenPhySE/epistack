

#' plotGeneTypeLegend
#'
#' @param gr a gRanges object
#'
#' @return a legend for plot of gene_type
#'
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#' #plotGeneTypeLegend(gRanges_test)
plotGeneTypeLegend <- function(gr){
    ml <- graphics::legend("topright", legend = rep("", length(unique(gr$gene_type))), fill = RColorBrewer::brewer.pal(length(unique(gr$gene_type)), name = "RdBu"), bty = "n",
                 title = "gene type:", title.adj = -8)
    graphics::text(ml$text$x - 0.15, ml$text$y - 0.003, levels(factor(gr$gene_type)), pos = 2)
}


