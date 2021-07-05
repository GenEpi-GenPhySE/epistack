

#' plotGeneTypeLegend
#'
#' @param gr a gRanges object
#' @param palette a function taking a number has a first argument, and returning a vector of colors.
#'
#' @return a legend for plot of gene_type
#'
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#' library(GenomicRanges)
#' gr <- GRanges(
#'     rep("chr1", 10),
#'     IRanges(seq(1, 901, by = 100), seq(100, 1000, by = 100)),
#'     gene_type = c(rep("protein_coding", 5), rep("linc_RNA", 5))
#' )
#' plotGeneTypeLegend(gr)
plotGeneTypeLegend <- function(gr, palette = grDevices::rainbow){
    ml <- graphics::legend("topright", legend = rep("", length(unique(gr$gene_type))), fill = palette(length(unique(gr$gene_type))), bty = "n",
                 title = "gene type:", title.adj = -8)
    graphics::text(ml$text$x - 0.15, ml$text$y - 0.003, levels(factor(gr$gene_type)), pos = 2)
}


