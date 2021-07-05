#' plotGeneType
#'
#' @param gr a gRanges object containing a "gene_type" column
#' @param palette a function taking a number has a first argument, and returning a vector of colors.
#'
#' @return a plot
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
#' plotGeneType(gr)
plotGeneType <- function(gr, palette = grDevices::rainbow){
    geneType <- as.numeric(factor(gr$gene_type))
    image(t(rev(geneType)), col = palette(length(unique(gr$gene_type))),
          axes = FALSE,
          breaks = seq(0.5, length(unique(gr$gene_type)) + 0.5, by = 1))
    graphics::box()
    mtext(side = 3, "gene\ntype", line = 0.5,  cex = 0.8, las = 2)
}

