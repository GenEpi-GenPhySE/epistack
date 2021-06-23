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
#' #plotGeneType(my_tss)
#'
plotGeneType <- function(gr, palette = RColorBrewer::brewer.pal){
    geneType <- as.numeric(factor(gr$gene_type))
    image(t(rev(geneType)), col = palette(length(unique(gr$gene_type))),
          axes = FALSE,
          breaks = seq(0.5, length(unique(gr$gene_type)) + 0.5, by = 1))
    graphics::box()
    mtext(side = 3, "gene\ntype", line = 0.5,  cex = 0.8, las = 2)

}

