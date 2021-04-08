
#' plot_gene_type
#'
#' @param gr a gRanges object containing a "gene_type" column
#'
#' @return a plot
#'
#' @importFrom grDevices rainbow
#'
#' @examples
#' plot_gene_type(my_tss)
#'
plot_gene_type <- function(gr){
    geneTypePalette = RColorBrewer::brewer.pal
    geneType <- as.numeric(factor(gr$gene_type))
    image(t(rev(geneType)), col = geneTypePalette(length(unique(gr$gene_type)), name = "RdBu"),
          axes = FALSE,
          breaks = seq(0.5, length(unique(gr$gene_type)) + 0.5, by = 1))
    graphics::box()
    mtext(side = 3, "gene\ntype", line = 0.5,  cex = 0.8, las = 2)

}

