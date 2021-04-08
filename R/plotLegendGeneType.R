

#' legned_gene_type
#'
#' @param gr a gRanges object
#'
#' @return a legend for plot of gene_type
#'
#' @importFrom grDevices rainbow
#'
#' @examples
#' legned_gene_type(my_tss)
legned_gene_type <- function(gr){
    geneTypePalette = RColorBrewer::brewer.pal
    ml <- graphics::legend("topright", legend = rep("", length(unique(gr$gene_type))), fill = geneTypePalette(length(unique(gr$gene_type)), name = "RdBu"), bty = "n",
                 title = "gene type:", title.adj = -8)
    graphics::text(ml$text$x - 0.15, ml$text$y - 0.003, levels(factor(gr$gene_type)), pos = 2)
}


