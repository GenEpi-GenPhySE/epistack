

#' legned_gene_type
#'
#' @param gr a gRanges object
#'
#' @return
#'
#' @examples
#' legned_gene_type(my_tss)
legned_gene_type <- function(gr){
    geneTypePalette = rainbow
    ml <- legend("topright", legend = rep("", length(unique(gr$gene_type))), fill = geneTypePalette(length(unique(gr$gene_type))), bty = "n",
                 title = "gene type:", title.adj = -8)
    text(ml$text$x - 0.1, ml$text$y - 0.003, levels(factor(gr$gene_type)), pos = 2)
}


