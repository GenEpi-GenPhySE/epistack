
#' addMetricAndArrangeGRanges
#'
#' @param gr an input GRanges
#' @param myOrder a data.frame or data.tabe contains the list of genes
#' @param gr_key a character ...
#' @param order_key a character corresponding to the column
#'    that will be used to sort
#' @param order_value a character corresponding to the column
#'    of gene expression
#' @param shuffle_tie a boolean Value (TRUE - FALSE),
#'    when TRUE, shuffle the gRanges before sorting
#'
#' @return a GRanges sorted in ascending order
#' @export
#'
#' @importFrom S4Vectors mcols
#' @importFrom IRanges IRanges
#'
#' @examples
#' ramdomOrder <- data.frame(gene_id = gRanges_test$gene_id,
#'    value = rnorm(length(gRanges_test)))
#' addMetricAndArrangeGRanges(gRanges_test,
#'    ramdomOrder, gr_key = "gene_id",
#'    order_key = "gene_id", order_value = "value" )
#'
#'
addMetricAndArrangeGRanges <- function(gr,
                                       myOrder,
                                       gr_key = "name",
                                       order_key = "name",
                                       order_value = "exp",
                                       shuffle_tie = TRUE) {
    names(gr) <- mcols(gr)[[gr_key]]
    common_names <- base::intersect(names(gr), myOrder[[order_key]])
    gr <- gr[common_names, ]
    myMcols <- base::merge(myOrder,
                           S4Vectors::mcols(gr),
                           by.x = order_key,
                           by.y = gr_key)
    rownames(myMcols) <- myMcols[[order_key]]
    myMcols <- myMcols[common_names, ]
    S4Vectors::mcols(gr) <- myMcols
    if (shuffle_tie && length(gr) > 1) {
        gr <- gr[sample(seq_len(length(gr))), ]
    }
    gr <- gr[order(S4Vectors::mcols(gr)[[order_value]], decreasing = FALSE)]
    gr
}
