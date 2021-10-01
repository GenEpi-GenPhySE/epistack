#' addMetricAndArrangeGRanges()
#'
#' @description
#'  Perform an inner join between a GRanges object and a data.frame. Sort
#'  the resulting GRanges based on a metric column.
#'
#' @param gr a GRanges object.
#' @param order a data.frame with at least two columns: keys and values.
#' @param gr_key name of the gr metadata column containing unique names for
#' each genomic region in \code{gr}. Usually gene names/id or peak id.
#' @param order_key name of the \code{order} column
#'    that will be used as key for the inner join.
#' @param order_value name of the \code{order} column
#'    that contain value used for sorting.
#' @param shuffle_tie a boolean Value (TRUE / FALSE).
#'    When TRUE, shuffle the GRanges before sorting, mixing the ties.
#'
#' @return a GRanges sorted in descending order.
#'
#' @details This utility function allow the addition of a metric column to
#' genomic regions of interest. One of its common use case is to add
#' gene expression values on a set of transcription start sites.
#' The resulting GRanges object will only contain regions presents in both
#' \code{gr} and \code{order}.
#'
#' @export
#'
#' @importFrom S4Vectors mcols
#' @importFrom IRanges IRanges
#'
#' @examples
#' data("stackepi")
#' ramdomOrder <- data.frame(gene_id = stackepi$gene_id,
#'    value = rnorm(length(stackepi)))
#' addMetricAndArrangeGRanges(stackepi,
#'    ramdomOrder, gr_key = "gene_id",
#'    order_key = "gene_id", order_value = "value" )
#'
#'
addMetricAndArrangeGRanges <- function(gr,
                                       order,
                                       gr_key = "name",
                                       order_key = "name",
                                       order_value = "exp",
                                       shuffle_tie = TRUE) {
    names(gr) <- mcols(gr)[[gr_key]]
    common_names <- base::intersect(names(gr), order[[order_key]])
    gr <- gr[common_names, ]
    myMcols <- base::merge(order,
                           S4Vectors::mcols(gr),
                           by.x = order_key,
                           by.y = gr_key)
    rownames(myMcols) <- myMcols[[order_key]]
    myMcols <- myMcols[common_names, ]
    S4Vectors::mcols(gr) <- myMcols
    if (shuffle_tie && length(gr) > 1) {
        gr <- gr[sample(seq_len(length(gr))), ]
    }
    gr <- gr[order(S4Vectors::mcols(gr)[[order_value]], decreasing = TRUE)]
    gr
}
