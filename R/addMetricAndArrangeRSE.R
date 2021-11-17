#' addMetricAndArrangeRSE()
#'
#' @description
#'  Perform an inner join between a rangedSummarizedExperiment object
#'  and a data.frame. Sort
#'  the resulting rangedSummarizedExperiment based on a metric column.
#'
#' @param rse a rangedSummarizedExperiment object.
#' @param order a data.frame with at least two columns: keys and values.
#' @param rse_key name of the gr metadata column containing unique names for
#' each genomic region in \code{rowRanges(rse)}. Usually gene names/id or
#' peak id.
#' @param order_key name of the \code{order} column
#'    that will be used as key for the inner join.
#' @param order_value name of the \code{order} column
#'    that contain value used for sorting.
#' @param shuffle_tie a boolean Value (TRUE / FALSE).
#'    When TRUE, shuffle the GRanges before sorting, mixing the ties.
#'
#' @return a rangedSummarizedExperiment sorted in descending order.
#'
#' @details This utility function allow the addition of a metric column to
#' genomic regions of interest. One of its common use case is to add
#' gene expression values on a set of transcription start sites.
#' The resulting GRanges object will only contain regions presents in both
#' \code{rse} and \code{order}.
#'
#' @export
#'
#' @importFrom S4Vectors mcols
#' @importFrom IRanges IRanges
#'
#' @examples
#' data("stackepi")
#' ramdomOrder <- data.frame(
#'    gene_id = SummarizedExperiment::rowRanges(stackepi)$gene_id,
#'    value = rnorm(length(stackepi))
#' )
#' addMetricAndArrangeRSE(stackepi,
#'    ramdomOrder, rse_key = "gene_id",
#'    order_key = "gene_id", order_value = "value")
#'
#'
addMetricAndArrangeRSE <- function(rse,
                                       order,
                                       rse_key = "name",
                                       order_key = "name",
                                       order_value = "exp",
                                       shuffle_tie = TRUE) {
    names(rse) <- S4Vectors::mcols(
        SummarizedExperiment::rowRanges(rse)
    )[[rse_key]]
    common_names <- base::intersect(names(rse), order[[order_key]])
    rse <- rse[common_names, ]
    myMcols <- base::merge(order,
                           S4Vectors::mcols(
                               SummarizedExperiment::rowRanges(rse)
                           ),
                           by.x = order_key,
                           by.y = gr_key)
    rownames(myMcols) <- myMcols[[order_key]]
    myMcols <- myMcols[common_names, ]
    S4Vectors::mcols(
        SummarizedExperiment::rowRanges(rse)
    ) <- myMcols
    if (shuffle_tie && length(rse) > 1) {
        rse <- rse[sample(seq_len(length(rse))), ]
    }
    rse <- rse[order(S4Vectors::mcols(
        SummarizedExperiment::rowRanges(rse)
    )[[order_value]], decreasing = TRUE)]
    rse
}
