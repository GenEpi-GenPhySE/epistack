#' GRanges2RSE()
#'
#' @description Convert objects from the old input format
#' (\code{GRanges} object) to the new recommanded input format
#' \code{RangedSummarizedExperiment}.
#'
#' @param gr a \code{GRanges} object with matrix embeded as metadata columns.
#' @param patterns A character vector of column prefixes
#' (can be regular expressions) that should match columns of \code{gr}.
#' @param names specify the desired names
#' of the assays (if different from patterns).
#'
#' @details Mostly used for backward compatibilities and unit testing.
#'
#' @export
#'
#' @return a \code{RangedSummarizedExperiment}.
#'
#' @examples
#' data("stackepi_gr")
#' GRanges2RSE(stackepi_gr, patterns = c("windows"))
#' GRanges2RSE(stackepi_gr, patterns = c("^windows_"), names = c("DNAme"))

GRanges2RSE <- function(gr, patterns, names = patterns) {
    mat <- S4Vectors::mcols(gr)

    assays <- lapply(patterns, function(pattern) {
        whichCols <- grepl(pattern, colnames(mat))
        as.matrix(mat[, whichCols])
    })
    names(assays) <- names

    tokeep <- grepl(paste(patterns, collapse = "|"), colnames(mat))

    S4Vectors::mcols(gr) <- S4Vectors::mcols(gr)[, !tokeep]

    SummarizedExperiment::SummarizedExperiment(
        assays = assays,
        rowData = gr
    )
}
