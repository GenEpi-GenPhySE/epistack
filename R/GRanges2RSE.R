#'

GRanges2RSE <- function(gr, patterns, names = patterns) {

    mat <- S4Vectors::mcols(gr)

    assays <- lapply(patterns, function(pattern) {
        whichCols <- grepl(pattern, colnames(mat))
        as.matrix(mat[, whichCols])
    })
    names(assays) <- names

    tokeep <- grepl(paste(patterns, collapse = "|"), colnames(mat))

    mcols(gr) <- mcols(gr)[, !tokeep]

    SummarizedExperiment::SummarizedExperiment(
        assays = assays,
        rowData = gr
    )
}
