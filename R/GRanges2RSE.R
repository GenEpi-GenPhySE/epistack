GRanges2RSE <- function(x, patterns, names = patterns) {

    mat <- S4Vectors::mcols(gr)

    assays <- lapply(patterns, function(pattern) {
        whichCols <- grepl(pattern, colnames(mat))
        as.matrix(mat[, whichCols])
    })



    testdata <- SummarizedExperiment(
        assays = assays
        rowData = epidata
    )

}
