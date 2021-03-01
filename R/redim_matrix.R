#' redim_matrix
#'
#' this function resizes the input matrix by applying the rounded average
#' on the columns
#'
#' @param mat a matrix: the input matrix
#' @param target_height a number: the length of the output matrix
#' @param target_width a number: the width of the output matrix
#' @param summary_func a function: the mean function with "na.rm = TRUE"
#' @param output_type a number: vapply style
#' @param n_core a number: parallel processing
#'
#' @return a matrix: a resized matrix
#' @export
#'
#' @importFrom S4Vectors mcols
#'
#' @examples
#' mat <- S4Vectors::mcols(gRanges_test)
#' whichCols <- grepl("^window_", colnames(mat))
#' mat <- as.matrix(mat[, whichCols])
#' dim(mat)
#' smallMat <- redim_matrix(mat, target_height = 10, target_width = ncol(mat))
#' dim(smallMat)
#'
#' mat <- matrix(sample(1:40,100,replace=TRUE),nrow=10,ncol=10)
#' dim(mat)
#' smallMat <- redim_matrix(mat, target_height = 5, target_width = ncol(mat),
#'    summary_func = function(x) max(x, na.rm = TRUE))
#' dim(smallMat)
redim_matrix <- function(
    mat,
    target_height = 100,
    target_width = 100,
    summary_func = function(x) mean(x, na.rm = TRUE),
    output_type = 0.0,
    n_core = 1
) {

    if(target_height > nrow(mat) | target_width > ncol(mat)) {
        stop("Input matrix must be bigger than target width and height.")
    }

    seq_height <- round(seq(1, nrow(mat), length.out = target_height + 1))
    seq_width  <- round(seq(1, ncol(mat), length.out = target_width  + 1))

    # complicated way to write a double for loop
    # i is row
    do.call(rbind, parallel::mclapply(seq_len(target_height), function(i) {
        vapply(seq_len(target_width), function(j) { # j is column
            summary_func(
                mat[
                    seq(seq_height[i], seq_height[i + 1]),
                    seq(seq_width[j] , seq_width[j + 1] )
                ]
            )
        }, output_type)
    }, mc.cores = n_core))
}


