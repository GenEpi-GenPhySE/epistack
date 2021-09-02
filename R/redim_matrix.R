#' redim_matrix()
#'
#' Reduce the input matrix size by applying a summary function on cells to
#' be fused.
#'
#' @param mat the input matrix.
#' @param target_height height of the output matrix
#' (should be smaller than or equal to \code{nrow(mat)}).
#' @param target_width width of the output matrix
#' (should be smaller than or equal to \code{ncol(mat)}).
#' @param summary_func how to summerize cells? A function such has
#' \code{mean}, \code{median}, \code{max}, or \code{meanColors}.
#' @param output_type Type of the output, to be passed to \code{vapply}'s
#' FUN.VALUE.
#' @param n_core number of core to use for parallel processing.
#'
#' @return a resized matrix of size \code{target_width} x \code{target_height}
#'  where the \code{summary_fun} was apply
#'  to adjacent cells.
#'
#' @details This function is used to reduce matrix right before plotting them
#' in order to avoid overplotting issues as well as other plotting artefacts.
#'
#' @seealso \code{\link[epistack]{meanColors}}
#'
#' @export
#'
#' @importFrom S4Vectors mcols
#'
#' @examples
#' data("gRanges_test")
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


