#' addBins
#'
#' @param gr a gRanges object
#' @param nbins a number corresponding to
#' @param bin a vector on number
#'
#' @return a gRanges with a new "bin" column
#'
#' @export
#'
#' @examples
#' addBins(gRanges_test)
#'
#' # 3 bins instead of 5
#' addBins(gRanges_test, nbins = 3)
#'
#' # assign bins using a vector
#' addBins(gRanges_test, bin = rep(c("a", "b", "c"),
#'  length.out = length(gRanges_test)))
#'
#'
addBins <- function(gr, nbins = 5, bin = NULL){

    if(is.null(bin)) {
        gr$bin = as.numeric(cut(seq_along(gr), nbins))
    } else {
        gr$bin = bin
    }

    gr
}


