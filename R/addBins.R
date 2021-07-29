#' addBins()
#'
#' @description
#'  Add an optional bin metadata column to \code{gr}, to serve as annotations
#'  for the epistack plots.
#'
#' @param gr a gRanges object.
#' @param nbins an integer number, the number of bins.
#' @param bin a vector containing pre-determined bins, in the same
#' order as \code{gr}.
#'
#' @return the \code{gr} GRanges object with a new \code{bin} metadata column
#'
#' @details
#'  \code{nbins} is taken into account only if \code{bin} is \code{NULL}.
#'  \code{gr} should be sorted first, usually with the
#'  \code{addMetricAndArrangeGRanges()}
#'  function. \code{addBin(gr, bin = vec)} is equivalent to
#'  \code{gr$bin <- vec}, while
#'  \code{addBin(gr, nbins = 5)} will create 5 bins of equal size based on
#'  \code{gr}
#'  order.
#'
#' @export
#'
#' @seealso \link{addMetricAndArrangeGRanges}
#'
#' @examples
#' data("gRanges_test")
#' addBins(gRanges_test)
#'
#' # 3 bins instead of 5
#' addBins(gRanges_test, nbins = 3)
#'
#' # assign bins using a vector
#' addBins(gRanges_test, bin = rep(c("a", "b", "c"),
#'  length.out = length(gRanges_test)))
addBins <- function(gr, nbins = 5L, bin = NULL){

    if(is.null(bin)) {
        gr$bin = as.numeric(cut(seq_along(gr), nbins))
    } else {
        gr$bin = bin
    }

    gr
}


