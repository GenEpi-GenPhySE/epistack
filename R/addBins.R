#' addBins()
#'
#' @description
#'  Add an optional \code{bin} metadata column to \code{gr},
#'  to serve as annotations
#'  for the epistack plots.
#'
#' @param rse a SummarizedExperiment or a GRanges object.
#' @param nbins an integer number, the number of bins.
#' @param bin a vector containing pre-determined bins, in the same
#' order as \code{gr}.
#'
#' @return the RangedSummarizedExperiment or GRanges object with a
#' new \code{bin} metadata column
#'
#' @details
#'  \code{nbins} is taken into account only if \code{bin} is \code{NULL}.
#'  \code{rse} should be sorted first, usually with the
#'  \code{addMetricAndArrangeGRanges()}
#'  function. \code{addBin(rse, bin = vec)} is equivalent to
#'  \code{rse$bin <- vec}, while
#'  \code{addBin(rse, nbins = 5)} will create 5 bins of equal size based on
#'  \code{rse}
#'  order.
#'
#' @export
#'
#' @seealso \code{\link[epistack]{addMetricAndArrangeGRanges}}
#' \code{\link[epistack]{plotBinning}}
#'
#' @examples
#' data("stackepi")
#' addBins(stackepi)
#'
#' # 3 bins instead of 5
#' addBins(stackepi, nbins = 3)
#'
#' # assign bins using a vector
#' addBins(stackepi, bin = rep(c("a", "b", "c"),
#'  length.out = length(stackepi)))
addBins <- function(rse, nbins = 5L, bin = NULL){
    if(is(rse, "RangedSummarizedExperiment")) {
        if(is.null(bin)) {
            SummarizedExperiment::rowRanges(rse)$bin = as.numeric(cut(seq_along(rse), nbins))
        } else {
            SummarizedExperiment::rowRanges(rse)$bin = bin
        }
        rse
    } else {
        if(is.null(bin)) {
            rse$bin = as.numeric(cut(seq_along(rse), nbins))
        } else {
            rse$bin = bin
        }
        rse
    }
}


