
#' boxplot_profileExp
#'
#' @param x a gRanges input
#' @param trans_func a function which applicate log10 transformation
#'
#' @return a boxplot
#' @export
#'
#' @examples
#' boxplot_profileExp(gRanges_test$exp)
boxplot_profileExp <- function(
    x, trans_func = function(x) log10(x+1)
){
    boxplot(trans_func(x))
}
