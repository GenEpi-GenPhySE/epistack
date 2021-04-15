
#' plotMetric
#'
#' @param x a GRanges input
#' @param trans_func a function to transform value of x.
#' Useful to apply log10 transformation (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param title a character corresponding to the title of the plot
#'
#' @return a plot
#' @export
#'
#' @examples
#' plotExpressionProfile(gRanges_test$exp, title = "Expression")
plotMetric <- function(
    x, trans_func = function(x) x, title = "Metric", ylim = NULL, xlab = NULL
){
    plot(
        x = trans_func(x),
        y = seq_along(x),
        type = "l", lwd = 3,
        axes = FALSE, xlab = NA, ylab = NA,
        ylim = c(1, length(x)),
        xaxs="i", yaxs="i"
    )
    graphics::box()
    axis(1)
    axis(2, at = c(1, length(x)))
    title(xlab = xlab)
    mtext(side = 3, title , line = 0.5, cex = 0.8)
}