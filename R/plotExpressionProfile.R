
#' plotExpressionProfile
#'
#' @param x a GRanges input
#' @param trans_func a function which applicate log10 transformation
#' @param title a character corresponding to the title of the plot
#'
#' @return a plot
#' @export
#'
#' @examples
#' plotExpressionProfile(gRanges_test$exp, title = "Expression")
plotExpressionProfile <- function(
    x, trans_func = function(x) log10(x+1), title = ""
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
    axis(3)
    axis(2, at = c(1, length(x)))
    mtext(side = 3, title , line = 2, cex = 0.8)
}
