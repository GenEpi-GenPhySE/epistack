#' plotMetric()
#'
#' @description Plot a vertical line chart or the metric column,
#' in the same order as the input.
#'
#' @param x a numeric vector.
#' @param trans_func a function to transform \code{x} values before ploting.
#' Useful to apply log10 transformation
#' (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param title Title of the plot.
#' @param ylim limit of the y axis; format: \code{ylim = c(min, max)}
#' @param xlab x-axis title
#'
#' @export
#'
#' @return Display a plot.
#'
#' @seealso \code{\link[epistack]{plotEpistack}},
#' \code{\link[epistack]{plotBoxMetric}}
#'
#' @examples
#' data("stackepi")
#' plotMetric(stackepi$exp)
#'
#'
plotMetric <- function(
    x, trans_func = function(x) x, title = "Metric", ylim = NULL, xlab = NULL
){
    plot(
        x = rev(trans_func(x)),
        y = seq_along(x),
        type = "l", lwd = 3,
        axes = FALSE, xlab = NA, ylab = NA,
        ylim = c(1, length(x)),
        xaxs="i", yaxs="i"
    )
    graphics::box()
    graphics::axis(1)
    graphics::axis(2, at = c(1, length(x)))
    graphics::title(xlab = xlab)
    mtext(side = 3, title , line = 0.5, cex = 0.8 * graphics::par()$cex.main)
}
