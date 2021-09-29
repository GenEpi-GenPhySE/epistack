#' plotBoxMetric()
#'
#' @description Plot distribution of a metric values as bowplots
#' depending of bins.
#' If the \code{bin} is absent from \code{gr}, a single bowplot is drawn.
#'
#' @param gr a GRanges input
#' @param metric name of the column in \code{gr} metadata containing scores.
#' @param title title of the plot.
#' @param trans_func A function to transform value of x before ploting.
#' Useful to apply log10 transformation
#' (i.e. with \code{trans_func = function(x) log10(x+1)}).
#' @param ylim limit of the y axis; format: \code{ylim = c(min, max)}
#' @param ylab y-axis title
#' @param palette a function that returns a palette of \code{n} colors.
#'
#' @importFrom stats setNames
#'
#' @return Display a plot.
#'
#' @export
#'
#' @examples
#' data("stackepi")
#' plotBoxMetric(
#'        stackepi,
#'        trans_func = function(x) x,
#'        metric = "exp",
#'        title = "Metric"
#'   )
plotBoxMetric <- function(
    gr,
    metric = "expr", title = "Metric",
    trans_func = function(x) x,
    ylim = NULL, ylab = "metric",
    palette = colorRampPalette(c("magenta", "black", "green"))
){
    if (!is.null(gr$bin)) {
        graphics::boxplot(
            lapply(
                stats::setNames(levels(factor(gr$bin)), levels(factor(gr$bin))),
                function(i) trans_func(mcols(gr)[gr$bin == i, metric])
            ),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(length(unique(gr$bin))), axes = FALSE
        )
        axis(1, at = seq_along(levels(factor(gr$bin))),
             labels = levels(factor(gr$bin)))
    } else {
        graphics::boxplot(
            trans_func(mcols(gr)[[metric]]),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(1), axes = FALSE
        )
    }
    axis(2, at = ylim)
    mtext(side = 3, title, line = 0.5, cex = 0.8 * graphics::par()$cex.main)
}




