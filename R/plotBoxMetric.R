
#' plotBoxMetric
#'
#' @param gr a GRanges input
#' @param metric name of the column containing epigenetic profile score
#' @param title a character corresponding to metric title
#' @param trans_func a function to transform value of x
#' Useful to apply log10 transformation (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param ylim limit of the y axis; format: ylim = c (min, max)
#' @param ylab y axis title
#' @param palette a function of parameter `n`
#'  that returns a palette of `n` colors.
#'
#' @importFrom stats setNames
#'
#' @return a boxplot
#' @export
#'
#' @examples
#' data("gRanges_test")
#' plotBoxMetric(
#'        gRanges_test,
#'        trans_func = function(x) x,
#'        metric = "exp",
#'        title = "Metric"
#'   )
#'
#'
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
    mtext(side = 3, title, line = 0.5,  cex = 0.8)
}




