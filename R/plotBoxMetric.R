#' plotBoxMetric()
#'
#' @description Plot distribution of a metric values as boxplots
#' depending of bins.
#' If the \code{bin} is absent from \code{gr}, a single boxplot is drawn.
#'
#' @param rse a RangedSummarizedExperiment input. Aletrnatively: can be a
#' GRanges object (for backward compatibility).
#' @param metric name of the column in \code{rse} metadata containing scores.
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
    rse,
    metric = "expr", title = "Metric",
    trans_func = function(x) x,
    ylim = NULL, ylab = "metric",
    palette = colorRampPalette(c("magenta", "black", "green"))
){
    if (is(rse, "RangedSummarizedExperiment")) {
        gr <- SummarizedExperiment::rowRanges(rse)
    } else  {
        gr <- rse
    }

    if (!is.null(gr$bin)) {
        bin <- gr$bin
        graphics::boxplot(
            lapply(
                stats::setNames(
                    levels(factor(bin)),
                    levels(factor(bin))
                ),
                function(i) trans_func(
                    S4Vectors::mcols(gr)[bin == i, metric]
                )
            ),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(length(unique(bin))), axes = FALSE
        )
        axis(1, at = seq_along(levels(factor(bin))),
             labels = levels(factor(bin)))
    } else {
        graphics::boxplot(
            trans_func(S4Vectors::mcols(gr)[[metric]]),
            ylab = ylab, pch = 19, ylim = ylim,
            col = palette(1), axes = FALSE
        )
    }
    axis(2, at = ylim)
    mtext(side = 3, title, line = 0.5, cex = 0.8 * graphics::par()$cex.main)
}




