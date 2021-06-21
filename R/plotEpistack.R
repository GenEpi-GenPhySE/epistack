
#' plotEpistack
#'
#' @param gr a granges input
#' @param patterns
#' @param tints color
#' @param titles
#' @param x_labels x axis labels
#' @param zlim the minimum and maximum z values for which colors should be plotted
#' @param ylim limit of the y axis; format: ylim = c (min, max)
#' @param metric_col a character corresponding to epigenetic profile score
#' @param metric_title name of the score column
#' @param metric_label label of the score column
#' @param metric_transfunc a function to transform value of x
#' Useful to apply log10 transformation (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param bin_palette a palette of color
#' @param npix_height
#' @param n_core a number : parallel processing
#' @param high_mar a vector of numerical values corresponding to the margins of the top figures
#' @param low_mar a vector of numerical values corresponding to the margins of the bottom figures
#'
#' @return a png
#' @export
#'
#' @examples
#' plotEpistack(gRanges_test, metric_col = "exp", metric_transfunc = function(x) log10(x+1))
#'
plotEpistack <- function(
    gr,
    patterns = "^window_", tints = "gray", titles = "window",
    x_labels = c("-2.5kb", "TSS", "+2.5kb"),
    zlim = c(0, 1), ylim = NULL,
    metric_col = "expr", metric_title = "Metric", metric_label = "metric",
    metric_transfunc = function(x) x,
    bin_palette = colorRampPalette(c("magenta", "black", "green")),
    npix_height = 650, n_core = 1,
    high_mar = c(2.5, 0.6, 4, 0.6), low_mar = c(2.5, 0.6, 0.3, 0.6)
) {

    oldpar <- par(
        mgp = c(1.5, 0.5, 0),
        mar = high_mar + c(0, 4, 0, 0)
    )

    n_pattern <- length(patterns)
    bin_present <- !is.null(gr$bin)

    layout_mat <- matrix(seq_len(3 + bin_present * 3 + n_pattern * 3), nrow  = 3)
    layout_heights <- c(1, 0.12, 0.25)
    layout_widths <- if (bin_present) {
        c(0.3, 0.08, rep(0.35, n_pattern))
    } else {
        c(0.3, rep(0.35, n_pattern))
    }

    layout(layout_mat, heights = layout_heights, widths = layout_widths)

    plotMetric(
        mcols(gr)[[metric_col]],
        title = metric_title,
        trans_func = metric_transfunc,
        xlab = metric_label
    )
    par(mar = low_mar)
    plot.new()
    par(mar = low_mar + c(0, 4, 0, 0))
    plotBoxMetric(
        gr, trans_func = metric_transfunc,
        palette = bin_palette,
        metric = metric_col,
        title = metric_title,
        ylab = metric_label
    )

    if (bin_present) {
        par(mar = high_mar)
        plotBinning(gr, target_height = npix_height, palette = bin_palette)
        par(mar = low_mar)
        plot.new()
        plot.new()
    }

    if(!is.list(zlim)) {
        zlim <- lapply(seq_along(patterns), function(x) zlim)
    }
    if(!is.list(ylim)) {
        ylim <- lapply(seq_along(patterns), function(x) ylim)
    }

    for (i in seq_along(patterns)) {
        par(mar = high_mar)
        plotStackedProfile(
            gr, what_pattern = patterns[i],
            palette = colorRampPalette(c("white", tints[i], "black")),
            zlim = zlim[[i]], target_height = npix_height, n_core = n_core,
            x_labels = x_labels, title = titles[i]
        )
        par(mar = low_mar)
        plotStackProfileLegend(zlim = zlim[[i]], palette = colorRampPalette(c("white", tints[i], "black")))
        par(mar = low_mar)
        plotAverageProfile(gr, what_pattern = patterns[i], ylim = ylim[[i]], colorPalette = bin_palette, xlabels = x_labels)
    }

    par(oldpar)
    return(invisible())
}




