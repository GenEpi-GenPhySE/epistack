#' plotEpistack()
#'
#' @description Given a list of genomic regions,
#' epigenetic signals surrounding these regions, and a score for each regions,
#' plot epigenetic stacks depending on the score. An optional \code{bin} column
#' allow the grouping of several genomic regions to produce average profiles per
#' bins.
#'
#' @param gr a GRanges input.
#' @param patterns a character vector of column prefixes
#' (can be regular expressions) that should match columns of \code{gr}.
#' @param tints a vector of colors to tint the heatmaps.
#' @param titles a titles of each heatmap.
#' @param legends a character vector of legend names for the epistacks.
#' @param x_labels a character vector of length 3 used as x-axis labels.
#' @param zlim the minimum and maximum z values the heatmap.
#' Format: zlim = c (min, max)
#' @param ylim limits of the y axis for bottom plots.
#' Format: ylim = c (min, max)
#' @param metric_col a character, name of a column in \code{gr}
#' such as expression value, peak height, pvalue, fold change, etc.
#' @param metric_title title to be display on the leftmost plots.
#' @param metric_label label of the leftmost plots.
#' @param metric_transfunc a function to transform value of \code{metric_col}
#' before plotting. Useful to apply log10 transformation
#' (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param bin_palette a palette of color,
#' (i.e. a function of parameter n that should retrun n colors), used to color
#' average profiles per bin in the bottom plots.
#' @param npix_height The matrix height is reduced to this number of rows
#' before plotting.
#' Useful to limit overplotting artefacts. It should roughtly be set to
#' the pixel height in the final heatmaps
#' @param n_core number of core used to speedup the martrix resizing.
#' @param high_mar a vector of numerical values
#'  corresponding to the margins of the top figures.
#'  c(bottom, left, top, right)
#' @param low_mar a vector of numerical values
#'  corresponding to the margins of the bottom figures.
#'  c(bottom, left, top, right)
#' @param ... Arguments to be passed to \code{\link[graphics]{par}} such as
#'  \code{cex}
#'
#' @details
#' This function produce a coprenhensive figure including epigenetic heatmaps
#' and average epigenetic profiles from a well formated \code{GRanges} object
#' with expected metadata-columns. It scales resonably well up to hundreds of
#' thousands of genomic regions.
#'
#' The visualisation is centered on an anchor,
#' a set of genomic coordinated that can be transcription start sites or
#' peak center for example. Anchor coordinates are those of the \code{GRanges}
#' used as an input (hereafter \code{gr}).
#'
#' Anchors are plotted from top to bottom in the same order as in \code{gr}.
#' One should sort \code{gr} before plotting if needed.
#'
#' \code{gr} should have a metric column that is used in the leftmost plots.
#' The name of the metric column must be specified to \code{metric_col}.
#' The metric can be transformed before ploting if needed using the
#' \code{metric_transfunc} parameter.
#'
#' The matrix or matrices used to display the heatmap(s) should be passed as
#' additional metadata columns of \code{gr}. Such matrix can be obtained using
#' \code{EnrichedHeatmap::normalizeToMatrix()} for example. The matrix columns
#' names are then specified through \code{patterns} using prefixes, suffixes or
#' regular expressions.
#'
#' If an optionnal \code{bin} column is present in \code{gr}, it will be used
#' to group genomic regions to performed average profile per bins in the bottom
#' plots.
#'
#' Epistack are multipanel plots build using \code{layout()}. Margins for the
#' panels can be specified using \code{high_mar} and \code{low_mar} parameters
#' if needed, especially to avoid text overlaps. The default value should
#' be apprpriate in most situations. Individual component can be plotted
#' using severa \code{epistack} functions such has \code{plotStackProfile()}
#' or \code{plotAverageProfile()}.
#'
#' Ploting more than > 1000 regions has an heatmap can lead to overploting
#' issued as well as some ploting artefacts (such as horizontal white strips).
#' Both issues can be resolved with fidling with the \code{npix_height}
#' parameter. \code{npix_height} should be smaller than the number of regions,
#' and in the same order of magnitude of the final heatmap height in pixels.
#' Last minutes call to the \code{redimMatrix()} function will hapen before
#' plotting using \code{npix_height} as target height. Parameter \code{n_core}
#' is passed to \code{redimMatrix()} to speed up the down-scaling.
#'
#' @export
#'
#' @return Display a plot.
#'
#' @seealso \code{\link[epistack]{plotStackProfile}},
#' \code{\link[epistack]{plotAverageProfile}},
#' \code{\link[epistack]{redimMatrix}},
#' \code{\link[EnrichedHeatmap]{normalizeToMatrix}},
#' \code{\link[epistack]{addMetricAndArrangeGRanges}},
#' \code{\link[epistack]{addBins}}
#'
#' @examples
#' data("stackepi")
#' plotEpistack(stackepi,
#'     metric_col = "exp",
#'     ylim = c(0, 1),
#'     metric_transfunc = function(x) log10(x+1))
#'
plotEpistack <- function(
    gr,
    patterns = "^window_", tints = "gray",
    titles = "", legends = "",
    x_labels = c("-2.5kb", "TSS", "+2.5kb"),
    zlim = c(0, 1), ylim = NULL,
    metric_col = "expr", metric_title = "Metric", metric_label = "metric",
    metric_transfunc = function(x) x,
    bin_palette = colorRampPalette(c("magenta", "black", "green")),
    npix_height = 650, n_core = 1,
    high_mar = c(2.5, 0.6, 4, 0.6), low_mar = c(2.5, 0.6, 0.3, 0.6),
    ...
) {

    n_pattern <- length(patterns)
    bin_present <- !is.null(gr$bin)

    layout_mat <- matrix(seq_len(3 + bin_present * 3 + n_pattern * 3),
                         nrow  = 3)
    layout_heights <- c(1, 0.14, 0.25)
    layout_widths <- if (bin_present) {
        c(0.3, 0.08, rep(0.35, n_pattern))
    } else {
        c(0.3, rep(0.35, n_pattern))
    }

    graphics::layout(layout_mat,
                     heights = layout_heights, widths = layout_widths)
    oldpar <- graphics::par(
        mgp = c(1.5, 0.5, 0),
        mar = high_mar + c(0, 4, 0, 0),
        ...
    )

    plotMetric(
        mcols(gr)[[metric_col]],
        title = metric_title,
        trans_func = metric_transfunc,
        xlab = metric_label
    )
    graphics::par(mar = low_mar)
    graphics::plot.new()
    graphics::par(mar = low_mar + c(0, 4, 0, 0))
    plotBoxMetric(
        gr, trans_func = metric_transfunc,
        palette = bin_palette,
        metric = metric_col,
        title = metric_title,
        ylab = metric_label
    )

    if (bin_present) {
        graphics::par(mar = high_mar)
        plotBinning(gr, target_height = npix_height, palette = bin_palette)
        graphics::par(mar = low_mar)
        graphics::plot.new()
        graphics::plot.new()
    }

    if(!is.list(zlim)) {
        zlim <- lapply(seq_along(patterns), function(x) zlim)
    }
    if(!is.list(ylim)) {
        ylim <- lapply(seq_along(patterns), function(x) ylim)
    }
    if(length(titles) == 1 && length(patterns) > 1) {
        titles = rep(titles, length(patterns))
    }
    if(length(tints) == 1 && length(patterns) > 1) {
        tints = rep(tints, length(patterns))
    }
    if(length(legends) == 1 && length(patterns) > 1) {
        legends = rep(legends, length(patterns))
    }

    for (i in seq_along(patterns)) {
        graphics::par(mar = high_mar)
        plotStackProfile(
            gr, what_pattern = patterns[i],
            palette = colorRampPalette(c("white", tints[i], "black")),
            zlim = zlim[[i]], target_height = npix_height, n_core = n_core,
            x_labels = x_labels, title = titles[i]
        )
        graphics::par(mar = low_mar)
        plotStackProfileLegend(
            zlim = zlim[[i]],
            palette = colorRampPalette(c("white", tints[i], "black")),
            title = legends[i]
        )
        graphics::par(mar = low_mar)
        plotAverageProfile(
            gr,
            what_pattern = patterns[i],
            ylim = ylim[[i]],
            palette = bin_palette,
            xlabels = x_labels
        )
    }

    graphics::par(oldpar)
    graphics::layout(1)
    return(invisible())
}




