#' plotEpistack()
#'
#' @description Given a list of genomic regions,
#' epigenetic signals surrounding these regions, and a score for each regions,
#' plot epigenetic stacks depending on the score. An optional \code{bin} column
#' allow the grouping of several genomic regions to produce average profiles per
#' bins.
#'
#' @param rse a RangedSummarizedExperiment input. Aletrnatively: can be a
#' GRanges object
#' (for backward compatibility, \code{patterns} will be required).
#' @param assays specify the name(s) and order of assay(s) to plot. A vector of
#' names that should match \code{assayNames(rse)}.
#' @param tints a vector of colors to tint the heatmaps. Can alos be a
#' function returning \code{n} colors, or a list of such palette
#' functions.
#' @param titles titles of each heatmap. Defaults to \code{assays}.
#' @param legends legend names for the epistacks.
#' @param main Main title for the figure.
#' @param x_labels a character vector of length 3 used as x-axis labels.
#' @param zlim the minimum and maximum z values the heatmap.
#' Format: \code{zlim = c(min, max)}. \code{zlim} can also be specified of as a list of pairs of limits, on for each assay.
#' @param ylim limits of the y axis for bottom plots. \code{ylim} can also be specified of as a list of pairs of limits, on for each assay.
#' Format: \code{ylim = c(min, max)}
#' @param metric_col a character, name of a column in \code{gr}
#' such as expression value, peak height, pvalue, fold change, etc.
#' @param metric_title title to be display on the leftmost plots.
#' @param metric_label label of the leftmost plots.
#' @param metric_ylab y axis label of the top left plot.
#' @param metric_transfunc a function to transform value of \code{metric_col}
#' before plotting. Useful to apply log10 transformation
#' (i.e. with `trans_func = function(x) log10(x+1)`).
#' @param bin_palette  A vector of colors, or a function that returns
#' a palette of \code{n} colors. Used to color
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
#' @param error_type, error_type, can be either \code{"sd"} (standard deviation),
#' \code{"sem"} (standard error of the mean),
#' or \code{"ci95"} (95% confidence interval). Default: \code{"ci95"}.
#' @param reversed_z_order For the bottom panels: should the z-order of
#' the curves be reversed (i.e. first or last bin on top)?
#' @param rel_widths A named vector of three elements of relative panel widths:
#' `score` is the left-most panel, `bin` is the optionnal binning panels, and
#' `assays` are the panels of the stacked-matrices.
#' Default to `c(score = .35, bin = .08, assays = .35)`
#' @param rel_heights A vector of three elements of relative panel heights.
#' Default to `c(1, .14, .3)`
#' @param patterns only if \code{rse} is of class GRanges.
#' A character vector of column prefixes
#' (can be regular expressions) that should match columns of \code{rse}.
#' @param ... Arguments to be passed to \code{\link[graphics]{par}} such as
#'  \code{cex}
#'
#' @details
#' This function produce a comprehensive figure including epigenetic heatmaps
#' and average epigenetic profiles from a well formated
#' \code{RangedSummarizedExperiment} object
#' with expected rowData metadata columns.
#' It scales resonably well up to hundreds of
#' thousands of genomic regions.
#'
#' The visualisation is centered on an anchor,
#' a set of genomic coordinated that can be transcription start sites or
#' peak center for example.
#' Anchor coordinates are those of the \code{GRanges}
#' used as a rowData in the input  RangedSummarizedExperiment object
#' (hereafter \code{rse}).
#'
#' Anchors are plotted from top to bottom in the same order as in \code{rse}.
#' One should sort \code{rse} before plotting if needed.
#'
#' \code{rse}'s rowData should have a metric column that is used in the
#' leftmost plots.
#' The name of the metric column must be specified to \code{metric_col}.
#' The metric can be transformed before plotting if needed using the
#' \code{metric_transfunc} parameter.
#'
#' The matrix or matrices used to display the heatmap(s) should be passed as
#' assay(s) in \code{rse}. Such matrix can be obtained using
#' \code{EnrichedHeatmap::normalizeToMatrix()} for example. The assay
#' names are then specified through \code{assays}.
#'
#' If an optionnal \code{bin} column is present in \code{rse}'s rowData,
#'  it will be used
#' to group genomic regions to performed average profile per bins in the bottom
#' plots.
#'
#' Epistack are multipanel plots build using \code{layout()}. Margins for the
#' panels can be specified using \code{high_mar} and \code{low_mar} parameters
#' if needed, especially to avoid text overlaps. The default value should
#' be appropriate in most situations. Individual component can be plotted
#' using severa \code{epistack} functions such has \code{plotStackProfile()}
#' or \code{plotAverageProfile()}.
#'
#' Plotting more than > 1000 regions can lead to overplotting
#' issued as well as some plotting artefacts (such as horizontal white strips).
#' Both issues can be resolved with fidling with the \code{npix_height}
#' parameter. \code{npix_height} should be smaller than the number of regions,
#' and in the same order of magnitude of the final heatmap height in pixels.
#' Last minutes call to the \code{redimMatrix()} function will hapen before
#' plotting using \code{npix_height} as target height. Parameter \code{n_core}
#' is passed to \code{redimMatrix()} to speed up the down-scaling.
#'
#' The input can also be a \code{GRanges} object for backward compatibility. See
#' \code{\link[epistack]{GRanges2RSE}}. \code{patterns} would then be required.
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
    rse,
    assays = NULL, tints = "gray",
    titles = NULL, legends = "",
    main = NULL,
    x_labels = c("Before", "Anchor", "After"),
    zlim = c(0, 1), ylim = NULL,
    metric_col = "exp", metric_title = "Metric", metric_label = "metric",
    metric_ylab = NULL,
    metric_transfunc = function(x) x,
    bin_palette = colorRampPalette(c("#DF536B", "black", "#61D04F")),
    npix_height = 650, n_core = 1,
    high_mar = c(2.5, 0.6, 4, 0.6), low_mar = c(2.5, 0.6, 0.3, 0.6),
    error_type = c("ci95", "sd", "sem"),
    reversed_z_order = FALSE,
    rel_widths = c(score = .35, bin = .08, assays = .35),
    rel_heights = c(1, .14, .3),
    patterns = NULL,
    ...
) {

    if (methods::is(rse, "GRanges")) {
        if (is.null(patterns)) {
            stop("patterns must be provided if the input is of class GRanges")
        }
        if (is.null(assays)) {
            assays <- patterns
        }
        rse <- GRanges2RSE(rse, patterns, assays)
    }

    if (is.null(SummarizedExperiment::assayNames(rse))) {
        SummarizedExperiment::assayNames(rse) <- paste0(
            "assay_",
            seq_len(length(SummarizedExperiment::assays(rse)))
        )
    }

    if (is.null(assays)) {
        assays <- SummarizedExperiment::assayNames(rse)
    }

    if (is.null(titles)) {
        titles <- assays
    }

    n_assays <- length(assays)
    bin_present <- !is.null(SummarizedExperiment::rowRanges(rse)$bin)

    layout_mat <- matrix(seq_len(3 + bin_present * 3 + n_assays * 3),
                         nrow  = 3)
    layout_heights <- rel_heights
    if (bin_present) {
        layout_widths <- c(
            rel_widths[["score"]], rel_widths[["bin"]],
            rep(rel_widths[["assays"]], n_assays)
        )
        # boxmetric extend below plotbin
        layout_mat[2, 2] <- 2
        layout_mat[3, 2] <- 2
        layout_mat[layout_mat > 6] <-  layout_mat[layout_mat > 6] - 2
    } else {
        layout_widths <- c(
            rel_widths[["score"]], rep(rel_widths[["assays"]], n_assays)
        )
    }
    # box metric also extend higher
    layout_mat[3, 1] <- 2
    layout_mat[layout_mat > 2] <-  layout_mat[layout_mat > 2] - 1

    graphics::layout(layout_mat,
                     heights = layout_heights, widths = layout_widths)
    oldpar <- graphics::par(
        mgp = c(1.5, 0.5, 0),
        mar = high_mar + c(0, 4, 0, 0),
        ...
    )

    plotMetric(
        S4Vectors::mcols(rse)[[metric_col]],
        title = metric_title,
        trans_func = metric_transfunc,
        xlab = metric_label, ylab = metric_ylab
    )
    graphics::par(mar = low_mar + c(0, 4, 3, 0))
    plotBoxMetric(
        rse, trans_func = metric_transfunc,
        palette = bin_palette,
        metric = metric_col,
        title = metric_title,
        ylab = metric_label
    )

    if (bin_present) {
        graphics::par(mar = high_mar)
        plotBinning(rse, target_height = npix_height, palette = bin_palette)
        graphics::par(mar = low_mar)
    }

    if(!is.list(zlim)) {
        zlim <- lapply(seq_along(assays), function(x) zlim)
    }
    if(!is.list(ylim)) {
        ylim <- lapply(seq_along(assays), function(x) ylim)
    }
    if(length(titles) == 1 && length(assays) > 1) {
        titles = rep(titles, length(assays))
    }
    if(is.function(tints)) {
        tints <- list(tints)
    }
    if(length(tints) == 1 && length(assays) > 1) {
        tints = rep(tints, length(assays))
    }
    if(length(legends) == 1 && length(assays) > 1) {
        legends = rep(legends, length(assays))
    }

    for (i in seq_along(assays)) {
        graphics::par(mar = high_mar)
        if(is.character(tints[[i]])) {
            col_pal <- colorRampPalette(c("white", tints[i], "black"))
        } else {
            col_pal <- tints[[i]]
        }
        plotStackProfile(
            rse, assay = assays[i],
            palette = col_pal,
            zlim = zlim[[i]], target_height = npix_height, n_core = n_core,
            x_labels = x_labels, title = titles[i]
        )
        graphics::par(mar = low_mar)
        plotStackProfileLegend(
            zlim = zlim[[i]],
            palette = col_pal,
            title = legends[i]
        )
        graphics::par(mar = low_mar)
        plotAverageProfile(
            rse,
            assay = assays[i],
            ylim = ylim[[i]],
            palette = bin_palette,
            x_labels = x_labels,
            error_type = error_type,
            reversed_z_order = reversed_z_order,
            y_title = legends[i]
        )
    }

    graphics::layout(1)
    graphics::par(oldpar)

    if (!is.null(main)) {
        graphics::title(main = main, xpd = NA, line = 2.8, ...)
    }

    return(invisible())
}




