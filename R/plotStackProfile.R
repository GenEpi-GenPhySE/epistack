#' plotStackProfile()
#'
#' @description Display a heatmap of an epigenetic track centered at
#' genomic anchors such as Transcription Start Sites or peak center.
#'
#' @param gr a GRanges input
#' @param pattern a character vector of length 1 of a column prefixe
#' (can be regular expressions) that should match columns of \code{gr}.
#' @param x_labels a character vectors of length 3 used to label the x-axis.
#' @param title The title of the heatmap
#' @param zlim The minimum and maximum z values to match color to values.
#' Format: zlim = c (min, max)
#' @param palette a palette of color,
#' (i.e. a function of parameter n that should retrun n colors).
#' @param target_height The matrix height is reduced to this number of rows
#' before plotting.
#' Useful to limit overplotting artefacts. It should roughtly be set to
#' the pixel height in the final heatmap.
#' @param summary_func function passed to \code{redimMatrix()}.
#' Usualy \code{mean}, but can be set to \code{median} or \code{max} for sparse
#' matrices.
#' @param n_core multicore option, passed to \code{redimMatrix()}.
#'
#' @details
#' The visualisation is centered on an anchor,
#' a set of genomic coordinated that can be transcription start sites or
#' peak center for example. Anchor coordinates are those of the \code{GRanges}
#' used as an input (hereafter \code{gr}).
#'
#' Anchors are plotted from top to bottom in the same order as in \code{gr}.
#' One should sort \code{gr} before plotting if needed.
#'
#' The matrix used to display the heatmap should be passed as
#' additional metadata columns of \code{gr}. Such matrix can be obtained using
#' \code{EnrichedHeatmap::normalizeToMatrix()} for example. The matrix columns
#' names are then specified through \code{what_pattern} using a prefix, a suffix
#' or a regular expressions.
#'
#' This function scale reasonnably wells up to hundred thousands
#' of regions. Overplotting issues are solved by last-minute reduction of the
#' matrix size using \code{redimMatrix()}.
#'
#' @seealso \code{\link[epistack]{plotAverageProfile}},
#' \code{\link[epistack]{plotEpistack}},
#' \code{\link[EnrichedHeatmap]{normalizeToMatrix}},
#' \code{\link[epistack]{plotStackProfileLegend}}
#'
#' @export
#'
#' @return Display a plot.
#'
#' @importFrom S4Vectors mcols
#' @importFrom viridisLite viridis
#' @importFrom BiocGenerics image
#' @importFrom graphics axis
#' @importFrom graphics mtext
#'
#' @examples
#' data("stackepi")
#' plotStackProfile(stackepi,
#'                    target_height = 650,
#'                    zlim = c(0, 1),
#'                    palette = colorRampPalette(c("white", "dodgerblue", "black")),
#'                    title = "DNA methylation")
#'
plotStackProfile <- function(
    rse,
    assay = NULL,
    x_labels = c("Before", "Anchor", "After"),
    title = "",
    zlim = NULL,
    palette = function(n) viridisLite::viridis(n, direction = -1),
    target_height = 650,
    summary_func = function(x) mean(x, na.rm = TRUE),
    n_core = 1,
    pattern = NULL
) {
    mat <- SummarizedExperiment::assay(rse, assay)

    mat[is.na(mat)] <- 0
    if (!is.null(zlim)) {
        mat[mat < zlim[1]] <- zlim[1]
    }
    if (!is.null(zlim)) {
        mat[mat > zlim[2]] <- zlim[2]
    }

    min_break <- if (is.null(zlim[1])) min(mat) else zlim[1]
    max_break <- if (is.null(zlim[2])) max(mat) else zlim[2]

    breaks <- seq(zlim[1], zlim[2], length.out = 100)

    smallMat <- redimMatrix(mat,
                             target_height = target_height,
                             target_width = ncol(mat),
                             summary_func = summary_func,
                             n_core = n_core)

    BiocGenerics::image(
        t(smallMat)[, seq(from = nrow(smallMat), to = 1, by = -1), drop = FALSE],
        breaks = breaks,
        col = palette(length(breaks) - 1),
        axes = FALSE
    )
    graphics::box()
    graphics::axis(1, at = 0, labels = x_labels[1], hadj = 0)
    graphics::axis(1, at = 0.5, labels = x_labels[2], hadj = 0.5)
    graphics::axis(1, at = 1, labels = x_labels[3], hadj = 1)
    graphics::mtext(side = 3, title, line = 0.5,
                    cex = 0.8 * graphics::par()$cex.main)
}



