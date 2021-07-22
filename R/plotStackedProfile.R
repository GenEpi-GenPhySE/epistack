#' plotStackedProfile
#'
#' @param gr a GRanges input
#' @param what_pattern a character
#' @param x_labels a character vectors with column labels to use
#' @param title a character corresponding to the title of the heatmap
#' @param zlim = the minimum and maximum z values for which colors should be plotted
#' @param palette function to determine the palette of heatmap color
#' @param target_height a number
#' @param summary_func function whitch applicate
#'    the mean function to the gRanges input
#' @param n_core a number : parallel processing
#'
#' @return a png
#' @export
#'
#' @importFrom S4Vectors mcols
#' @importFrom viridisLite viridis
#' @importFrom BiocGenerics image
#' @importFrom graphics axis
#' @importFrom graphics mtext
#'
#' @examples
#' data("gRanges_test")
#' plotStackedProfile(gRanges_test,
#'                    target_height = 650,
#'                    zlim = c(0, 1),
#'                    palette = colorRampPalette(c("magenta", "black", "green")),
#'                    title = "DNA methylation")
#'
plotStackedProfile <- function(
    gr,
    what_pattern = "^window_",
    x_labels = c("-2.5kb", "TSS", "+2.5kb"),
    title = "",
    zlim = NULL,
    palette = function(n) viridisLite::viridis(n, direction = -1),
    target_height = 650,
    summary_func = function(x) mean(x, na.rm = TRUE),
    n_core = 1
) {
    mat <- S4Vectors::mcols(gr)
    whichCols <- grepl(what_pattern, colnames(mat))
    mat <- as.matrix(mat[, whichCols])

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

    smallMat <- redim_matrix(mat,
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
    box()
    graphics::axis(1, at = 0, labels = x_labels[1], hadj = 0)
    graphics::axis(1, at = 0.5, labels = x_labels[2], hadj = 0.5)
    graphics::axis(1, at = 1, labels = x_labels[3], hadj = 1)

    graphics::mtext(side = 3, title, line = 0.5,  cex = 0.8)
}



