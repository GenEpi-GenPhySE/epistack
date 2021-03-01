

#' plotStackedProfile
#'
#' @param gr a GRanges input
#' @param what_pattern a character
#' @param x_labels a character vectors with column labels to use
#' @param title a character corresponding to the title of the heatmap
#' @param min_value a number
#' @param max_value a number
#' @param palette function to determine the palette of heatmap color
#' @param target_height a number
#' @param summary_func function whitch applicate
#'    the mean function to the gRanges input
#' @param n_core a number : parallel processing
#'
#' @return an heatmap
#' @export
#'
#' @importFrom S4Vectors mcols
#' @importFrom viridisLite viridis
#' @importFrom BiocGenerics image
#' @importFrom graphics axis
#' @importFrom graphics mtext
#'
#'
#'
#' @examples
#' plotStackedProfile(gRanges_test,
#'    max_value = 1,
#'    target_height = length(gRanges_test),
#'    title = "DNA methylation")
plotStackedProfile <- function(
    gr,
    what_pattern = "^window_",
    x_labels = c("-2.5kb", "TSS", "+2.5kb"),
    title = "",
    min_value = NULL, max_value = NULL,
    palette = function(n) viridisLite::viridis(n, direction = -1),
    target_height = 650,
    summary_func = function(x) mean(x, na.rm = TRUE),
    n_core = 1
) {
    mat <- S4Vectors::mcols(gr)
    whichCols <- grepl(what_pattern, colnames(mat))
    mat <- as.matrix(mat[, whichCols])

    mat[is.na(mat)] <- 0
    if (!is.null(min_value)) {
        mat[mat < min_value] <- min_value
    }
    if (!is.null(max_value)) {
        mat[mat > max_value] <- max_value

    }

    min_break <- if (is.null(min_value)) min(mat) else min_value
    max_break <- if (is.null(max_value)) max(mat) else max_value

    breaks <- seq(min_break, max_break, length.out = 100)

    smallMat <- redim_matrix(mat,
                             target_height = target_height,
                             target_width = ncol(mat),
                             summary_func = summary_func,
                             n_core = n_core)

    BiocGenerics::image(
        t(smallMat),
        breaks = breaks,
        col = palette(length(breaks) - 1),
        axes = FALSE
    )
    graphics::axis(1, at = c(0, 0.5, 1), labels = x_labels)
    graphics::mtext(side = 3, title, line = 0.5,  cex = 0.8)
}

