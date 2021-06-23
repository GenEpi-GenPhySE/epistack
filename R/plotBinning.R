#' plotBinning
#'
#' @param gr a gRanges object containing a "bin" column
#' @param target_height an integer, the approximate number of pixels of the final plot.
#' Used to reduce the matrix before plotting to improve performance and reduce overploting artefacts.
#' @param palette a function taking a number has a first argument, and returning a vector of colors.
#'
#' @return a plot
#'
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#' gr <- gRanges_test
#' gr <- addBins(gr, nbins = 3)
#' plot_bin <- plotBinning(gr)
#'
#'
#' gr2 <- data.frame(bin = rep(c(1,2,3,4), each = 5))
#' plotBinning(gr2, palette = colorRampPalette(c("blue4", "forestgreen", "coral3", "goldenrod")))
#'
#'
#'
plotBinning <- function(
    gr,
    target_height = 650,
    palette = colorRampPalette(c("magenta", "black", "green"))
) {
    bins <- as.numeric(factor(gr$bin))
    if (length(bins) > target_height) {
        bins_red <- redim_matrix(matrix(bins), target_height = target_height, target_width = 1)
    } else {
        bins_red <- matrix(bins)
    }

    image(t(bins_red), col = palette(length(unique(bins))),
          axes = FALSE,
          breaks = seq(0.5, length(unique(bins)) + 0.5, by = 1))
    graphics::box()
    mtext(side = 3, "bins", line = 0.5,  cex = 0.8, las = 2)
}


