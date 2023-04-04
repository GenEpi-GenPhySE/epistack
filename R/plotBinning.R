#' plotBinning()
#'
#' @description Plot a vertical color bar of the \code{bin} column.
#'
#' @param rse a RangedSummarizedExperiment input with a column \code{bin} in
#' \code{rowRanges(rse)}.
#' Alternatively (for backward compatibility),
#' a GRanges object or any object such as \code{rse$bin} exists.
#' @param target_height an integer, the approximate height (in pixels)
#'  of the final plot. Used to avoid overplotting artefacts.
#' @param palette A vector of colors, or a function that returns
#' a palette of \code{n} colors.
#'
#' @importFrom grDevices rainbow
#' @importFrom methods is
#'
#' @return Display a plot.
#'
#' @export
#'
#' @examples
#' data("stackepi")
#' rse <- stackepi
#' rse <- addBins(rse, nbins = 3)
#' plotBinning(rse)
#'
#' gr2 <- data.frame(bin = rep(c(1,2,3,4), each = 5))
#' plotBinning(gr2, palette = colorRampPalette(c("blue4", "forestgreen", "coral3", "goldenrod")))
#'
plotBinning <- function(
    rse,
    target_height = 650,
    palette = colorRampPalette(c("#DF536B", "black", "#61D04F"))
) {
    if(is.character(palette)) {
        palette <- colorRampPalette(palette)
    }

    if (methods::is(rse, "RangedSummarizedExperiment")) {
        bins <- as.numeric(factor(SummarizedExperiment::rowRanges(rse)$bin))
    } else {
        bins <- as.numeric(factor(rse$bin))
    }

    if (length(bins) > target_height) {
        bins_red <- redimMatrix(
            matrix(bins),
            target_height = target_height, target_width = 1
        )
    } else {
        bins_red <- matrix(bins)
    }

    image(t(bins_red)[, seq(from = nrow(bins_red), to = 1, by = -1), drop = FALSE],
          col = palette(length(unique(bins))),
          axes = FALSE,
          breaks = seq(0.5, length(unique(bins)) + 0.5, by = 1))
    graphics::box()
    mtext(side = 3, "bins", line = 0.5,
          cex = graphics::par()$cex.main * 0.8)
}
