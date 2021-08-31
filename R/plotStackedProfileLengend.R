#' plotStackProfileLegend()
#'
#' @description Utility function to plot the corresponding legend key of
#' \code{plotStackedProfile()}'s plots.
#'
#' @param zlim the limits of the values to be displayed.
#' Format: \code{c(min, max)}
#' @param palette a palette of color,
#' (i.e. a function of parameter n that should retrun n colors).
#' @param title an optionnal title to be display bellow the color legend.
#'
#' @seealso \code{\link[epistack]{plotStackedProfile}}
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics box
#'
#' @export
#'
#' @examples
#' plotStackProfileLegend(zlim = c(0, 2),
#'     palette = colorRampPalette(c("white", "grey", "black")))
#'
plotStackProfileLegend <- function(
    zlim,
    palette = colorRampPalette(c("white", "grey", "black")),
    title = NA
) {
    image(
        matrix(seq(zlim[1], zlim[2], length.out = 50)),
        col =  palette(100),
        useRaster = TRUE,
        axes = FALSE
    )
    box()
    axis(1, at = c(0, 1), labels = zlim)
    axis(1, tick = FALSE, at = 0.5, labels = title)
}
