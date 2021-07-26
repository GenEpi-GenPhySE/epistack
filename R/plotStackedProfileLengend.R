#' plotStackProfileLegend
#'
#' @param zlim the limits of the values to be displayed
#' @param palette color palette,
#' by default: colorRampPalette(c ("white", "gray", "black"))
#' @param title an optionnal title to be display bellow the color legend.
#'
#' @return a png
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics box
#'
#' @export
#'
#' @examples
#' plotStackProfileLegend(zlim = c(0, 2), palette = colorRampPalette(c("white", "grey", "black")))
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
