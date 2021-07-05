#' plotStackProfileLegend
#'
#' @param zlim the limits of the values to be displayed
#' @param palette color palette,
#'  by default: colorRampPalette(c ("white", "gray", "black"))
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
    palette = colorRampPalette(c("white", "grey", "black"))
) {
    image(
        matrix(seq(zlim[1], zlim[2], length.out = 50)),
        col =  palette(100),
        useRaster = TRUE,
        axes = FALSE
    )
    box()
    axis(1, at = c(0, 1), labels = zlim)
}
