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
