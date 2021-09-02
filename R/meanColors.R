#' meanColor
#'
#' @description
#' Return the mean color of a vector of colors computed in the RGB space.
#'
#' @param colors vector of colors
#'
#' @return a single color value
#'
#' @export
#'
#' @details Input colors can be either in html or color name formats.
#' The alpha channel is supported but optional.
#'
#' @seealso \code{\link[epistack]{redim_matrix}}
#'
#' @examples
#' meanColor(c("#000000FF", "#FFFFFF00", "#FFFF00FF", "#FF0000FF"))
#'
#' # works with color names
#' meanColor(c("blue", "red"))
#'
#' # Mix color names and HTML codes
#' meanColor(c("blue", "red", "#FFFF00FF"))
#'
#' # works without alpha channel in inputs (but outputs an alpha channel):
#' meanColor(c("#000000", "#FFFFFF", "#FFFF00", "#FF0000"))

meanColor <- function(colors) {
    do.call(
        grDevices::rgb,
        c(
            as.list(rowMeans(grDevices::col2rgb(colors, alpha = TRUE))),
            maxColorValue = 255
        )
    )
}


