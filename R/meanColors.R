#' mean_color
#'
#' @param mycolors vector of colors
#'
#' @return a color
#'
#' @examples
#' mean_color(c("#000000FF", "#FFFFFFFF", "#FFFF00FF", "#FF0000FF"))
#'
#'plot(matrix(1:10), col = mean_color(c("#0FF000FF", "#FFF000FF")))
#'
mean_color <- function(mycolors) {
    R     <- strtoi(x = substr(mycolors,2,3), base = 16)
    G     <- strtoi(x = substr(mycolors,4,5), base = 16)
    B     <- strtoi(x = substr(mycolors,6,7), base = 16)
    alpha <- strtoi(x = substr(mycolors,8,9), base = 16)

    return(
        grDevices::rgb(
            red   = round(mean(R)),
            green = round(mean(G)),
            blue  = round(mean(B)),
            alpha = round(mean(alpha)),
            maxColorValue = 255
        )
    )
}


