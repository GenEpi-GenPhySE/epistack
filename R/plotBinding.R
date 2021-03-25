


#' plotBinding
#'
#' @param gr a granges object
#' @param nbin a numbre of bins
#' @param palette a vector of colors, by default, colorRampPalette(c("magenta", "black", "green")) it used
#' @param title a caracter
#'
#' @return a plot
#'
#' @examples
#' plotBinding(my_tss)
plotBinding <- function(gr,
                        nbin = 5,
                        palette = colorRampPalette(c("magenta", "black", "green")),
                        title = "bin"){

    image(t(rev(gr$bin)), col = palette(nbin), axes = FALSE)
    mtext(side = 3, "bin", line = 0.5,  cex = 0.8, las = 2)
}

