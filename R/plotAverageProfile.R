#' plotAverageProfile()
#'
#' @description Plot the average stack profiles +/-  error (sd or sem).
#' If a \code{bin} column is present in \code{gr}, one average profile
#' is drawn for each bin.
#'
#' @param gr a GRanges input
#' @param pattern a single character that should match
#'  metadata of \code{gr} (can be a regular expression).
#' @param x_labels x-axis labels.
#' @param palette a color palette function,
#'  by default: \code{colorRampPalette(c("magenta", "black", "green"))}
#' @param alpha_for_se the transparency (alpha) value for
#' the error band.
#' @param error_type, can be either \code{sd} (standard deviation)
#' or \code{sem} (standard error of the mean). Default: \code{sem}.
#' @param reversed_z_order should the z-order of the curves be reversed
#' (i.e. first or last bin on top?)
#' @param ylim a vector of two numbers corresponding
#'  to the y-limits of the plot
#'
#' @export
#'
#' @return Display a plot.
#'
#' @examples
#' data("stackepi")
#' plotAverageProfile(stackepi)
#'
plotAverageProfile <- function(
    gr,
    pattern = "^window_",
    x_labels = c("Before", "Anchor", "After"),
    palette = colorRampPalette(c("magenta", "black", "green")),
    alpha_for_se = 0.25,
    error_type = c("sd", "sem"),
    reversed_z_order = FALSE,
    ylim = NULL
) {
    mat <- S4Vectors::mcols(gr)
    whichCols <- grepl(pattern, colnames(mat))
    mat <- as.matrix(mat[, whichCols])
    error_type <- error_type[1]

    if(!is.null(gr$bin)) {
        myMats <- lapply(levels(factor(gr$bin)), function(x) mat[gr$bin == x, ])
    } else {
        myMats <- list(mat)
    }
    myMeans <- lapply(myMats, function(x) colMeans(x, na.rm = TRUE))
    mySds <- lapply(myMats,
                    function(x) apply(x, 2,
                                      function(y) stats::sd(y, na.rm = TRUE)))
    mySes <- lapply(seq_along(myMeans),
                    function(i) ifelse(myMeans[[i]] == 0, 0,
                                       mySds[[i]]/sqrt(nrow(myMats[[i]]))))

    myErr <- if (error_type == "sem") {mySes} else {mySds}
    
    ymax <- max(
        vapply(
            seq_along(myMeans), function(i) {
                max(myMeans[[i]] + myErr[[i]])
            }, 0.0
        )
    )
    xind <- seq_len(ncol(mat))
    if(!is.null(gr$bin)) {
        mypalette <- palette(length(unique(gr$bin)))
    } else {
        mypalette <- palette(1)
    }
    if (is.null(ylim)) {
        ylim <- c(0, ymax)
    }

    plot(NA, xlim = range(xind), ylim = ylim, axes = FALSE, xlab = NA, ylab = NA)
    axis(1, at = xind[1], labels = x_labels[1], hadj = 0)
    axis(1, at =  xind[(length(xind)+1)/2], labels = x_labels[2], hadj = 0.5)
    axis(1, at = xind[length(xind)], labels = x_labels[3], hadj = 1)
    axis(1, at = c(xind[1], xind[(length(xind)+1)/2], xind[length(xind)]), labels = NA)
    axis(2, at = ylim)

    iter <- rev(seq_along(myMats))
    if (reversed_z_order) {
        iter <- rev(iter)
    }

    for(i in iter) {
        plotrix::dispersion(xind,
                            myMeans[[i]],
                            myErr[[i]],
                            type = "l",
                            fill = grDevices::adjustcolor(mypalette[i],
                                                          alpha_for_se))
        graphics::lines(xind,
                        myMeans[[i]],
                        type = "l",
                        col = mypalette[i], lwd = 2)
    }
}



