#' plotAverageProfile()
#'
#' @description Plot the average stack profiles +/- standard error of the
#' means. If a \code{bin} column is present in \code{gr}, one average profile
#' is drawn for each bin.
#'
#' @param gr a GRanges input
#' @param what_pattern a single character that should match
#'  metadata of \code{gr} (can be a regular expression).
#' @param xlabels x-axis labels.
#' @param palette a color palette function,
#'  by default: \code{colorRampPalette(c("magenta", "black", "green"))}
#' @param alpha_for_se the transparency (alpha) value for
#' standard error of the mean band.
#' @param reversed_z_order should the z-order of the curves be reversed
#' (i.e. first or last bin on top?)
#' @param ylim a vector of two numbers corresponding
#'  to the y-limits of the plot
#'
#' @export
#'
#' @examples
#' data("stackepi")
#' plotAverageProfile(stackepi)
#'
plotAverageProfile <- function(
    gr,
    what_pattern = "^window_",
    xlabels = c("-2.5kb", "TSS", "+2.5kb"),
    palette = colorRampPalette(c("magenta", "black", "green")),
    alpha_for_se = 0.25,
    reversed_z_order = FALSE,
    ylim = NULL
) {
    mat <- S4Vectors::mcols(gr)
    whichCols <- grepl(what_pattern, colnames(mat))
    mat <- as.matrix(mat[, whichCols])

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

    ymax <- max(
        vapply(
            seq_along(myMeans), function(i) {
                max(myMeans[[i]] + mySes[[i]])
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
    axis(1, at = xind[1], labels = xlabels[1], hadj = 0)
    axis(1, at =  xind[(length(xind)+1)/2], labels = xlabels[2], hadj = 0.5)
    axis(1, at = xind[length(xind)], labels = xlabels[3], hadj = 1)
    axis(1, at = c(xind[1], xind[(length(xind)+1)/2], xind[length(xind)]), labels = NA)

    axis(2, at = ylim)

    iter <- rev(seq_along(myMats))
    if (reversed_z_order) {
        iter <- rev(iter)
    }

    for(i in iter) {
        plotrix::dispersion(xind,
                            myMeans[[i]],
                            mySes[[i]],
                            type = "l",
                            fill = grDevices::adjustcolor(mypalette[i],
                                                          alpha_for_se))
        graphics::lines(xind,
                        myMeans[[i]],
                        type = "l",
                        col = mypalette[i], lwd = 2)
    }
}



