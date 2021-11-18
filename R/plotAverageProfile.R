#' plotAverageProfile()
#'
#' @description Plot the average stack profiles +/-  error (sd or sem).
#' If a \code{bin} column is present in \code{rowRanges(rse)},
#' one average profile is drawn for each bin.
#'
#' @param rse a RangedSummarizedExperiment input. Aletrnatively: can be a
#' GRanges object
#' (for backward compatibility, \code{pattern} will be required).
#' @param assay specify the name of the assay to plot,
#' that should match one of \code{assayNames(rse)}.
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
#' @param pattern only if \code{rse} is of class GRanges.
#' A single character that should match
#' metadata of \code{rse} (can be a regular expression).
#' @export
#'
#' @return Display a plot.
#'
#' @examples
#' data("stackepi")
#' plotAverageProfile(stackepi)
#'
plotAverageProfile <- function(
    rse,
    assay = NULL,
    x_labels = c("Before", "Anchor", "After"),
    palette = colorRampPalette(c("magenta", "black", "green")),
    alpha_for_se = 0.25,
    error_type = c("sd", "sem"),
    reversed_z_order = FALSE,
    ylim = NULL,
    pattern = NULL
) {

    if (methods::is(rse, "GRanges")) {
        if (is.null(pattern)) {
            stop("pattern must be provided if the input is of class GRanges")
        }
        if (is.null(assay)) {
            assay <- pattern
        }
        rse <- GRanges2RSE(rse, pattern, assay)
    }

    if (is.null(SummarizedExperiment::assayNames(rse))) {
        SummarizedExperiment::assayNames(rse) <- paste0(
            "assay_",
            seq_len(length(SummarizedExperiment::assays(rse)))
        )
    }

    if (is.null(assay)) {
        assay <- SummarizedExperiment::assayNames(rse)[[1]]
    }

    mat <- SummarizedExperiment::assay(rse, assay)
    error_type <- error_type[1]

    if(!is.null(SummarizedExperiment::rowRanges(rse)$bin)) {
        myMats <- lapply(
            levels(factor(SummarizedExperiment::rowRanges(rse)$bin)),
            function(x) mat[SummarizedExperiment::rowRanges(rse)$bin == x, ]
        )
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
    if(!is.null(SummarizedExperiment::rowRanges(rse)$bin)) {
        mypalette <- palette(
            length(unique(SummarizedExperiment::rowRanges(rse)$bin))
        )
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



