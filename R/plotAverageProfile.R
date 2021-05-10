#' plotAverageProfile
#'
#' @param gr a gRanges input
#' @param mycols a character
#' @param ylim a vector of number corresponding to the limits of the values to be displayed
#' @param labels a vector of character corresponding to the labels of the x-axis
#'
#' @return a plot
#' @export
#'
#' @importFrom plotrix dispersion
#'
#' @examples
#' mycols <- paste0("window_", 1:51 )
#' plotAverageProfile(gRanges_test, mycols, ylim = c(0, 2))

# plotAverageProfile <- function(
#     gr, mycols, ylim,
#     labels = c("-2.5kb", "TSS", "+2.5kb")
# ){
#     dataF <- as.data.frame(gr)
#     avg <- colMeans((dataF[, mycols]), na.rm = TRUE)
#     plot(avg, type = "l", xlab = NA, axes = FALSE, ylim = ylim, lwd = 2)
#     axis(1, at = seq(1, length(mycols), length.out = 3), labels = labels)
#     axis(2, at = ylim)
# }

plotAverageProfile <- function(
    gr,
    what_pattern = "^window_",
    xlabels = c("-2.5kb", "TSS", "+2.5kb"),
    colorPalette = colorRampPalette(c("magenta", "black", "green")),
    alphaForSe = 0.25,
    reversedZOrder = FALSE,
    ylim = NULL
) {

    mat <- S4Vectors::mcols(gr)
    whichCols <- grepl(what_pattern, colnames(mat))
    mat <- as.matrix(mat[, whichCols])

    if(!is.null(gr$bin)) {
        myMats <- lapply(unique(gr$bin), function(x) mat[gr$bin == x, ])
    } else {
        myMats <- list(mat)
    }
    myMeans <- lapply(myMats, function(x) colMeans(x, na.rm = TRUE))
    mySds <- lapply(myMats, function(x) apply(x, 2, function(y) sd(y, na.rm = TRUE)))
    mySes <- lapply(seq_along(myMeans), function(i) ifelse(myMeans[[i]] == 0, 0, mySds[[i]]/sqrt(nrow(myMats[[i]]))))

    ymax <- max(
        sapply(
            seq_along(myMeans), function(i) {
                max(myMeans[[i]] + mySes[[i]])
            }
        )
    )
    xind <- seq_len(ncol(mat))

    if(!is.null(gr$bin)) {
        myColorPalette <- colorPalette(length(unique(gr$bin)))
    } else {
        myColorPalette <- colorPalette(1)
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
    if (reversedZOrder) {
        iter <- rev(iter)
    }

    for(i in iter) {
        plotrix::dispersion(xind, myMeans[[i]], mySes[[i]], type = "l", fill = adjustcolor(myColorPalette[i], alphaForSe))
        lines(xind, myMeans[[i]], type = "l", col = myColorPalette[i], lwd = 2)
    }
}
