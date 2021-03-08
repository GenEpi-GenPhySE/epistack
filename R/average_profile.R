
#' average_profile
#'
#' @param gr a gRanges input
#' @param mycols a character
#'
#' @return a plot
#' @export
#'
#' @examples
#' mycols <- paste0("window_", 1:51)
#' average_profile(gRanges_test, mycols)
average_profile <- function(
    gr, mycols, ylim,
    labels = c("-2.5kb", "TSS", "+2.5kb")
){
    dataF <- as.data.frame(gr)
    avg <- colMeans((dataF[, mycols]), na.rm = TRUE)
    plot(avg, type = "l", xlab = NA, axes = FALSE, ylim = ylim, lwd = 2)
    axis(1, at = seq(1, length(mycols), length.out = 3), labels = labels)
    axis(2, at = c(0, 2))
}



