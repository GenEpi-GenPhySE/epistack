
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
    gr, mycols
){
    dataF <- as.data.frame(gr)
    avg <- colMeans((dataF[, mycols]), na.rm = TRUE)
    plot(avg, type = "l", xlab = "the average profile")
}
