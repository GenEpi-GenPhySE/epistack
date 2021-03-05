
#' global_plot
#'
#' @param x a gRanges input
#' @param mycols a character
#'
#' @return a png
#'
#' @export
#'
#' @importFrom stringr str_sub
#'
#' @examples
#' bw <- "G11+_fed_R6.bigWig"
#' mysample <- list_file$id_bed[which(list_file$id_bw == bw)]
#' global_plot(gRanges_test)
global_plot <- function(x, mycols = paste0("window_", 1:51)){
    graphics::layout(matrix(1:4, ncol = 2, byrow = TRUE), heights = c(0.8, 0.2))
    graphics::par(mar=c(2.4,2.4,1.8,1.1))
    plotExpressionProfile(x$exp)
    plotStackedProfile(x, max_value = 1)
    graphics::par(mar=c(2.5,2.4,1.5,1.1))
    boxplot_profileExp(x$exp)
    average_profile(x, mycols)
}
