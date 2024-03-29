% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotStackedProfileLengend.R
\name{plotStackProfileLegend}
\alias{plotStackProfileLegend}
\title{plotStackProfileLegend}
\usage{
plotStackProfileLegend(
  zlim,
  palette = colorRampPalette(c("white", "grey", "black")),
  title = NA
)
}
\arguments{
\item{zlim}{the limits of the values to be displayed}

\item{palette}{color palette,
by default: colorRampPalette(c ("white", "gray", "black"))}

\item{title}{an optionnal title to be display bellow the color legend.}
}
\value{
a png
}
\description{
plotStackProfileLegend
}
\examples{
plotStackProfileLegend(zlim = c(0, 2), palette = colorRampPalette(c("white", "grey", "black")))

}
