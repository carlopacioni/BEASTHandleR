% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BDSkyGridScripts.R
\name{make.BDMMbyDeme.violin}
\alias{make.BDMMbyDeme.violin}
\title{Draw violin plot for each deme and epoch}
\usage{
make.BDMMbyDeme.violin(log, par = c("R0", "s", "delta", "r"), keepScale = TRUE)
}
\arguments{
\item{log}{The log (read with read.log)}

\item{par}{the parameter to be plotted. Possible option: "R0", "s" (sampling
proportion), "delta" (becomingUninfectiousRate), "r" (removevalProbability)}

\item{keepScale}{Whether the y axis should be kept consistent (TRUE) across
all rows of the plot or adjusted depending on the posterior distribution
(FALSE). The second is sometime more convenient for readability.}
}
\value{
a ggplot
}
\description{
This function generates a violin plot with each deme is plotted (and
colour-coded) separately. Each Epoch is plotted as one line in the plot.
}
\details{

}
