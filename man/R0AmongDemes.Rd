% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/R0AmongDemes.R
\name{R0AmongDemes}
\alias{R0AmongDemes}
\title{Reproductive number among demes}
\usage{
R0AmongDemes(
  log,
  alpha = 0.05,
  densBarWidth = c(0.8, 0.95),
  fill.col = "#F8766D"
)
}
\arguments{
\item{log}{The log (read with read.log)}

\item{alpha}{The credibility interval (between 0 and 1)}

\item{densBarWidth}{The width of the (black) bar at the base of the density
plots.}

\item{fill.col}{The colour to use to fill the density area}
}
\description{
Calculate HPD for the Reproductive number among demes (\code{R0AmongDemes}),
and plots its density.
}
\details{
Currently this function assumes that the parameter \code{R0AmongDemes} is fixed
across different epochs (i.e. there are not changes over time).

 It returns a table with median and HPD intervals and a density plot for each
 parameter. These results are arranged in a matrix-like layout, where movements
 are from columns to rows.

The density plots will have a (black) bar at the bottom. The dot on the bar
shows the median, the thick bar the lower and the thin bar the highest
credibility intervals requested with \code{densBarWidth}.
}
