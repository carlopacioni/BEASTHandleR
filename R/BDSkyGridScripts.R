#' Reads a log file
#'
#' @param filename The name of the file (with the path if necessary)
#' @param burnin The burnin to remove from the log in proportion
#' @import data.table
#' @export
#' @return A data.table
read.log <- function (filename, burnin = 0.1)
{
  logfile <- data.table::fread(cmd=paste("grep -v '^#'",filename))
  n <- nrow(logfile)
  if(all(is.na(logfile[[ncol(logfile)]]))) logfile <- logfile[, -ncol(logfile), with=FALSE]
  return(logfile[floor(burnin * n):n, ])
}

#' Extract a parameter from HPD using a regular expression
#'
#' @param  HPD The HPD table (obtained with extract.HPD)
#' @param param The name (pattern) to use in the regular expression
#' @import data.table
#' @export
#' @return A data.table
extract.param <- function(HPD, param) {
  param <- HPD[grep(param, HPD[, Param]), ]
  return(param)
}

#' Extract the Highest Probability Density (HPD) from logged data
#'
#' This function uses boa::boa.hpd
#'
#' @param log The log (read with read.log)
#' @param alpha The credibility interval (between 0 and 1)
#' @import data.table
#' @import stats
#' @importFrom boa boa.hpd
#' @return A data.table with HPD (lower and upper limits, and median)
#' @export
extract.HPD <- function(log, alpha=0.05) {
  HPD <- log[, lapply(.SD, boa::boa.hpd, alpha)]
  Median <- log[, lapply(.SD, median)]
  HPD <- rbind(HPD, Median)
  # HPD <- log[, lapply(.SD, quantile, probs=c(0 + alpha/2, 0.5, 1 - alpha/2))]
  HPD <- transpose(HPD, keep.names = "Param")
  setnames(HPD, c("Param", "Lower", "Upper", "Median"))
  setcolorder(HPD, c("Param", "Lower", "Median", "Upper"))
  return(HPD)
}

#' Generate the starting and ending times of the epochs
#'
#' skygrid reports as first parameter the most recent, so needs \code{reverse=TRUE}.
#'
#' @param HPD A data.table with HPD (lower and upper limits, and median). This is
#'   the output from extract.HPD
#' @param maxAge The age of the most recent sample.
#' @param gridPoints The times used in the analysis as gridPoints (skygrid) or
#'   changeTimes (BD models)
#' @param rootHeight The median value of the rootHeight
#' @param reverse When \code{FALSE}, the epoch in the log are from the oldest to
#'   the youngest (that is, the first entry for the parameter is for the oldest
#'   epoch). If \code{TRUE} the epoch in the log are from the youngest to the
#'   oldest.
#' @inheritParams extract.HPD
#' @return A data.table
#' @import data.table
#' @import utils
#' @export
make.epochs.data <- function(HPD, maxAge, gridPoints, rootHeight, reverse=FALSE) {
  if(reverse ==TRUE) {
    Time <- c(maxAge, maxAge - gridPoints, maxAge - rootHeight)
    epochs <- HPD[, `:=`(TimeMin=tail(Time, -1), TimeMax=head(Time, -1))]
  } else {
    Time <- c(maxAge - rootHeight, maxAge - gridPoints, maxAge)
    epochs <- HPD[, `:=`(TimeMin=head(Time, -1), TimeMax=tail(Time, -1))]
  }
  return(epochs)
}

#' Extract columns based on param name
#'
#' @param dt The data.table from where to extract the columns
#' @param param The name of the param used internally
#' @import data.table
extract.cols <- function(dt, param) {
  newdt <- dt[, grep(param, names(dt)), with=FALSE]
  return(newdt)
}








































