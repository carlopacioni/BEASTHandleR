#' Draw a violin plot for each deme and epoch
#'
#' This function generates a violin plot with each deme is plotted (and
#' colour-coded) separately. Each Epoch is plotted as one line in the plot.
#'
#' Within the violin plot there are tree lines, the two most external lines mark
#' the 2.5 and 97.5 percentile. The middle one is the median.
#'
#' The object returned is a ggplot object, which can be modified as usual. For
#' example, in presence of long tails, one may want to trim the y axis like so:
#' \code{p + ylim(c(l, u))}, where p is the plot returned by the function, and
#' \code{l} and \code{u} are the lower and upper desire limits on the y axis. It
#' is important to also note that the first element of the ggplot list returned
#' are the actual data that are used to generate the plot, which can then be
#' used for other down-stream analysis or plots.
#'
#' @param keepScale Whether the y axis should be kept consistent (TRUE) across
#'   all rows of the plot or adjusted depending on the posterior distribution
#'   (FALSE). The second is sometime more convenient for readability.
#' @inheritParams  make.BDMMbyDeme.skyplot
#' @return a ggplot
#' @import data.table
#' @export
make.BDMMbyDeme.violin <- function(log, par=c("R0", "s", "delta", "r"), keepScale=TRUE) {
    param <- match.arg(par)
    switch(param, R0=param <- "R0SVEpi", s=param <- "samplingProportionSVEpi",
           delta=param <- "becomeUninfectiousRateSVEpi", r=param <- "removalProbSVEpi")

    dat <- log[, grep(param, names(log)), with=FALSE]

    # matches <- regexec(paste0(param,"\\.i[0-9]+_"), names(dat))
    # starts <- sapply(matches, attr, "match.length") + 1
    # demes <- substring(names(dat), starts)

    demes <- sapply(strsplit(names(dat), "_"), tail, 1)
    demes <- unique(demes[grep("endtime", demes, invert = TRUE)])
    nDemes <- length(demes)
    dat_demes <- lapply(demes, extract.cols, dt=dat)

    process.epochs <- function(dt, param, par) {
        dt2 <- melt.data.table(dt, measure.vars = patterns(param),
                               variable.name = "Epoch", value.name = par)
        starts <- regexec(paste0(param,".*i[0-9]"), dt2[, Epoch])
        starts <- sapply(starts, attr, "match.length")
        ends <- regexec(paste0(param,".*i[0-9]+"), dt2[, Epoch])
        ends <- sapply(ends, attr, "match.length")
        dt2[, Epoch := substr(Epoch, starts, ends)]
        return(dt2)
    }
    dat_demes <- lapply(dat_demes, process.epochs, param=param, par=par)
    names(dat_demes)  <- demes
    dat_fin <- rbindlist(dat_demes, idcol = "Deme")

    viol_plot <- ggplot(data=dat_fin, aes_string("Deme", par, fill="Deme")) +
        geom_violin(draw_quantiles=c(0.025, 0.5, 0.975)) +
        facet_grid(Epoch~., scales = if(keepScale) "fixed" else "free_y")


    return(viol_plot)
}
