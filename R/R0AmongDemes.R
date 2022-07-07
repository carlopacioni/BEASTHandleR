#' Reproductive number among demes
#'
#' Calculate HPD for the Reproductive number among demes (\code{R0AmongDemes}),
#' and plots its density.
#'
#' Currently this function assumes that the parameter \code{R0AmongDemes} is fixed
#' across different epochs (i.e. there are not changes over time).
#'
#'  It returns a table with median and HPD intervals and a density plot for each
#'  parameter. These results are arranged in a matrix-like layout, where movements
#'  are from columns to rows.
#'
#' The density plots will have a (black) bar at the bottom. The dot on the bar
#' shows the median, the thick bar the lower and the thin bar the highest
#' credibility intervals requested wtih \code{densBarWidth}.
#'
#' @param densBarWidth The width of the (black) bar at the base of the density
#'    plots.
#' @param fill.col The colour to use to fill the density area
#' @inheritParams extract.HPD
#' @import data.table
#' @import ggplot2
#' @import tidybayes
#' @importFrom boa boa.hpd
#' @export
#'
R0AmongDemes <- function(log, alpha=0.05, densBarWidth=c(0.8, 0.95), fill.col="#F8766D") {

logAmong <- log[, grep("Among", names(log)), with=FALSE]
hpd <- extract.HPD(logAmong, alpha = alpha)
hpd[, fromTo := sub("R0AmongDemesSMEpi.", replacement = "", x = Param)]
fromTo_split <- strsplit(hpd[, fromTo], split = "_to_")

hpd[, From := sapply(fromTo_split, "[", 1)]
hpd[, To := sapply(fromTo_split, "[", 2)]
setkeyv(hpd, c("From", "To"))

demes <- hpd[, unique(From)]
amongMatix <- matrix(nrow = length(demes), ncol = length(demes))
rownames(amongMatix) <- demes
colnames(amongMatix) <- demes
for(r in demes) {
    for(c in demes) {
        values <- hpd[.(r, c),]
        if(nrow(values)) {
            # invert rows and columns to have column to row
            amongMatix[c, r] <- values[, paste0(round(Median, 3), " (",
                                                round(Lower, 3), " - ",
                                                round(Upper, 3), ")")]
        } else {
            next
        }
    }
}


longlog <- melt.data.table(logAmong, measure.vars = names(logAmong))
longlog[, variable := sub("R0AmongDemesSMEpi.", replacement = "", x = variable)]
fromTo_split <- strsplit(longlog[, variable], split = "_to_")

longlog[, From := sapply(fromTo_split, "[", 1)]
longlog[, To := sapply(fromTo_split, "[", 2)]


p <- ggplot(longlog, aes(value)) +
    stat_halfeye(alpha=0.5, .width=densBarWidth, fill=fill.col) +
    facet_grid(To~From) +
    labs(x="R0 among demes", y="Density")

return(list(table=amongMatix, plot=p))
}
