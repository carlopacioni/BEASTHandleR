#' Generate a skyline plot
#'
#' If more than one set of data are passed, the function will overimpose each
#' set in the same plot. In this case, the data have to be in a list, with each
#' data.table as an element of the list, and an additional column needs to be
#' added to the data. This column, whose name has to be passed with the argument
#' \code{by}, will be used to colourcode the plot and identify the datasets.
#'
#' The plot returned by the function is a ggplot object and can, therefore, be
#' modified as usual (e.g. \code{plot + theme_classic()})
#'
#' @param epochData The starting and ending times of the epochs (generated with
#'   make.epochs.data) or a list (see Details)
#' @param  by the column's name to use to colourcode the skylines
#' @import data.table
#' @export
make.skyplot <- function(epochData, by=NULL) {
    proc.data <- function(epochData) {
        if(nrow(epochData) == 1) stop("There is only one epoch in the data")
        if(epochData[1, TimeMin] > epochData[2, TimeMin]) {
            newepoch <- rbind(epochData[1,], epochData)
            newepoch[1, TimeMin := newepoch[1, TimeMax]]
        } else {
            newepoch <- rbind(epochData, epochData[nrow(epochData),])
            newepoch[nrow(newepoch), TimeMin := newepoch[nrow(newepoch), TimeMax]]
        }
        return(newepoch)
    }
    if(is.list(epochData) && !is.data.table(epochData)) {
        newepoch <- lapply(epochData, proc.data)
        newepoch <- rbindlist(newepoch)
    } else {
        newepoch <- proc.data(epochData)
    }
    skyplot <- ggplot(newepoch, aes_string(col=by, fill=by)) +
        geom_rect(aes(xmin=TimeMin, xmax=TimeMax, ymin=Lower, ymax=Upper), alpha=0.5) +
        geom_step(aes(TimeMin, Median))
    return(skyplot)
}
