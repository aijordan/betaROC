#
#
#
# pvroc <- function(formula, data, method) {
#
#     if ( missing(data) ) data <- NULL
#
#     # This will be our S3 object at the end
#     res <- list()
#
#     # Setting up the model.frame
#     res$model.frame <- model.frame(formula, data, na.action = na.omit)
#
#
#     # Empirical quick'n'dirty ROC
#     res$fit.empirical <- pvroc_empirical(res$model.frame)
#
#     class(res) <- "pvroc"
#     return(res)
# }
#
# pvroc_empirical <- function(mf) {
#     # Calculate FAR
#     fun <- function(x, mf) {
#         return(list(FAR = sum(mf[,2] > x & mf[,1] == 0) / sum(mf[,2] > x),
#                     TPR = sum(mf[,2] > x & mf[,1] == 1) / sum(mf[,1] == 1)))
#     }
#     at  <- seq(0, 1, length = 100)
#     res <- lapply(at, fun, mf = mf)
#     return(as.data.frame(do.call(rbind, res)))
# }
#
#
# Some S3 methods for our object
plot.pvroc <- function(x, which, ...) {
    avail <- c("hist", "ROC")

    # If "which" is missing: make all plots
    if ( missing(which) ) {
        create <- avail
    # If input is an integer: pick "which" from vector "avail"
    } else if ( inherits(which, c("integer", "numeric")) ) {
        if ( any(which > length(avail)) | any(which <= 0) )
            stop(sprintf(paste("Wrong input for argument \"which\",",
                               "has to be between 0 and %d", length(avail))))
        create <- avail[which]
    # Else match arg, will fail if input is wrong/strange
    } else {
        create <- as.character(sapply(which, function(x, avail)
                                             match.arg(x, avail), avail = avail))
    }


    # Making the plots
    # Keep current user par args!
    hold <- par(no.readonly = TRUE); on.exit(par(hold))

    # Set ask = TRUE iff more than one figure has been requested
    if ( length(create) > 1 ) par(ask = TRUE)

    # ROC plot
    if ( "ROC" %in% create ) {
        plot(x$fit.empirical$FAR, x$fit.empirical$TPR,
             type = "l", col = 2,
             xlim = c(0,1), ylim = c(0,1),
             xaxs = "i", yaxs = "i",
             xlab = "False Alarm Rate",
             ylab = "True Positive Rate",
             main = "Something like ROC")
    }

    # Histogram plot
    if ( "hist" %in% create ) {
        bk     <- seq(0, 1, by = 0.05)
        h_obs  <- hist(mf[,1], breaks = bk, plot = FALSE)
        h_fcst <- hist(mf[,2], breaks = bk, plot = FALSE)

        ylim <- c(-max(h_fcst$density), max(h_obs$density))
        plot(NA, type = "n", xlim = c(0,1), ylim = ylim,
             xaxs = "i", main = "Completely wrong Histogram Plot")
        rect(bk[-1], 0, bk[-length(bk)], h_obs$density, col = "gray79")
        rect(bk[-1], 0, bk[-length(bk)], -h_fcst$density, col = "gray50")
    }

}
#
#
# # Data set
# set.seed(666)
# data <- data.frame(obs = runif(1000, 0,1))
# data$fcst <- pmax(0, pmin(1, data$obs + rnorm(1000, 0, 0.2)))
# data$obs  <- round(data$obs)
#
# # Create pvroc object
# x <- pvroc(obs ~ fcst, data = data)
#
# # Plotting different stuff
# plot(x, "ROC")
# plot(x, "hist")
# plot(x, "R")
# plot(x, c("ROC","hist"))
# plot(x, c("R", "h"))
# plot(x, 1)
# plot(x, 2)
# plot(x, 1:2)
#
#
#
#
#
#
#
#
#
