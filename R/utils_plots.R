#' \code{align_caption} aligns the caption at the left if the \code{cowplot}
#' package is available
#'
#' @param p A plot created by \code{ggplot}
#' @param caption optional; the caption to be added
#' @param subtitle optional; the subtitle to be added
#'
#' @return The output is the same plot but with left-aligned caption and a
#'  subtitle if provided
align_caption <- function(p, caption = NULL, subtitle = NULL){

  if(is.null(subtitle)){
    p <- p + ggtitle(label = "")
    p <- cowplot::ggdraw(p) +
      cowplot::draw_text(
        text = caption,
        x = 0.01,
        y = 0.995,
        hjust = 0,
        vjust = 1,
        size = 22)
  }else{
    p <- p + ggtitle(label = "", subtitle = "")

    p <- cowplot::ggdraw(p) +
      cowplot::draw_text(
        text = caption,
        x = 0.01,
        y = 0.98,
        hjust = 0,
        vjust = 1,
        size = 22)

    p <- cowplot::ggdraw(p) +
      cowplot::draw_text(
        text = subtitle,
        x = 0.01,
        y = 0.91,
        hjust = 0,
        vjust = 1,
        size = 16)
  }

  return(p)
}

#' S3 method for \code{roc} object
#'
#' \code{plot.roc} is the S3
#'
#' @param x \code{roc} object
#' @param which optional; specifies which type of plots shall be created. If not
#'   set, all plots are created. Possible values are histogramm, empirical, fit,
#'   roc_uncertainty, parameter_uncertainty, and partial matching is supported.
#' @param ... optional; additional arguments to be passed to the single plotting
#'   functions (see details)
#' @details TBD
#'
plot.roc <- function(x, which, ...) {

  avail <- c("histogramm", "empirical", "fit", "roc_uncertainty",
             "parameter_uncertainty")

  if ( missing(which) ) {
    create <- avail
  } else if ( inherits(which, c("integer", "numeric")) ) {
    if ( any(which > length(avail)) | any(which <= 0) )
      stop(sprintf(paste("Wrong input for argument \"which\",",
                         "has to be between 0 and %d", length(avail))))
    create <- avail[which]
  } else {
    create <- as.character(sapply(which, function(x, avail)
      match.arg(x, avail), avail = avail))
  }

  hold <- par(no.readonly = TRUE); on.exit(par(hold))

  if ( length(create) > 1 ) par(ask = TRUE)

  if ( "empirical" %in% create ) {

  }

  if ( "histogramm" %in% create ) {

  }

  if ( "fit" %in% create ) {

    cat("ANDERE METHODE ALS NUR EMPIRICAL VORHANDEN")

  }

  if ( "roc_uncertainty" %in% create ) {

    cat("ANDERE METHODE ALS NUR EMPIRICAL VORHANDEN")

  }

  if ( "parameter_uncertainty" %in% create ) {

    cat("ANDERE METHODE ALS NUR EMPIRICAL VORHANDEN")

  }
}