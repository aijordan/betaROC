#' Plot functions for empirical and parametric ROC curves
#'
#' @param x an object of class \code{'roc'}.
#' @param pars parameters for the ROC family given by \code{MDE_info}
#' @inheritParams MDE
#' @param p ggplot object to add a line to
#' @param lty line type
#' @param lwd line width
#' @param color line color
#' @param ... parameters to be passed to \code{geom_line}.
#'
#' @name plot_roc
NULL

#' @rdname plot_roc
#'
#' @export
plot_empirical <- function(x, ...){
  stopifnot(inherits(x, "roc"))
  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = tibble::as_tibble(x$empROC),
      mapping = ggplot2::aes(x = .data$FPR, y = .data$TPR), ...) +
    ggplot2::theme_minimal()
  return(p)
}

#' @rdname plot_roc
#'
#' @export
plot_beta <- function(pars, MDE_info, p, lty, lwd, color, ...){

  if(missing(pars)) stop("pars need to be specified")

  if(missing(MDE_info)){
    if(length(pars) == 2)
      MDE_info <- list(methods = "beta2p", info = "unrestricted")
    if(length(pars) == 3)
      MDE_info <- list(methods = "beta3p_v", info = "unrestricted")
    if(length(pars) == 4)
      MDE_info <- list(methods = "beta4p", info = "unrestricted")
  }

  if (missing(lty)) lty <- 1
  if (missing(lwd)) lwd <- 0.7
  if (missing(color)) color <- "#377EB8"

  FPR <- seq(0, 1, by = 0.005)
  TPR <- get_TPR(FPR, pars, MDE_info)
  tib <- tibble::tibble(FPR = c(0, FPR, 1), TPR = c(0, TPR, 1))

  betainfo <- paste0(round(pars, 1), collapse = "; ")
  subtitle <- paste0("Beta parameters (", betainfo, ")")

  if (missing(p)) {
    p <- ggplot2::ggplot() +
      ggplot2::ggtitle(label = "Beta ROC curve", subtitle = subtitle) +
      ggplot2::theme_minimal()
  }
  p + ggplot2::geom_line(
    data = tib,
    mapping = ggplot2::aes(x = .data$FPR, y = .data$TPR),
    col = color,
    lty = lty,
    lwd = lwd,
    ...)
}

#' @rdname plot_roc
#'
#' @export
plot_binormal <- function(pars, MDE_info, p, lty, lwd, color, ...){

  if(missing(pars)) stop("pars need to be specified")

  if(missing(MDE_info)){
    if(length(pars) == 2)
      MDE_info <- list(methods = "bin2p", info = "unrestricted")
    if(length(pars) == 3)
      MDE_info <- list(methods = "bin3p", info = "unrestricted")
  }

  if(missing(lty)) lty <- 1
  if(missing(lwd)) lwd <- 0.7
  if (missing(color)) color <- "#377EB8"

  FPR <- seq(0, 1, by = 0.005)
  TPR <- get_TPR(FPR, pars, MDE_info)
  tib <- tibble::tibble(FPR = c(0, FPR, 1), TPR = c(0, TPR, 1))

  bininfo <- paste0(round(pars, 1), collapse = "; ")
  subtitle <- paste0("Binormal parameters (", bininfo, ")")

  if (missing(p)) {
    p <- ggplot2::ggplot() +
      ggplot2::ggtitle(label = "Binormal ROC curve", subtitle = subtitle) +
      ggplot2::theme_minimal()
  }
  p + ggplot2::geom_line(
    data = tib,
    mapping = ggplot2::aes(x = .data$FPR, y = .data$TPR),
    col = color,
    lty = lty,
    lwd = lwd,
    ...)
}
