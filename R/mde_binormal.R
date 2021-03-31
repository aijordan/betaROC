# 2 parameter binormal ROC ----
L2dist_empROC_bin2p <- function(pars, empROCwide, pencon) {
  if (any(pars < 0)) {
    return(1)
  }
  intsqdiff <- stats::integrate(
    sqdiff,
    lower = 0,
    upper = 1,
    calc_TPR = get_TPR_bin2p,
    pars = pars,
    empROCwide = empROCwide
  )$value
  sqrt(intsqdiff)
}

get_TPR_bin2p <- function(FPR, pars) {
  if (length(pars) == 1L) {
    return(stats::pnorm(stats::qnorm(FPR, pars[1])))
  }
  stats::pnorm(stats::qnorm(FPR, pars[1], pars[2]))
}

# 3 parameter binormal ROC ----
L2dist_empROC_bin3p <- function(pars, empROCwide, pencon) {
  if (any(pars < 0) || pars[length(pars)] > 1) {
    return(1)
  }
  intsqdiff <- stats::integrate(
    sqdiff,
    lower = 0,
    upper = 1,
    calc_TPR = get_TPR_bin3p,
    pars = pars,
    empROCwide = empROCwide
  )$value
  sqrt(intsqdiff)
}

get_TPR_bin3p <- function(FPR, pars) {
  if (length(pars) == 2L) {
    pars[2] + (1 - pars[2]) * get_TPR_bin2p(FPR, pars[1])
  }
  pars[3] + (1 - pars[3]) * get_TPR_bin2p(FPR, pars[1:2])
}


# binormal ROC derivatives ----
get_slope_bin2p <- function(FPR, pars) {
  stats::dnorm(stats::qnorm(FPR, pars[1], pars[2])) * pars[2] / stats::dnorm(stats::qnorm(FPR))
}
get_slope_bin3p <- function(FPR, pars) {
  (1 - pars[3]) * get_slope_bin2p(FPR, pars)
}
