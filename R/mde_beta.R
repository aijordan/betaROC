# 2 parameter beta ROC ----
L2dist_empROC_beta2p <- function(pars, empROCwide, pencon) {
  if (any(pars < 0)) {
    return(1)
  }
  L2dist <- sqrt(
    stats::integrate(
      sqdiff,
      lower = 0,
      upper = 1,
      calc_TPR = get_TPR_beta2p,
      pars = pars,
      empROCwide = empROCwide
    )$value
  )
  if (isTRUE(pencon)) {
    return(penalize_beta(L2dist, pars))
  }
  L2dist
}

get_TPR_beta2p <- function(FPR, pars) {
  stats::pbeta(FPR, pars[1], pars[2])
}

# 3 parameter beta ROC ----
L2dist_empROC_beta3p_v <- function(pars, empROCwide, pencon) {
  if (any(pars < 0) || pars[3] > 1) {
    return(1)
  }
  L2dist <- sqrt(
    stats::integrate(
      sqdiff,
      lower = 0,
      upper = 1,
      calc_TPR = get_TPR_beta3p_v,
      pars = pars,
      empROCwide = empROCwide
    )$value
  )
  if (isTRUE(pencon)) {
    return(penalize_beta(L2dist, pars))
  }
  L2dist
}

get_TPR_beta3p_v <- function(FPR, pars) {
  pars[3] + (1 - pars[3]) * get_TPR_beta2p(FPR, pars[1:2])
}

L2dist_empROC_beta3p_h <- function(pars, empROCwide, pencon) {
  if (any(pars < 0) || pars[3] > 1) {
    return(1)
  }
  L2dist <- sqrt(
    stats::integrate(
      sqdiff,
      lower = 0,
      upper = 1,
      calc_TPR = get_TPR_beta3p_h,
      pars = pars,
      empROCwide = empROCwide
    )$value
  )
  if (isTRUE(pencon)) {
    return(penalize_beta(L2dist, pars))
  }
  L2dist
}

get_TPR_beta3p_h <- function(FPR, pars) {
  ifelse(FPR >= pars[3], 1, get_TPR_beta2p(FPR / pars[3], pars[1:2]))
}

# 4 parameter beta ROC ----
L2dist_empROC_beta4p <- function(pars, empROCwide, pencon) {
  if (any(pars < 0) || any(pars[3:4] > 1)) {
    return(1)
  }
  L2dist <- sqrt(
    stats::integrate(
      sqdiff,
      lower = 0,
      upper = 1,
      calc_TPR = get_TPR_beta4p,
      pars = pars,
      empROCwide = empROCwide
    )$value
  )
  if (isTRUE(pencon)) {
    return(penalize_beta(L2dist, pars))
  }
  L2dist
}

get_TPR_beta4p <- function(FPR, pars) {
  ifelse(FPR >= pars[4], 1, get_TPR_beta3p_v(FPR / pars[4], pars[-4]))
}

# nonconcavity penalization ----
penalize_beta <- function(L2dist, pars) {
  penfac <- 1 + 100 * get_distance_to_beta_concave_region(pars)
  penincr <- pars[1] > 1 || sum(pars) < 2
  L2dist * penfac + penincr
}
# Minimum euclidean distance from current beta parameters to concave beta parameters
#
# @inheritParams L2dist_empROC_parROC
get_distance_to_beta_concave_region <- function(pars) {
  alpha <- pars[1]
  beta  <- pars[2]
  if (alpha > 1  && beta >= 1) {
    dist2 <- abs(alpha - 1)
  }
  if (alpha >= 1 & beta < 1) {
    dist2 <- sqrt((alpha - 1)^2 + (beta - 1)^2)
  }
  if (alpha >= beta & alpha < 1 & beta < 1) {
    dist2 <- sqrt((alpha - 1)^2 + (beta - 1)^2)
  }
  if (alpha < beta & beta + alpha < 2 & alpha < 1) {
    dist2 <- sqrt(2 * ((2 - alpha - beta) / 2)^2)
  }
  if (!exists("dist2")) {
    dist2 <- 0
  }
  return(dist2)
}


# beta ROC derivatives ----
get_slope_beta2p <- function(FPR, pars) {
  stats::dbeta(FPR, pars[1], pars[2])
}
get_slope_beta3p_v <- function(FPR, pars) {
  (1 - pars[3]) * get_slope_beta2p(FPR, pars)
}
get_slope_beta3p_h <- function(FPR, pars) {
  ifelse(FPR >= pars[3], 0, get_slope_beta2p(FPR / pars[3], pars))
}
get_slope_beta4p <- function(FPR, pars) {
  ifelse(FPR >= pars[3], 0, get_slope_beta3p_v(FPR / pars[3], pars[-3]))
}
