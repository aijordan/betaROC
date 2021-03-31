#' Minimum distance fit for empirical ROC curve
#'
#' Uses minimum distance estimation to fit a parametric ROC curve model to the
#' empirical ROC curve at hand.
#'
#' @param empROC \code{data.frame} with columns \code{FPR} and \code{TPR},
#'   false positive and true positive values of an empirical ROC curve.
#' @param MDE_info \code{list} containing information about the required minimum
#'   distance estimation (MDE) fit. Entries are \code{method} and \code{info}.
#' @param maxit maximum number of iterations for the numerical optimization,
#'   a \code{control} parameter for \code{\link[stats]{optim}}.
#'
#' @return Returns a list with initially estimated parameters and corresponding
#'   L2 distance and the final estimated parameters as well as the associated L2
#'   distance
#' @details \code{MDE} relies on optimization of the L2 distance to estimate
#'   the optimal parameters of a parametric ROC curve model.
#'
#' @export
MDE <- function(empROC, MDE_info, maxit = 50){
  if(all(MDE_info$method == "empirical")) return(NULL)
  if (inherits(empROC, "roc")) empROC <- empROC$empROC
  empROC <- dplyr::arrange(empROC, .data$FPR, .data$TPR)
  empROCwide <- interval_roc(empROC)

  L2dist_empROC_parROC <- switch(
    MDE_info$method[1],
    bin2p = L2dist_empROC_bin2p,
    bin3p = L2dist_empROC_bin3p,
    beta2p = L2dist_empROC_beta2p,
    beta3p_v = L2dist_empROC_beta3p_v,
    beta3p_h = L2dist_empROC_beta3p_h,
    beta4p = L2dist_empROC_beta4p
  )

  pars_init <- fit_initial_pars(empROC, MDE_info)
  L2_init <- L2dist_empROC_parROC(pars_init, empROCwide, pencon = FALSE)

  est <- try(stats::optim(
    par      = pars_init,
    fn       = L2dist_empROC_parROC,
    empROCwide   = empROCwide,
    pencon   = MDE_info$info == "concave",
    method   = "BFGS",
    control  = list(trace = FALSE, maxit = maxit)))

  pars_fit <- est$par
  L2_fit <- L2dist_empROC_parROC(pars_fit, empROCwide, pencon = FALSE)

  if (grepl("bin", MDE_info$method[1]) && MDE_info$info == "concave") {
    pars_fit <- c(pars_fit[1], 1, pars_fit[-1])
  }

  res <- list(
    pars_init = pars_init,
    L2_init   = L2_init,
    pars_fit  = pars_fit,
    L2_fit    = L2_fit
  )

  return(res)
}

# Initial parameters ----

# Fits initial parameters for the minimum distance estimation of an empirical
# ROC curve
#
# @inheritParams MDE
# @return to be added
# @details to be added
# @export
fit_initial_pars <- function(empROC, MDE_info){

  MDEm <- MDE_info$method
  MDEi <- MDE_info$info
  empROC <- fill_empROC(empROC)
  gamma <- fit_ipar_gamma(empROC)
  delta <- fit_ipar_delta(empROC)

  if("bin2p" %in% MDEm & MDEi == "unrestricted") pars <- c(1,1)
  if("bin2p" %in% MDEm & MDEi == "concave") pars <- 1
  if("bin3p" %in% MDEm & MDEi == "unrestricted") pars <- c(1,1,gamma)
  if("bin3p" %in% MDEm & MDEi == "concave") pars <- c(1,gamma)

  if("beta2p" %in% MDEm) pars <- c(1,1)
  if("beta3p_v" %in% MDEm) pars <- c(1,1,gamma)
  if("beta3p_h" %in% MDEm) pars <- c(1,1,delta)
  if("beta4p" %in% MDEm) pars <- c(1,1,gamma,delta)

  est <- try(stats::optim(
      par      = pars,
      fn       = roc_sqe,
      empROC   = empROC,
      MDE_info = MDE_info,
      method   = "BFGS"
    ))

  if(any(grepl("beta", MDEm)) & MDEi == "concave")
    pars <- shift_ipar_beta(pars, MDE_info)

  return(pars)
}

# @inheritParams fit_initial_pars
fit_ipar_gamma <- function(empROC){
  if(sum(empROC$FPR == 0) > 0){
    max(empROC$TPR[empROC$FPR == 0])
  }else{
    0
  }
}

# @inheritParams fit_initial_pars
fit_ipar_delta <- function(empROC){
  if(sum(empROC$TPR == 1) > 0){
    min(empROC$FPR[empROC$TPR == 1])
  }else{
    1
  }
}

# Squared error for fit of empirical by parametric ROC curve
# @param pars Parameter vector corresponding to \code{MDE_info$method}
# @inheritParams fit_initial_pars
# @details Computes the sum of the squared errors between an empirical ROC
#   curve and a parametric one specified by pars and MDE_info, evaluated at the
#   FPR values of the empirical ROC curve. In case a concave binormal ROC curve
#   is specified, the parameters have to be adjusted in that sigma = 1 is
#   inserted into the pars vector.
# @export
roc_sqe <- function(pars, empROC, MDE_info){
  MDEm <- MDE_info$method
  MDEi <- MDE_info$info
  if(any(grepl("bin", MDEm)) & MDEi == "concave") pars <- c(pars[1], 1, pars[-1])

  if(any(pars[1:2] <= 0)) return(1)
  if(length(pars) >= 3 & (pars[3] < 0  | pars[3] > 1)) return(1)
  if(length(pars) >= 4 & (pars[4] < 0  | pars[4] > 1)) return(1)

  TPR <- get_TPR(empROC$FPR, pars, MDE_info)
  sqe <- mean((TPR - empROC$TPR)^2)
  return(sqe)
}

# Shifts beta parameters into the triangular region corresponding to concave
# beta ROC curves
#
# @param eps optional; specifies how far into the triangular parameter region
#   corresponding to a concave beta ROC curve the beta parameters are shifted
#   (see details).
# @inheritParams fit_initial_pars
# @details WRITE ABOUT SHIFTING ETC. A value of 0 corresponds to a shift
#   exactly on the edge of the triangular region
shift_ipar_beta <- function(pars, MDE_info, eps = 0.1){

  sepline <- function(x) 3.41-2.41*x
  seplineinv <- function(y) (3.41-y)/2.41

  if(any(grepl("beta", MDE_info$method)) & MDE_info$info == "concave"){

    s_x <- 1-eps/2.41
    s_y <- sepline(s_x)

    if(pars[2] >= 1+eps & pars[1] >= s_x){
      pars[1] <- seplineinv(pars[2])
    }
    if(pars[2] < 1+eps & pars[2]-pars[1] < s_y - s_x){
      pars[1:2] <- c(s_x, s_y)
    }
    if(pars[2]-pars[1] > s_y - s_x & pars[1]+pars[2] <= s_x + s_y){
      s_par <- 1-(2.41*pars[1] + pars[2])/3.41
      pars[1:2] <- pars[1:2] + s_par
    }
  }
  return(pars)
}

# Distance calculation ----

# L2 distance between empirical and parametric ROC curve
#
# @inheritParams MDE
# @param pars Vector of parameters as specified by \code{MDE_info}
# @param pencon specifies if the L2 distance in case of ROC curves that shall
#   be concave, but violate the concavity constraint, shall be penalized.
#   This option is essential for concave MDE estimation (see ...s)

# L2dist_empROC_parROC template:
# function(pars, empROCwide, pencon) {
#   if (invalid_parameter_values) {
#     return(1)
#   }
#   L2dist <- sqrt(integrate(sqdiff, lower = 0, upper = 1,
#                            calc_TPR = get_TPR_parROC, pars = pars,
#                            empROCwide = empROCwide)$value)
#   if (isTRUE(pencon)) {
#     return(penalize_parROC(L2dist, pars))
#   }
#   L2dist
# }
# ...
sqdiff <- function(u, calc_TPR, pars, empROCwide) {
  sapply(u, function(y) {
    with(empROCwide, {
      TPR_par <- calc_TPR((1 - y) * FPR0 + y * FPR1, pars)
      TPR_emp <- (1 - y) * TPR0 + y * TPR1
      sum((FPR1 - FPR0) * (TPR_par - TPR_emp)^2)
    })
  })
}

