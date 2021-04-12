#' Goodness-of-fit test for parametric models of ROC curves
#'
#' @param roc an object of class \code{'roc'}.
#' @param m the number of Monte Carlo replications.
#' @inheritParams MDE
#'
#' @return
#'   returns a list with entries
#'   \tabular{ll}{
#'   \code{L2_empirical} \tab the L2 distance of the fitted ROC curve model to
#'   the empirical ROC curve \cr
#'   \code{L2_reference} \tab a vector of L2 distances from the Monte Carlo
#'   resample procedure \cr
#'   \code{pval} \tab the p-value calculated as \code{(sum(L2_reference >= L2_empirical) + 1) / (m + 1)}
#'   }
#'
#' @export
resample_test <- function(roc, MDE_info, m = 100) {
  stopifnot(inherits(roc, "roc"))
  fit <- MDE(roc, MDE_info)

  n0 <- sum(roc$model.frame$obs == 0)
  n1 <- sum(roc$model.frame$obs == 1)
  if (grepl("bin", MDE_info$method[1])) {
    mu <- fit$pars_fit[1]
    sigma <- fit$pars_fit[2]
    sample_data <- function() {
      sample_binormal(n0, n1, mu, sigma)
    }
  } else if (grepl("beta", MDE_info$method[1])) {
    alpha <- fit$pars_fit[1]
    beta <- fit$pars_fit[2]
    sample_data <- function() {
      sample_beta(n0, n1, alpha, beta)
    }
  }

  if (grepl("bin", MDE_info$method[1]) && MDE_info$info == "concave") {
    pars_init <- fit$pars_fit[1]
  } else {
    pars_init <- shift_ipar_beta(fit$pars_fit, MDE_info)
  }

  L2_reference <- replicate(m, {
    x <- sample_data()
    empROC <- roc(obs ~ forc, x)
    MDE(empROC, MDE_info, pars_init = pars_init)$L2_fit
  })

  list(
    L2_empirical = fit$L2_fit,
    L2_reference = L2_reference,
    pval = (sum(L2_reference >= fit$L2_fit) + 1) / (m + 1)
  )
}


sample_binormal <- function(n0, n1, mu, sigma) {
  # F|0 = N(0, 1)
  # F|1 = N(mu1, sigma1)
  mu1 <- mu / sigma
  sigma1 <- 1 / sigma

  tibble::tibble(obs = c(rep(0, n0), rep(1, n1)),
                 forc = c(stats::rnorm(n0), stats::rnorm(n1, mu1, sigma1)))
}

sample_beta <- function(n0, n1, alpha, beta) {
  # F|0 = U(0, 1)
  # F|1 = Beta(beta, alpha)
  tibble::tibble(obs = c(rep(0, n0), rep(1, n1)),
                 forc = c(stats::runif(n0), stats::rbeta(n1, beta, alpha)))
}
