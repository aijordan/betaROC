
resample_test <- function(empROC, MDE_info, m) {
  stopifnot(inherits(empROC, "roc"))
  fit <- MDE(empROC, MDE_info)

  n0 <- sum(empROC$model.frame$obs == 0)
  n1 <- sum(empROC$model.frame$obs == 1)
  if (isTRUE(grepl("bin", MDE_info$method))) {
    mu <- fit$pars_fit[1]
    sigma <- fit$pars_fit[2]
    sample_data <- function() {
      sample_binormal(n0, n1, mu, sigma)
    }
  } else if (isTRUE(grepl("beta", MDE_info$method))) {
    alpha <- fit$pars_fit[1]
    beta <- fit$pars_fit[2]
    sample_data <- function() {
      sample_beta(n0, n1, alpha, beta)
    }
  }

  L2_reference <- replicate(m, {
    x <- sample_data()
    empROC <- roc(obs ~ forc, x)
    MDE(empROC, MDE_info)$L2_fit
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
