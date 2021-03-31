context("Correctness of minimum distance estimation for emp ROC curves")





FPR <- seq(0, 1, length.out = 100)
methods <- c("bin2p", "bin3p", "beta2p", "beta3p_v", "beta3p_h", "beta4p")

for(i in seq_along(methods)) {
  assign(paste0("MDE_info_", methods[i]),
         list(method = methods[i], info = "unrestricted"))
}

pars_bin2p <- c(1, 1)
pars_bin3p <- c(1, 1, 0.5)
pars_beta2p <- c(0.7, 2)
pars_beta3p_v <- c(0.7, 2, 0.2)
pars_beta3p_h <- c(0.7, 2, 0.8)
pars_beta4p <- c(0.7, 2, 0.2, 0.8)

for(i in 1:length(methods)) {
  assign(paste0("empROC_", methods[i]),
         tibble::tibble(FPR = FPR,
                        TPR = get_TPR(FPR,
                                      get(
                                        paste0("pars_", methods[i])
                                      ),
                                      get(
                                        paste0("MDE_info_", methods[i])
                                      ))))
}

test_that("all unrestricted MDE fits are approximately correct",{

  for(i in seq_along(methods)) {

    print(methods[i])

    pars <- get(paste0("pars_", methods[i]))
    empROC <- get(paste0("empROC_", methods[i]))
    MDE_info <- get(paste0("MDE_info_", methods[i]))

    pars_fit <- MDE(empROC, MDE_info)

    expect_equal(pars, pars_fit$pars_fit, tolerance = .1)
  }

})



for(i in seq_along(methods)) {
  assign(paste0("MDE_info_", methods[i]),
         list(method = methods[i], info = "concave"))
}

pars_bin2p <- c(1.1, 0.9)
pars_bin3p <- c(1.1, 0.9, 0.5)
pars_beta2p <- c(0.7, 1.25)
pars_beta3p_v <- c(0.7, 1.25, 0.2)
pars_beta3p_h <-c(0.7, 1.25, 0.8)
pars_beta4p <- c(0.7, 1.25, 0.2, 0.8)

for(i in seq_along(methods)) {
  assign(paste0("empROC_", methods[i]),
         tibble::tibble(FPR = FPR,
                    TPR = get_TPR(FPR,
                                  get(
                                    paste0("pars_", methods[i])
                                  ),
                                  get(
                                    paste0("MDE_info_", methods[i])
                                  ))))
}

test_that("all MDE fits under the concavity constraint are concave",{

  for(i in c(1)) {#seq_along(methods)) {

    print(methods[i])

    pars <- get(paste0("pars_", methods[i]))
    empROC <- get(paste0("empROC_", methods[i]))
    MDE_info <- get(paste0("MDE_info_", methods[i]))

    pars_fit <- MDE(empROC, MDE_info)

    if(any(grepl("bin", MDE_info$method)))
      expect_equal(pars_fit$pars_fit[2], 1, tolerance = .001)
  }

})


