context("Correctness of minimum distance estimation for emp ROC curves")





FPR <- seq(0, 1, by = 0.005)
methods <- c("bin2p", "bin3p", "beta2p", "beta3p_v", "beta3p_h", "beta4p")

for(i in 1:length(methods)){
  assign(paste0("MDE_info_", methods[i]),
         list(methods = c(methods[i], "empirical"), info = "unrestricted"))
}

pars_bin2p <- c(1, 1)
pars_bin3p <- c(1, 1, 0.5)
pars_beta2p <- c(0.7, 2)
pars_beta3p_v <- c(0.7, 2, 0.2)
pars_beta3p_h <- c(0.7, 2, 0.8)
pars_beta4p <- c(0.7, 2, 0.2, 0.8)

for(i in 1:length(methods)){
  assign(
    paste0("empROC_", methods[i]),
    data.frame(FPR = FPR,
               TPR = get_TPR(FPR,
                             get(paste0("pars_", methods[i])),
                             get(paste0("MDE_info_", methods[i]))
                             )
               ))

  if(grepl("bin", methods[i]))
    p <- plot_binormal(pars = get(paste0("pars_", methods[i])))
  if(grepl("beta", methods[i]))
    p <- plot_beta(pars = get(paste0("pars_", methods[i])),
              MDE_info = get(paste0("MDE_info_", methods[i])))

  print(p)
}

test_that("all unrestricted MDE fits are approximately correct",{

  for(i in 1:length(methods)){

    print(methods[i])

    pars <- get(paste0("pars_", methods[i]))
    empROC <- as.tibble(get(paste0("empROC_", methods[i])))
    selec  <- sample(1:nrow(empROC))[1:ceiling(nrow(empROC)/2)]
    empROC <- empROC[selec, ] %>% arrange(FPR, TPR)
    MDE_info <- get(paste0("MDE_info_", methods[i]))

    pars_fit <- fit_MDE(empROC, MDE_info, maxit = 10)

    expect_equal(pars, pars_fit$pars_fit, tolerance = .01)
  }

})







for(i in 1:length(methods)){
  assign(paste0("MDE_info_", methods[i]),
         list(methods = c(methods[i], "empirical"), info = "concave"))
}

pars_bin2p <- c(1.1, 0.9)
pars_bin3p <- c(1.1, 0.9, 0.5)
pars_beta2p <- c(0.7, 1.25)
pars_beta3p_v <- c(0.7, 1.25, 0.2)
pars_beta3p_h <-c(0.7, 1.25, 0.8)
pars_beta4p <- c(0.7, 1.25, 0.2, 0.8)

for(i in 1:length(methods)){
  assign(
    paste0("empROC_", methods[i]),
    data.frame(FPR = FPR,
               TPR = get_TPR(FPR,
                             get(paste0("pars_", methods[i])),
                             get(paste0("MDE_info_", methods[i]))
               )
    ))

  if(grepl("bin", methods[i]))
    p <- plot_binormal(pars = get(paste0("pars_", methods[i])))
  if(grepl("beta", methods[i]))
    p <- plot_beta(pars = get(paste0("pars_", methods[i])),
                   MDE_info = get(paste0("MDE_info_", methods[i])))

  print(p)
}

test_that("all MDE fits under the concavity constraint are concave",{

  for(i in 1:length(methods)){

    print(methods[i])

    pars <- get(paste0("pars_", methods[i]))
    empROC <- as.tibble(get(paste0("empROC_", methods[i])))
    selec  <- sample(1:nrow(empROC))[1:ceiling(nrow(empROC)/2)]
    empROC <- empROC[selec, ] %>% arrange(FPR, TPR)
    MDE_info <- get(paste0("MDE_info_", methods[i]))

    pars_fit <- fit_MDE(empROC, MDE_info, maxit = 10)

    if(any(grepl("bin", MDE_info$methods)))
      expect_equal(pars_fit$pars_fit[2], 1, tolerance = .001)
  }

})


