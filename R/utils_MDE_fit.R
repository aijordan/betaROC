#' Minimum distance fit for empirical ROC curve
#'
#' Uses minimum distance estimation to fit a parametric ROC curve model to the
#' empirical ROC curve at hand.
#'
#' @param empROC \code{data.frame} with true positive (TPR) and false positive
#'   (FPR) values of the empirical ROC curve, sorted by increasing FPR and TPR.
#' @param MDE_info \code{list} containing information about the required minimum
#'   distance estimation (MDE) fit. Entries are \code{method} and \code{info}.
#'
#' @return Returns a list with initially estimated parameters and corresponding
#'   L2 distance and the final estimated parameters as well as the associated L2
#'   distance
#' @details \code{fit_MDE} relies on optimization of the L2 distance to estimate
#'   the optimal parameters of a parametric ROC curve model.
#'
fit_MDE <- function(empROC, MDE_info, maxit){

  if(missing(empROC)) stop("no empirical ROC curve provided")
  if(missing(MDE_info)) stop("no information about MDE fit provided")
  if(all(MDE_info$method == "empirical")) return(NULL)
  if(missing(maxit)) maxit <- 50

  pars_init <- fit_initial_pars(empROC, MDE_info)

  L2_init <- L2dist_empROC_parROC(empROC, pars_init, MDE_info, pencon = FALSE)

  est <- try(optim(
    par      = pars_init,
    fn       = L2dist_empROC_parROC,
    empROC   = empROC,
    MDE_info = MDE_info,
    pencon   = TRUE,
    method   = "BFGS",
    control  = list(trace = FALSE, maxit = maxit)))

  pars_fit <- est$par
  L2_fit <- L2dist_empROC_parROC(empROC, pars_fit, MDE_info, pencon = FALSE)

  res <- list(
    pars_init = pars_init,
    L2_init   = L2_init,
    pars_fit  = pars_fit,
    L2_fit    = L2_fit
  )

  return(res)
}

#' Fits initial parameters for the minimum distance estimation of an empirical
#' ROC curve
#'
#' @inheritParams fit_MDE
#' @return
#' @details TBD
fit_initial_pars <- function(empROC, MDE_info){

  MDEm <- MDE_info$method
  MDEi <- MDE_info$info
  if(!is.data.frame(empROC)) empROC <- as.data.frame(empROC)
  empROC <- fill_empROC(empROC)
  gamma <- fit_ipar_gamma(empROC)
  delta <- fit_ipar_delta(empROC)

  if("bin2p" %in% MDEm & MDEi == "unrestricted") pars <- c(1,1)
  if("bin2p" %in% MDEm & MDEi == "concave") pars <- 1
  if("bin3p" %in% MDEm & MDEi == "unrestricted") pars <- c(1,1,gamma)
  if("bin3p" %in% MDEm & MDEi == "concave") pars <- c(1,gamma)

  if("beta2p" %in% MDEm) pars <- c(1,1)
  if("beta2p_v" %in% MDEm) pars <- c(1,1,gamma)
  if("beta2p_h" %in% MDEm) pars <- c(1,1,delta)
  if("beta4p" %in% MDEm) pars <- c(1,1,gamma,delta)

  if("bin2p" %in% MDEm & MDEi == "concave"){
    est <- try(optim(
      par      = pars,
      fn       = roc_sqe,
      empROC   = empROC,
      MDE_info = MDE_info,
      method   = ifelse("bin2p" %in% MDEm & MDEi == "concave", "Brent", "BFGS"),
      lower    = 0,
      upper    = 10,
      control  = list(trace = TRUE)
    ))
  }else{
    est <- try(optim(
      par      = pars,
      fn       = roc_sqe,
      empROC   = empROC,
      MDE_info = MDE_info,
      method   = "BFGS"
    ))
  }

  if("bin2p" %in% MDEm & MDEi == "concave"){
    pars <- c(est$par, 1)
  }else{
    pars <- est$par
  }

  if(any(grepl("beta", MDEm)) & MDEi == "concave")
    pars <- shift_ipar_beta(pars, MDE_info)

  return(pars)
}

#' Squared error for fit of empirical by parametric ROC curve
#' @param pars Parameter vector corresponding to \code{MDE_info$method}
#' @inheritParams fit_initial_pars
#' @details Computes the sum of the squared errors between an empirical ROC
#'   curve and a parametric one specified by pars and MDE_info, evaluated at the
#'   FPR values of the empirical ROC curve. In case a concave binormal ROC curve
#'   is specified, the parameters have to be adjusted in that sigma = 1 is
#'   inserted into the pars vector.
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

#' @inheritParams fit_initial_pars
fit_ipar_gamma <- function(empROC){
  if(sum(empROC$FPR == 0) > 0){
    max(empROC$TPR[empROC$FPR == 0])
  }else{
    0
  }
}

#' @inheritParams fit_initial_pars
fit_ipar_delta <- function(empROC){
  if(sum(empROC$TPR == 1) > 0){
    min(empROC$FPR[empROC$TPR == 1])
  }else{
    1
  }
}


#' Shifts beta parameters into the triangular region corresponding to concave
#' beta ROC curves
#'
#' @param eps optional; specifies how far into the triangular parameter region
#'   corresponding to a concave beta ROC curve the beta parameters are shifted
#'   (see details).
#' @inheritParams fit_initial_pars
#' @details WRITE ABOUT SHIFTING ETC. A value of 0 corresponds to a shift
#'   exactly on the edge of the triangular region
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

#' Only for test purposes
#'
#' @inheritParams shift_ipar_beta
#' @note Visualizes the shift in initial pars in case of concave beta fits
visualize_ipar_beta <- function(MDE_info, eps){

  if(missing(MDE_info))
    MDE_info <- list(method = "beta2p", info = "concave")
  if(missing(eps)) eps <- 0.1
  alphac   <- seq(0, 2, by = 0.01)
  betac    <- seq(0, 3, by = 0.01)

  alphaf <- rep(alphac, each = length(betac))
  betaf  <- rep(betac, length(alphac))
  par_tib <- tibble(alpha = alphaf, beta = betaf)
  conc    <- apply(par_tib, 1, check_concavity, family = "beta")
  dist    <- apply(par_tib, 1, get_dist_to_beta_concave_region)
  dist[dist == 0] <- NA
  par_tib <- par_tib %>% tibble::add_column(dist = dist)


  s_x <- 1-eps/2.41
  s_y <- sepline(s_x)
  dat1 <- tibble(alpha = seq(0, s_x, length.out = 100)) %>%
    mutate(beta = sepline(alpha)) %>%
    filter(beta <= max(betac))
  dat2 <- tibble(alpha = c(s_x, max(alphac)), beta = 1+eps)
  dat3 <- tibble(alpha = seq(0, s_x, length.out = 100)) %>%
    mutate(beta = s_y-s_x+alpha) %>%
    filter(beta >= 0)

  pars_check <- par_tib[sample(1:nrow(par_tib), size = 100), ] %>%
    select(alpha, beta)
  pars_shift <- as.tibble(t(apply(pars_check, 1, function(x)
    shift_ipar_beta(pars = x, MDE_info = MDE_info, eps = eps))))
  pars_check <- pars_check %>% tibble::add_column(group = 1:nrow(pars_check))
  pars_shift <- pars_shift %>% tibble::add_column(group = 1:nrow(pars_shift))
  pars_vis   <- bind_rows(pars_check, pars_shift) %>%
    arrange(group) %>% mutate(group = as.factor(group))

  colors <- rep(RColorBrewer::brewer.pal(n = 9, "Set1"),
                each = ceiling(max(as.numeric(pars_vis$group)) / 9))

  p0 <- ggplot() +
    geom_raster(data = par_tib, aes(x = alpha, y = beta, fill = dist)) +
    coord_fixed()

  p1 <- p0 +
    geom_line(dat = pars_vis,
              aes(x = alpha, y = beta, col = group, group = group)) +
    scale_color_manual(values = colors, guide = FALSE)

  p2 <- p1 +
    geom_line(dat = dat1, aes(x = alpha, y = beta)) +
    geom_line(dat = dat2, aes(x = alpha, y = beta)) +
    geom_line(dat = dat3, aes(x = alpha, y = beta))

  print(p2)
}



#'
#'
#'
#'
#'@details
#'Case I
#'2 Abfrageschritte
#'s1 <= m -> out
#'(s1-m)*diff(x) < d1 -> out
#'ansonsten 2 Loesungen und Split-Mechanismus anwenden
#'3 Abfrageschritte
#'a convex -> out
#'b Steigung Beta in rechtem Endpunkt groesser gleich m -> out
#'c Differenz der Steigungen mal x-Differenz kleiner y-Differenz -> out
#'-> ansonsten 2 Loesungen und Split-Mechanismus anwenden
check_consider <- function(x, MDE_info){

  expectnames <- c("FPR0", "TPR0", "FPR1", "TPR1",
                   "m", "b", "s1", "s2", "d1", "d2")
  if(any(!(names(x) == expectnames))) stop("x is different than expected")
  # if(any(!(c("case", "s1", "m", "FPR1", "FPR0", "d1")) %in% names(x))){
  #   stop("Parameters in \x\ are missing")
  # }

  if(grepl("bin", MDE_info$method)){
    con <- x %>% add_column(consider = TRUE)
  }else{
    cat("USE case_when and check simplification as well as correctness")

    concave <- (MDE_info$info == "concave")
    con <- x %>% add_column(consider = NA) %>%
      mutate(consider = ifelse(d1 > 0 & d2 < 0, TRUE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 > 0, TRUE, consider)) %>%

      mutate(consider = ifelse(d1 < 0 & d2 < 0 & m == 0 & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 < 0 & s1 <= m & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 < 0 & (s1-m)*(FPR1-FPR0) < d1 & concave, FALSE, consider)) %>%

      mutate(consider = ifelse(d1 > 0 & d2 > 0 & m == 0, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 > 0 & d2 > 0 & s2 <= m & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 > 0 & d2 > 0 & (s2-m)*(FPR1-FPR0) < d2 & concave, FALSE, consider))

    # con <- x %>% add_column(consider = NA) %>%
    #   mutate(consider = )
    #   mutate(consider = ifelse(!(d1 < 0 & d2 < 0), consider,
    #                            ifelse(s1 <= m, FALSE,
    #                                   ifelse((s1-m)*FPR1-FPR0) < d1, "no", TRUE))) %>%
    #   mutate(consider = ifelse(!(d1 >= 0 & d2 >= 0), consider,
    #                            ifelse(s2 <= m, FALSE,
    #                                   ifelse((s2-m)*(FPR1-FPR) < d2, "no", TRUE)))) %>%
    #   mutate(consider = ifelse(is.na(consider), TRUE, FALSE))
  }
  return(con)
}

#' Increases empirical ROC curve by intersections with parametric ROC curve
#'
#' @param pars Vector of parameters as specified by \code{MDE_info}
#' @inheritParams fit_MDE
#'
#'@details
roc_intersection <- function(empROC, pars, MDE_info){

  empROCsect <- empROC %>%
    interval_roc %>%
    add_slope_empROC %>%
    add_slope_parROC(pars, MDE_info) %>%
    add_diff_empROC_parROC(pars, MDE_info) %>%
    dplyr::mutate(case = (d1 >= 0)*2 + (d2 >= 0) + 1) %>%
    dplyr::mutate(case = ifelse(FPR0 > 0 & FPR1 < 1, case, 1)) %>%
    check_consider(MDE_info)

  empROCsect_14 <- empROCsect %>% filter(case %in% c(1,4))
  if(nrow(empROCsect_14) == 0){
    intersect_14 <- 0
  }else{
    intersect_14 <- test_for_zeroes(empROCsect_14, pars, MDE_info)
  }

  empROCsect_23 <- empROCsect %>% filter(case %in% c(2,3))
  if(nrow(empROCsect_23) == 0){
    intersect_23 <- 0
  }else{
    intersect_23 <- get_zeroes(empROCsect_23, pars, MDE_info)
  }

  intersect <- combine_intersect(intersect_14, intersect_23)

  empROCsect <- add_zeroes(empROCsect, intersect, pars, MDE_info)

  return(empROCsect)
}


combine_intersect <- function(intersect_14, intersect_23){
  intersect <- unique(sort(c(intersect_14, intersect_23)))
  intersect <- intersect[!is.na(intersect)]
  intersect <- intersect[intersect > 0 & intersect < 1]
  return(intersect)
}

#'
#'
#'
#'
#'@details
#'Case I
#'2 Abfrageschritte
#'s1 <= m -> out
#'(s1-m)*diff(x) < d1 -> out
#'ansonsten 2 Loesungen und Split-Mechanismus anwenden
#'3 Abfrageschritte
#'a convex -> out
#'b Steigung Beta in rechtem Endpunkt groesser gleich m -> out
#'c Differenz der Steigungen mal x-Differenz kleiner y-Differenz -> out
#'-> ansonsten 2 Loesungen und Split-Mechanismus anwenden
check_consider <- function(x, MDE_info){

  expectnames <- c("FPR0", "TPR0", "FPR1", "TPR1",
                   "m", "b", "s1", "s2", "d1", "d2", "case")
  if(any(!(names(x) == expectnames))) stop("x is different than expected")

  if(any(grepl("bin", MDE_info$method))){
    con <- x %>% tibble::add_column(consider = TRUE)
  }else{
    concave <- (MDE_info$info == "concave")
    con <- x %>% tibble::add_column(consider = NA) %>%
      mutate(consider = ifelse(d1 > 0 & d2 < 0, TRUE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 > 0, TRUE, consider)) %>%

      mutate(consider = ifelse(d1 < 0 & d2 < 0 & m == 0 & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 < 0 & s1 <= m & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 < 0 & d2 < 0 & (s1-m)*(FPR1-FPR0) < d1 & concave, FALSE, consider)) %>%

      mutate(consider = ifelse(d1 > 0 & d2 > 0 & m == 0, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 > 0 & d2 > 0 & s2 <= m & concave, FALSE, consider)) %>%
      mutate(consider = ifelse(d1 > 0 & d2 > 0 & (s2-m)*(FPR1-FPR0) < d2 & concave, FALSE, consider))
  }
  return(con)
}

#' Extends empirical ROC curve points by intersection with parametric ROC curve
#'
#' @param x
#' @param intersect
#' @inheritParams roc_intersection
add_zeroes <- function(x, intersect, pars, MDE_info){

  save(x, intersect, pars, MDE_info,
      file = "tests/testdata.Rdata")

  if(length(intersect) == 0) return(x)

  x_add <- tibble()
  r_del <- NULL

  for(i in 1:length(intersect)){

    ind <- max(which(x$FPR0 <= intersect[i]))
    if(x$FPR0[ind] == intersect[i]) next

    r_del <- c(r_del, ind)
    TPRint <- x$m[ind] * intersect[i] + x$b[ind]
    diffint <- diff_empROC_parROC(intersect[i], x$m[ind], x$b[ind], pars, MDE_info)
    caseint <- 0
    sint <- get_slope(intersect[i], pars, MDE_info)

    x_r1 <- x[ind,]
    x_r1[, c(3,4,8,9,11)] <- c(intersect[i], TPRint, diffint, caseint, sint)

    x_r2 <- x[ind,]
    x_r2[, c(1,2,7,9,10)] <- c(intersect[i], TPRint, diffint, caseint, sint)

    x_add <- x_add %>% bind_rows(x_r1, x_r2)
  }

  if(is.null(r_del)){
    x <- x %>% bind_rows(x_add) %>% arrange(FPR0, TPR0) %>% distinct()
  }else{
    x <- x[-r_del, ] %>% bind_rows(x_add) %>% arrange(FPR0, TPR0) %>% distinct()
  }

  # ind0     <- abs(TFPR_use$FPR1) < 1e-06
  # ind1     <- abs(TFPR_use$FPR - 1) < 1e-06
  # ind      <- !(ind0 | ind1)
  # TFPR_use <- TFPR_use[ind, ]
  return(x)
}


#' Computes the intersections of empirical and parametric ROC curves
#'
#' @param x
#' @param intersect
#' @inheritParams roc_intersection
get_zeroes <- function(x, pars, MDE_info){

    intersect <- NULL

    for(i in 1:nrow(x)){
      addzero <- uniroot(
        f = function(y) diff_empROC_parROC(y, x$m[i], x$b[i], pars, MDE_info),
        interval = c(x$FPR0[i], x$FPR1[i]))$root
      intersect <- c(intersect, addzero)
      rm(addzero)
    }

    return(intersect)
}

#' Tests for intersections of empirical and parametric ROC curves
#'
#' @param x
#' @param intersect
#' @inheritParams roc_intersection
test_for_zeroes <- function(x, pars, MDE_info){

    intersect <- NULL

    for(i in 1:nrow(x)){
      FPRtest <- seq(x$FPR0[i], x$FPR1[i], length.out = 300)
      zerotest <- diff_empROC_parROC(FPR = FPRtest, m = x$m[i], b = x$b[i],
                                     pars = pars, MDE_info = MDE_info)

      if(length(unique((sign(zerotest)))) == 1){
        intersect <- c(intersect, NA)
      }else{
        s0 <- sign(zerotest[1])
        while(TRUE){
          inds <- min(which(sign(zerotest) != s0))
          indl <- FPRtest[inds-1]
          indr <- FPRtest[inds]
          intersect <- c(intersect, uniroot(
            f = function(y) diff_empROC_parROC(y, x$m[i], x$b[i],
                                               pars, MDE_info),
            interval = c(indl, indr))$root)
          zerotest <- zerotest[-(1:(inds-1))]
          FPRtest <- FPRtest[-(1:(inds-1))]
          s0 <- sign(zerotest[1])

          if(length(unique((sign(zerotest)))) == 1) break
        }
      }
    }

    return(intersect)
}






#' L2 distance between empirical and parametric ROC curve
#'
#' @param pencon specifies if the L2 distance in case of ROC curves that shall
#'   be concave, but violate the concavity constraint, shall be penalized.
#'   This option is essential for concave MDE estimation (see ...s)
#' @inheritParams roc_intersection
L2dist_empROC_parROC <- function(empROC, pars, MDE_info, pencon){

  if(any(pars[1:2] <= 0)) return(1)
  if(length(pars) >= 3 & (pars[3] < 0  | pars[3] > 1)) return(1)
  if(length(pars) >= 4 & (pars[4] < 0  | pars[4] > 1)) return(1)

  allowednames <- c("FPR0", "TPR0", "FPR1", "TPR1", "m", "b")
  empROCsect <- roc_intersection(empROC, pars, MDE_info)
  if(any(!(names(empROCsect)[1:6] == allowednames))) stop("wrong names der. obj.")
  L2_dist_vec <- apply(empROCsect, 1, function(x)
    integratediff(x, pars = pars, MDE_info = MDE_info))
  L2_dist <- L2dist_concave(sqrt(sum(L2_dist_vec)), pars, MDE_info, pencon)
  return(L2_dist)
}



#' Integrate squared difference between empirical and parametric ROC curve
#'
#' @param x ...
#' @inheritParams roc_intersection
integratediff <- function(x, pars, MDE_info){

  if(any(pars[1:2] <= 0)) return(1)
  if(length(pars) >= 3 & (pars[3] < 0  | pars[3] > 1)) return(1)
  if(length(pars) >= 4 & (pars[4] < 0  | pars[4] > 1)) return(1)

  diffint2 <- function(y) diff_empROC_parROC(y, x[5], x[6], pars, MDE_info)^2
  intres  <- integrate(f = diffint2, lower = x[1], upper = x[3])
  return(intres$value)
}

#' ...
#'
#' @param x ...
#' @inheritParams roc_intersection
L2dist_concave <- function(x, pars, MDE_info, pencon){
  if(pencon){
    corfac <- get_correctionfactor(pars, MDE_info)
    corincr <- get_correctionincrement(pars, MDE_info)
    return(x * corfac + corincr)
  }else{
    return(x)
  }
}

#' Correction factor for concavity constraint violation for concave MDE fits
#'
#' @inheritParams L2dist_empROC_parROC
get_correctionfactor <- function(pars, MDE_info){
  MDEm <- MDE_info$method
  MDEi <- MDE_info$info
  if(MDEi == "unrestricted") return(1)
  if(MDEi == "concave"){
    if(any(grepl("bin", MDEm))) corfac <- 1 + 10^2*(pars[2]-1)^2
    if(any(grepl("beta", MDEm))){
      corfac <- 1 + 10^2 * get_distance_to_beta_concave_region(pars)
    }
  }
  return(corfac)
}

#' Correction increment for concavity constraint violation for concave MDE fits
#'
#' @inheritParams L2dist_empROC_parROC
get_correctionincrement <- function(pars, MDE_info){
  MDEm <- MDE_info$method
  MDEi <- MDE_info$info
  if(MDEi == "unrestricted") return(0)
  if(MDEi == "concave"){
    if(MDEm[1] == "bin2p") corincr <- (pars[2] != 1)
    if(MDEm[1] == "bin3p") corincr <- (pars2 != 1 | pars[3] < 0 | pars[3] > 1)
    if(any(grepl("beta", MDEm))){
      alpha <- (pars[1] < 0 | pars[1] > 1)
      beta  <- (sum(pars[1:2]) < 2 | pars[2] < 0)

      if(MDEm[1] == "beta2p") corincr <- max(alpha, beta)
      if(any(grepl("beta3p", MDEm))){
        third <- (pars[3] < 0 | pars[3] > 1)
        corincr <- max(alpha, beta, third)
      }
      if("MDEm"[1] == "beta4p"){
        gamma <- (pars[3] < 0 | pars[3] > 1)
        delta <- (pars[4] < 0 | pars[4] > 1)
        corincr <- max(alpha, beta, gamma, delta)
      }
    }
  }
  return(corincr)
}

#' Minimum euclidean distance from current beta parameters to concave beta parameters
#'
#' @inheritParams L2dist_empROC_parROC
get_distance_to_beta_concave_region <- function(pars){
  alpha <- pars[1]
  beta  <- pars[2]
  if(alpha > 1  & beta >= 1) dist2 <- abs(alpha-1)
  if(alpha >= 1 & beta < 1 ) dist2 <- sqrt((alpha-1)^2 + (beta-1)^2)
  if(alpha >= beta & alpha < 1 & beta < 1) dist2 <- sqrt((alpha-1)^2 + (beta-1)^2)
  if(alpha < beta & beta+alpha < 2 & alpha < 1) dist2 <- sqrt(2*((2-alpha-beta)/2)^2)
  if(!exists("dist2")) dist2 <- 0
  return(dist2)
}
