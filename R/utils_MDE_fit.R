#' Minimum distance fit for empirical ROC curve
#'
#' @param empROC \code{data.frame} with true positive (TPR) and false positive
#'   (FPR) values of the empirical ROC curve
#' @param MDE_info \code{list} containing information about the required minimum
#'   distance estimation (MDE) fit
#'
#' @return
#' @details
fit_MDE <- function(empROC, MDE_info){

  if(missing(empROC)) stop("no empirical ROC curve provided")
  if(missing(MDE_info)) stop("no information about MDE fit provided")

  pars <- fit_initial_pars(empROC, MDE_info)



}

MDEfit <- function() "a"


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

  if(MDEm == "bin2p" & MDEi == "unrestricted") pars <- c(1,1)
  if(MDEm == "bin2p" & MDEi == "concave") pars <- 1
  if(MDEm == "bin3p" & MDEi == "unrestricted") pars <- c(1,1,gamma)
  if(MDEm == "bin3p" & MDEi == "concave") pars <- c(1,gamma)

  if(MDEm == "beta2p") pars <- c(1,1)
  if(MDEm == "beta3p_v") pars <- c(1,1,gamma)
  if(MDEm == "beta3p_h") pars <- c(1,1,delta)
  if(MDEm == "beta4p") pars <- c(1,1,gamma,delta)

  est <- try(optim(
    par      = pars,
    fn       = roc_sqe,
    empROC   = empROC,
    MDE_info = MDE_info
  ))
  pars <- est$par

  if(grepl("beta", MDEm) & MDEi == "concave")
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
  if(grepl("bin", MDEm) & MDEi == "concave") pars <- c(pars[1], 1, pars[-1])

  TPR <- get_TPR(empROC$FPR, pars, MDE_info)
  sqe <- sum((TPR - empROC$TPR)^2)
  return(sqe)
}

#' @inheritParams fit_initial_pars
fit_ipar_gamma <- function(empROC) max(empROC$TPR[empROC$FPR == 0])

#' @inheritParams fit_initial_pars
fit_ipar_delta <- function(empROC) min(empROC$FPR[empROC$TPR == 1])

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

  if(grepl("beta", MDE_info$method) & MDE_info$info == "concave"){

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
  par_tib <- par_tib %>% add_column(dist = dist)


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
  pars_check <- pars_check %>% add_column(group = 1:nrow(pars_check))
  pars_shift <- pars_shift %>% add_column(group = 1:nrow(pars_shift))
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

roc_intersection <- function(empROC, pars, MDE_info){

  empROCsect <- empROC %>%
    interval_roc %>%
    add_slope_empROC %>%
    add_slope_parROC(pars, MDE_info) %>%
    add_diff_empROC_parROC(pars, MDE_info) %>%
    check_consider(MDE_info)

  # Fälle 1-4
  # Fälle 2-3
  # Alle Fälle direkt mit schachtel Verfahren
  # -> uniroot



}








  # Erst Splitten fuer Faelle 1 & 4
  if(TRUE){

    TFPR_split <- TFPR_treat %>%
      filter(case == 1 | case == 4) %>%
      select(FPR, FPR1, m, b, case, everything())

    # Gibt es ueberhaupt zu behandelnde Faelle fuer cases 1 und 4
    if(nrow(TFPR_split) == 0) intersect_14_return <- 0

    if(nrow(TFPR_split) > 0){

      if(debug) print("")
      if(debug) print("Calling splitbeta()")

      intersect_14_help <- apply(as.matrix(TFPR_split[, 1:5]), 1, splitbeta, param = param, debug = debug)

      if(debug) print("")
      if(debug) print(intersect_14_help)

      # Als Vektor
      if(is.list(intersect_14_help)){
        intersect_14 <- do.call(c, intersect_14_help)
      }else{
        if(is.vector(intersect_14_help)){
          intersect_14 <- intersect_14_help
        }else{
          if(is.matrix(intersect_14_help)){
            intersect_14 <- as.vector(intersect_14_help)
          }else{
            print("STILL SOMETHING DIFFERENT")
          }
        }
      }

      if(debug) print("")
      if(debug) print(intersect_14)

      # Ohne NA's
      intersect_14_return <- if(all(is.na(intersect_14))){
        0
      }else{
        unique(intersect_14[!is.na(intersect_14)])
      }

      if(debug) print("")
      if(debug) print(intersect_14_return)
    }
  }

  # Schachtel Algorithmus anwenden für 2 und 3
  if(TRUE){

    TFPR_sch <- TFPR_treat %>%
      filter(case == 2 | case == 3) %>%
      select(FPR, FPR1, m, b)

    if(debug) print("")
    if(debug) print("Calling schachtel()")

    intersect_23 <- apply(as.matrix(TFPR_sch), 1, schachtel, diff_func = diffbl_debug,
                          param = param, parameterization = "beta", debug = debug)

    if(debug) print(str(intersect_23))
  }

  # Intersection zusammenstellen
  intersect    <- unique(sort(c(intersect_14_return, intersect_23)))
  if(debug) save(intersect, file = "Debug/intersect_pre.Rdata")

  # Alle Stellen zu nahe an 0 und 1 rauswerfen
  ind       <- (abs(intersect - 1) <= 1e-04) | (abs(intersect) <= 1e-04)
  intersect <- intersect[!ind]
  if(debug) save(intersect, file = "Debug/intersect_post.Rdata")

  if(FALSE){
    p <- plot_beta(param = param, p = plot_TFPR(TFPR_raw, add_auc = FALSE))
    p <- p + geom_vline(xintercept = intersect)
    p
  }

  if(debug) print("")
  if(debug) print("intersect:")
  if(debug) print(intersect)

  # Gibt es Stellen zum Unterteilen -> Falls nein, direkt Rueckgabe
  if(length(intersect) == 0){
    if(debug) print("No splitpoint found")
    return(TFPR_use)
  }

  # Gibt es Stellen zum Unterteilen -> Berechnung der Werte an den Punkten
  if(length(intersect) != 0){

    # TFPR_use + um Schnittpunkte erweitern
    tib_add <- tibble()
    r_del   <- NULL

    if(debug) print("")
    if(debug) print("Iteratively finding an deleting duplicates and adding intersection values")
    if(debug) print("Calling diffbl() and slopebeta()")
    if(debug) print("")

    for(i in 1:length(intersect)){

      # Welche Zeile gehoert dazu
      ind <- max(which(TFPR_use$FPR <= intersect[i]))
      # ind <- which(abs(TFPR_use$FPR - intersect[i]) == min(abs(TFPR_use$FPR - intersect[i])))

      # Bei Uebereinstimmung ueberspringen
      if(TFPR_use$FPR[ind] == intersect[i]) next

      # Bei Nichtuebereinstimmung manipulieren
      if(TFPR_use$FPR[ind] != intersect[i]){

        r_del <- c(r_del, ind)

        dat_man <- TFPR_use[ind, ]

        TPR_mean  <- dat_man$m * intersect[i] + dat_man$b

        d_mean    <- diffbl(x = intersect[i], m = dat_man$m, b = dat_man$b, param = param)
        case_mean <- 0

        s_mean    <- slopebeta(x = intersect[i], param = param, debug = debug)
        concave   <- dat_man$concave

        dat_r1    <- dat_man
        dat_r1[, c(3,4,8,9,11)] <- c(intersect[i], TPR_mean, d_mean, case_mean, s_mean)

        dat_r2    <- dat_man
        dat_r2[, c(1,2,7,9,10)] <- c(intersect[i], TPR_mean, d_mean, case_mean, s_mean)

        tib_add <- tib_add %>% bind_rows(dat_r1, dat_r2)
      }
    }

    if(is.null(r_del)){
      TFPR_use <- TFPR_use %>%
        bind_rows(tib_add) %>%
        arrange(FPR, TPR) %>%
        distinct()
    }

    if(!is.null(r_del)){
      TFPR_use <- TFPR_use[-r_del, ] %>%
        bind_rows(tib_add) %>%
        arrange(FPR, TPR) %>%
        distinct()
    }

    if(FALSE) TFPR_use %>% select(FPR, FPR1, TPR, TPR1, everything())
  }

  # TFPR_use von
  if(debug) print("Clean TFPR_use")
  ind0     <- abs(TFPR_use$FPR1) < 1e-06
  ind1     <- abs(TFPR_use$FPR - 1) < 1e-06
  ind      <- !(ind0 | ind1)
  TFPR_use <- TFPR_use[ind, ]

  return(TFPR_use)
}



