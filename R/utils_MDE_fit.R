#' Fits initial parameters for the minimum distance estimation of an empirical
#' ROC curve
#'
#' @param empROC \code{data.frame} with true positive (TPR) and false positive
#'   (FPR) values of the empirical ROC curve
#' @param MDE_info \code{list} containing information about the required minimum
#'   distance estimation (MDE) fit
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






