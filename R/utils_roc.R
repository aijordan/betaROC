#' Computes the empirical ROC curve values
#'
#' @param mf Model frame as provide by \code{roc}
#' @param emp_info Information if unrestricted or concave empirical ROC curve
#'   shall be returned
#'
#' @return Returns a data.frame with true positive rates (TPR) and false
#'   positive rates (FPR)
#'
#' @details \code{roc_empirical} computes the true positive (TPR) and
#'   corresponding false positive rates (FPR) for the empirical data provided in
#'   \code{mf}. For faster computations, the evaluation utilizes that only at
#'   the distinct forecast values, changes of FPR or TPR occur.
roc_empirical <- function(mf, emp_info){

  avail_rates <- c("TPR", "FPR")

  if(emp_info == "concave"){
    if(requireNamespace("isotone", quietly = TRUE)){
      mf$forc <- isotone::gpava(z = mf$forc, y = mf$obs, ties = "secondary")$x
    }else{
      stop("Package \"pkg\" needed for this function to work. Please install it.",
           call. = FALSE)
    }
  }

  mf <- mf[rev(order(mf$forc)), ]

  empROC <- NULL

  pos <- sum(mf$obs)
  neg <- sum(1-mf$obs)

  counts <- c(0, 0)
  names(counts) <- c("FP", "TP")

  i     <- 1
  fprev <- -Inf
  while(i <= nrow(mf)){
    if(mf$forc[i] != fprev){
      empROC <- rbind(empROC, counts)
      fprev  <- mf$forc[i]
    }

    if(mf$obs[i] == 1){
      counts["TP"] <- counts["TP"] + 1
    }else{
      counts["FP"] <- counts["FP"] + 1
    }
    i <- i + 1
  }

  empROC     <- rbind(empROC, c(neg, pos))
  empROC[,1] <- empROC[,1]/neg
  empROC[,2] <- empROC[,2]/pos

  dimnames(empROC) <- list(NULL, sapply(dimnames(empROC)[[2]],
                                        function(x) match.arg(x, avail_rates)))

  empROC <- as.data.frame(unique(empROC))

  return(empROC)
}


#' Computes the TPR value for given FPR value and model specification
#'
#' @param FPR optional; false positive rates for which true positive rates (TPR)
#'   are required
#' @param pars; a vector of parameters
#' @param MDE_info; a list with information about the MDE fit
#'
#' @return Returns a numeric vector of TPR entries
#'
get_TPR <- function(FPR, pars, MDE_info){

  if(missing(FPR)) FPR <- seq(0, 1, by = 0.001)
  if(missing(pars)) stop("no parameters")
  if(missing(MDE_info)) stop("no MDE_info")

  func <- get(paste0("get_TPR_", MDE_info$method[1]))
  TPR  <- func(FPR, pars)

  return(TPR)
}

#' Implementations of get_TPR functions for the different MDE methods
get_TPR_beta2p <- function(FPR, pars){
  pbeta(q = FPR, shape1 = pars[1], shape2 = pars[2])
}
get_TPR_beta3p_v <- function(FPR, pars){
  pars[3] + (1-pars[3]) * get_TPR_beta2p(FPR, pars[1:2])
}
get_TPR_beta3p_h <- function(FPR, pars){
  ifelse(FPR >= pars[3], 1, get_TPR_beta2p(FPR/pars[3], pars[1:2]))
}
get_TPR_beta4p <- function(FPR, pars){
  ifelse(FPR >= pars[4], 1, get_TPR_beta3p_v(FPR/pars[4], pars[-4]))
}
get_TPR_bin2p <- function(FPR, pars){
  pnorm(pars[1] + pars[2] * qnorm(FPR))
}
get_TPR_bin3p <- function(FPR, pars){
  pars[3] + (1-pars[3]) * get_TPR_bin2p(FPR, pars[1:2])
}

#' Checks if a ROC curve is concave
#'
#' @param empROC optional; in case an empirical ROC curve is provided, it is
#'   check for concavity
#' @param pars optional;
#' @param MDE_info optional;
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave <- function(empROC, pars, MDE_info){

  if(missing(empROC) & (missing(pars) | missing(MDE_info))){
    stop("In case of an empiric ROC curve, specify empROC. In case of a
         parametric ROC curve specify pars and MDE_info")
  }else{
    if(missing(empROC)){
      # parametric ROC curve provided
      concave <- check_roc_concave_parametric(pars, MDE_info)
    }else{
      # empROC provided
      concave <- check_roc_concave_empirical(empROC)
    }
  }
  return(concave)
}

#' Checks if an empirical ROC curve is concave
#'
#' @param empROC ...
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave_empirical <- function(empROC){
  stop("TBD")
}

#' Checks if a parametric ROC curve is concave
#'
#' @param pars
#' @param MDE_info
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave_parametric <- function(par, MDE_info){
  stop("TBD")
}

#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
interval_roc <- function(x){
  if(any(!(c("FPR", "TPR") %in% names(x)))) stop("Missing FPR or TPR")
  x <- x %>% select(FPR, TPR)
  empROCl <- x %>% group_by(FPR) %>% arrange(TPR) %>% slice(1) %>% ungroup()
  empROCu <- x %>% group_by(FPR) %>% arrange(desc(TPR)) %>% slice(1) %>% ungroup()
  empROCsect <- bind_cols(empROCu[-nrow(empROCu), ], empROCl[-1, ])
  empROCsect <- empROCsect %>% dplyr::select(FPR0 = FPR, TPR0 = TPR, everything())
  return(empROCsect)
}

#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
add_slope_empROC <- function(x){
  empROCsect <- check_empROCsect(x) %>%
    mutate(m = linslope(y2 = TPR1, y1 = TPR0, x2 = FPR1, x1 = FPR0),
           b = linint(y = TPR1, x = FPR1, m = m))
  return(empROCsect)
}


#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
check_empROCsect <- function(x){
  if(all(c("FPR0", "TPR0", "FPR1", "TPR1") %in% names(x))){
    return(x)
  }else{
    return(interval_roc(x))
  }
}

#' Checks if a ROC curve is concave
#'
#' @param empROC optional; in case an empirical ROC curve is provided, it is
#'   check for concavity
#' @param pars optional;
#' @param MDE_info optional;
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave <- function(empROC, pars, MDE_info){

  if(missing(empROC) & (missing(pars) | missing(MDE_info))){
    stop("In case of an empiric ROC curve, specify empROC. In case of a
         parametric ROC curve specify pars and MDE_info")
  }else{
    if(missing(empROC)){
      # parametric ROC curve provided
      concave <- check_roc_concave_parametric(pars, MDE_info)
    }else{
      # empROC provided
      concave <- check_roc_concave_empirical(empROC)
    }
  }
  return(concave)
}

#' Checks if an empirical ROC curve is concave
#'
#' @param empROC ...
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave_empirical <- function(empROC){
  stop("TBD")
}

#' Checks if a parametric ROC curve is concave
#'
#' @param pars
#' @param MDE_info
#'
#' @return Logical vector of length one indicating if the ROC curve provided as
#'   empirical or parametric form is concave
#' @details TBD
#'
check_roc_concave_parametric <- function(par, MDE_info){
  stop("TBD")
}

#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
interval_roc <- function(x){
  if(any(!(c("FPR", "TPR") %in% names(x)))) stop("Missing FPR or TPR")
  x <- x %>% select(FPR, TPR)
  empROCl <- x %>% group_by(FPR) %>% arrange(TPR) %>% slice(1) %>% ungroup()
  empROCu <- x %>% group_by(FPR) %>% arrange(desc(TPR)) %>% slice(1) %>% ungroup()
  empROCsect <- bind_cols(empROCu[-nrow(empROCu), ], empROCl[-1, ])
  empROCsect <- empROCsect %>% dplyr::select(FPR0 = FPR, TPR0 = TPR, everything())
  return(empROCsect)
}

#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
add_slope_empROC <- function(x){
  empROCsect <- check_empROCsect(x) %>%
    mutate(m = linslope(y2 = TPR1, y1 = TPR0, x2 = FPR1, x1 = FPR0),
           b = linint(y = TPR1, x = FPR1, m = m))
  return(empROCsect)
}


#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
check_empROCsect <- function(x){
  if(all(c("FPR0", "TPR0", "FPR1", "TPR1") %in% names(x))){
    return(x)
  }else{
    return(interval_roc(x))
  }
}

#' ...
#'
#' @param x Element of type empROC or empROCsect
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
add_slope_parROC <- function(x, pars, MDE_info){
  empROCsect <- check_empROCsect(x) %>%
    mutate(s1 = get_slope(FPR0, pars = pars, MDE_info = MDE_info),
           s2 = get_slope(FPR1, pars = pars, MDE_info = MDE_info))
  return(empROCsect)
}

#' ...
#'
#' @param ...
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
get_slope <- function(FPR, pars, MDE_info){
  if(missing(FPR)) stop("no FPR values")
  if(missing(pars)) stop("no parameters")
  if(missing(MDE_info)) stop("no MDE_info")
  func <- get(paste0("get_TPR_", MDE_info$method[1]))
  slope <- func(FPR, pars)
  return(slope)
}


#' Implementations of get_TPR functions for the different MDE methods
get_slope_beta2p <- function(FPR, pars){
  dbeta(x = x, shape1 = pars[1], shape2 = pars[2])
}
get_slope_beta3p_v <- function(FPR, pars){
  (1-pars[3]) * get_slope_beta2p(FPR, pars)
}
get_slope_beta3p_h <- function(FPR, pars){
  ifelse(FPR >= pars[3], 0, get_slope_beta2p(FPR/pars[3], pars))
}
get_slope_beta4p <- function(FPR, pars){
  ifelse(FPR >= pars[3], 0, get_slope_beta3p_v(FPR/pars[3], pars[-3]))
}
get_slope_bin2p <- function(FPR, pars){
  dnorm(pars[1] + pars[2]*qnorm(x)) * pars[2] * 1/dnorm(qnorm(x))
}
get_slope_bin3p <- function(FPR, pars){
  (1-pars[3]) * get_slope_bin2p(FPR, pars)
}



#' ...
#'
#' @param x Element of type empROC or empROCsect
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
add_diff_empROC_parROC <- function(x, pars, MDE_info){
  empROCsect <- check_empROCsect(x)
  if(any(!(c("m", "b") %in% names(empROCsect)))){
    empROCsect <- add_slope_empROC(empROCsect)
  }
  empROCsect <- empROCsect %>%
    mutate(d1 = diff_empROC_parROC(FPR0, m, b, pars, MDE_info),
           d2 = diff_empROC_parROC(FPR1, m, b, pars, MDE_info))
  return(empROCsect)
}

#' ...
#'
#' @param x Element of type empROC or empROCsect
#' @param ...
#'
#' @return TBD
#' @details TBD
#'
diff_empROC_parROC <- function(FPR, m, b, pars, MDE_info){
  get_TPR(FPR, pars, MDE_info) - (m * FPR + b)
}




