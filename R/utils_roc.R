# Computes the empirical ROC curve values
#
# @param mf Model frame as provide by \code{roc}
# @param emp_info Information if unrestricted or concave empirical ROC curve
#   shall be returned
#
# @return Returns a data.frame with true positive rates (TPR) and false
#   positive rates (FPR)
#
# @details \code{roc_empirical} computes the true positive (TPR) and
#   corresponding false positive rates (FPR) for the empirical data provided in
#   \code{mf}. For faster computations, the evaluation utilizes that only at
#   the distinct forecast values, changes of FPR or TPR occur.
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

# ...
#
# @param x ...
#
# @return TBD
# @details TBD
#
interval_roc <- function(empROC) {
  x <-
    dplyr::summarize(
      dplyr::group_by(empROC, FPR),
      TPR1 = min(.data$TPR),
      TPR0 = max(.data$TPR)
    )
  tibble::tibble(
    FPR0 = x$FPR[-nrow(x)],
    TPR0 = x$TPR0[-nrow(x)],
    FPR1 = x$FPR[-1],
    TPR1 = x$TPR1[-1]
  )
}


# Checks if a ROC curve is concave
#
# @param empROC optional; in case an empirical ROC curve is provided, it is
#   check for concavity
# @param pars optional;
# @param MDE_info optional;
#
# @return Logical vector of length one indicating if the ROC curve provided as
#   empirical or parametric form is concave
# @details TBD
#
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

# Checks if an empirical ROC curve is concave
#
# @param empROC ...
#
# @return Logical vector of length one indicating if the ROC curve provided as
#   empirical or parametric form is concave
# @details TBD
#
check_roc_concave_empirical <- function(empROC){
  stop("TBD")
}

# Checks if a parametric ROC curve is concave
#
# @param pars ...
# @param MDE_info ...
#
# @return Logical vector of length one indicating if the ROC curve provided as
#   empirical or parametric form is concave
# @details TBD
#
check_roc_concave_parametric <- function(par, MDE_info){
  stop("TBD")
}



# Computes the TPR value for given FPR value and model specification
#
# @param FPR optional; false positive rates for which true positive rates (TPR)
#   are required
# @param pars; a vector of parameters
# @param MDE_info; a list with information about the MDE fit
#
# @return Returns a numeric vector of TPR entries
#
get_TPR <- function(FPR, pars, MDE_info){

  if(missing(FPR)) FPR <- seq(0, 1, by = 0.001)
  if(missing(pars)) stop("no parameters")
  if(missing(MDE_info)) stop("no MDE_info")

  func <- get(paste0("get_TPR_", MDE_info$method[1]))
  TPR  <- func(FPR, pars)

  return(TPR)
}

# ...
#
# @param ... ...
#
# @return TBD
# @details TBD
#
get_slope <- function(FPR, pars, MDE_info){
  if(missing(FPR)) stop("no FPR values")
  if(missing(pars)) stop("no parameters")
  if(missing(MDE_info)) stop("no MDE_info")
  func <- get(paste0("get_TPR_", MDE_info$method[1]))
  slope <- func(FPR, pars)
  return(slope)
}



