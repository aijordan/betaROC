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

  mf <- mf[order(mf$forc), ]

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

MDEfit <- function(x, MDE_info){
  "a"
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
  ifelse(FPR >= pars[3], 1, get_TPR_beta2p(FPR/pars[3], pars[-3]))
}
get_TPR_bin2p <- function(FPR, pars){
  pnorm(pars[1] + pars[2] * qnorm(FPR))
}
get_TPR_bin3p <- function(FPR, pars){
  par[3] + (1-par[3]) * get_TPR_bin2p(FPR, pars[1:2])
}
