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
roc_empirical <- function(mf, emp_info) {
  mf <- mf[order(mf$forc), ]
  if (emp_info == "concave") {
    mf$forc <- with(stats::isoreg(mf$forc, mf$obs), yf)
  }
  counts <- c(0, 0)
  names(counts) <- c("FP", "TP")
  empROC <- NULL
  fprev <- -Inf
  for (i in rev(seq_len(nrow(mf)))) {
    if (mf$forc[i] != fprev) {
      empROC <- rbind(empROC, counts)
      fprev  <- mf$forc[i]
    }
    if (mf$obs[i] == 1) {
      counts["TP"] <- counts["TP"] + 1
    } else {
      counts["FP"] <- counts["FP"] + 1
    }
  }
  empROC <- rbind(empROC, counts)
  empROC[, 1] <- empROC[, 1] / empROC[nrow(empROC), 1]
  empROC[, 2] <- empROC[, 2] / empROC[nrow(empROC), 2]
  dimnames(empROC) <- list(NULL, c("FPR", "TPR"))
  as.data.frame(empROC)
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
      dplyr::group_by(empROC, .data$FPR),
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
