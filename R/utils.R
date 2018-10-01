#' Checks the correctness of model.frame inputs
#'
#' @param mf Model frame
#' @return Returns a model frame
#' @details Ensures that observations are binary and no infinite values are
#'   present in the forecasts
check_model.frame <- function(mf){

  if(!all(mf$obs %in% c(0,1))) stop("observation is not binary")
  if(all(is.infinite(mf$forc))) stop("forecasts are all infinite")
  if(any(lapply(mf, class) %in% c("character", "factor")))
    stop("observation or forecast can not be converted to numeric")

  if(any(is.infinite(mf$forc))) mf <- mf[!is.infinite(mf$forc), ]
  return(mf)
}

#' Fill ROC curve by linear interpolation
#'
#' @param empROC \code{data.frame} containing true positive (TPR) and false
#'   positive rates (FPR), ordered by FPR, then TPR
#' @param spacing optional; defines approximately the spacing at which
#'   interpolated values are computed
#'
#' @note Check sorting by FPR, then TPR
#' @return \code{data.frame} empROC, containing FPR and TPR values, ordered by
#'   FPR, then TPR
#'
fill_empROC <- function(empROC, spacing = 0.05){

  if(max(diff(empROC$FPR)) <= spacing) return(empROC)

  FPRadd <- seq(0, 1, length.out = ceiling(1/spacing) + 1)
  FPRadd <- FPRadd[!(FPRadd %in% empROC$FPR)]
  TPRadd <- as.data.frame(
    do.call(rbind, lapply(FPRadd, function(x) linint_empROC(x, empROC))))
  empROC <- rbind(empROC, TPRadd)
  empROC <- empROC[order(empROC$FPR),]

  return(empROC)
}

#' Linear interpolation of a ROC curve
#'
#' @param FPR False positive rate for which linear interpolation is required
#' @param empROC \code{data.frame} containing true positive (TPR) and false
#'   positive rates (FPR), ordered by FPR, then TPR
#'
#' @note Check sorting by FPR, then TPR
#' @return Vector of FPR and corresponding TPR value
#'
linint_empROC <- function(FPR, empROC){
  FPRdiff <- empROC$FPR - FPR
  ind <- c(max(which(FPRdiff < 0)), min(which(FPRdiff > 0)))
  alpha <- (FPR-empROC$FPR[ind[1]])/abs(diff(empROC$FPR[ind]))
  TPR <- alpha * empROC$TPR[ind[2]] + (1-alpha) * empROC$TPR[ind[1]]
  return(c("FPR" = FPR, "TPR" = TPR))
}

linslope  <- function(y2, y1, x2, x1) as.numeric((y2-y1)/(x2-x1))
linint    <- function(y, x, m) as.numeric(y - x * m)

#' Removes the attribute terms from a model.frame
#'
#' @param mf Model frame
#' @return Returns a data.frame
rm_attribute_terms <- function(mf){
  if(!is.null(attributes(mf)$terms)) attr(mf, "terms") <- NULL
  return(mf)
}

#' Checks pairedness of two ROC curves
#'
#' @inheritParams roc.test
#' @details Evaluates based on the observations used for creating the two
#'   \code{roc} curves if they are paired. This is based on the observation
#'   vector and therefore sensitive to reordering of the forecast-observation
#'   pairs used in constructing the \code{roc} curves.
check_paired <- function(roc1, roc2){

  if(missing(roc1) | missing(roc2))
    stop("two roc curves are to be provided for pairedness check")

  obs1 <- roc1$model.frame$obs
  obs2 <- roc2$model.frame$obs

  if(length(obs1) != length(obs2)){
    return(FALSE)
  }else{
    if(any(!(obs1 == obs2))){
      return(FALSE)
    }else{
      return(TRUE)
    }
  }
}



