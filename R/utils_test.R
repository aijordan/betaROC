#' Tests two objects of class \code{roc}
#'
#' @param roc1 Element of class \code{roc}
#' @param roc2 Element of class \code{roc}; necessary for tests of equal AUC or
#'   equal ROC curves
#' @param testname Name of the test to be performed. The choices
#'   goodness-of-fit, auc, and roc are available and partial matching is
#'   supported.
#'
#' @return Returns an object of class \code{roctest}, containing ...
#' @details The function allows to test the goodness-of-fit of one ROC curve as
#'   well as to test for AUC or ROC curve equality.
#'
#'   The goodness-of-fit tests in case for one provided ROC curves are based on
#'   Monte Carlo type approaches to estimate the .. and require comparatively
#'   long computation times.
#'
#'   The tests for AUC and ROC curve inequality are based on the asymptotic
#'   distribution in case of parametric beta and binormal ROC curves and, so
#'   far, only available for the 2-parameter cases.
#'
#'
#'
#' @note GOODNESS-OF-FIT - Parametric - Non-parametric (?) AUC value -
#'   Parametric - Non-parametric (?) - for paired and unpaired data ROC curve -
#'   Parametric - Non-parametric - for paired and unpaired data
#'
roc.test <- function(roc1, roc2, testname, parametric, ...){

  availnames <- c("goodness-of-fit", "auc", "roc")

  if(missing(testname)) testname <- "auc"
  if(misssing(parametric)) parametric <- TRUE

  testname <- match.arg(testname, availnames)

  if(testname == "goodness-of-fit"){
    if(parametric){

      if(missing(roc1)){
        if(missing(roc2)){
          stop("No roc curve provided for goodness-of-fit test")
        }else{
          print("Using provided roc2 as roc1 is missing")
          roc1 <- roc2; rm(roc2)
        }
      }

      test_goodness_of_fit(roc1, ...)
    }else{
      stop("goodness-of-fit test only possible for fitted ROC curves")
    }
  }

  if(missing(roc1) | missing(roc2))
    stop("two roc curves are to be provided for tests of equal AUC or ROC curves")

  is.paired <- check_paired(roc1, roc2)

  if(testname == "auc"){

    if(parametric){
      if(is.paired){
        auc_test_parametric_paired(roc1, roc2, ...)
      }else{
        auc_test_parametric_unpaired(roc1, roc2, ...)
      }
    }else{
      stop("Test Venkatraman")
      auc_test_nonparametric(roc1, roc2, ...)
    }

  }

  if(testname == "roc"){

    if(parametric){
      if(is.paired){
        roc_test_parametric_paired(roc1, roc2, ...)
      }else{
        roc_test_parametric_unpaired(roc1, roc2, ...)
      }
    }else{
      stop("Test Venkatraman & Begg")
      roc_test_nonparametric(roc1, roc2, ...)
    }

  }

}



test_goodness_of_fit <- function(roc, ...){
  return("a")
}

auc_test_parametric_paired <- function(roc1, roc2, ...){
  return("a")
}

auc_test_parametric_unpaired <- function(roc1, roc2, ...){
  return("a")
}

auc_test_nonparametric <- function(roc1, roc2, ...){
  return("a")
}

roc_test_parametric_paired <- function(roc1, roc2, ...){
  return("a")
}

roc_test_parametric_unpaired <- function(roc1, roc2, ...){
  return("a")
}

roc_test_nonparametric <- function(roc1, roc2, ...){
  return("a")
}
