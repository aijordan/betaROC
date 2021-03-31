#' Receiver Operating Characteristic (ROC) object
#'
#' \code{roc} constructs and returns an object of class \code{roc}.
#'
#' @param formula Formula specifying observation and forecast
#' @param data optional; data frame which incorporates the variables named in
#'   \code{formula}. If \code{data} is not specified, \code{roc} searches for
#'   the variables specified in \code{formula} in .GlobalEnv.
#' @param fit_method Method specifies the type of ROC curve to be computed. Partial
#'   matching is supported and the following names are permitted.
#'   \tabular{ll}{
#'   \code{"empirical"} \tab generates only the empirical ROC curve \cr
#'   \code{"bin2p"} \tab generates the classical 2-parameter binormal ROC model \cr
#'   \code{"bin3p"} \tab generates the 3-parameter binormal ROC model \cr
#'   \code{"beta2p"} \tab generate the 2-parameter beta ROC model \cr
#'   \code{"beta3p_v"} \tab generates the 3-parameter beta ROC model with a
#'   vertical straight edge at (0,0) \cr
#'   \code{"beta3p_h"} \tab generates the 3-parameter beta ROC model with a
#'   horizontal straight edge at (1,1) \cr
#'   \code{"beta4p"} \tab generates the 4-parameter beta ROC model
#'   }
#' @param emp_info should the empirical ROC curve be \code{"concave"} or \code{"unrestricted"}.
#' @param fit_info should the parametric ROC curve be \code{"concave"} or \code{"unrestricted"}.
#'
#'
#' @return The output is an object of class \code{roc} which is a list
#'   containing the following components:
#'   \tabular{ll}{
#'   \code{formula} \tab The formula used within the \code{roc} command \cr
#'   \code{model.frame} \tab The model frame constructed from the data \cr
#'   \code{emp_info} \tab Information on the construction of the empirical
#'   ROC curve \cr
#'   \code{empROC} \tab \code{data.frame} containing true positive and false
#'   positive rates of the empirical ROC curve \cr
#'   \code{MDE_info} \tab \code{list} naming the required fits and the
#'   constraint for the MDE fit \cr
#'   \code{MDE_fit} \tab \code{list} with initial and final parameters and
#'   associated L2 distances between empirical and parametric ROC curve
#'   }
#'
#' @details to be added
#'
#' @export
roc <- function(formula, data, emp_info, fit_method, fit_info){

  info_avail <- c("concave", "unrestricted")
  method_avail <- c("empirical", "bin2p", "bin3p", "beta2p", "beta3p_v",
                  "beta3p_h", "beta4p")

  if( missing(data) ) data <- NULL
  if( missing(emp_info) ) emp_info <- "unres"
  if( missing(fit_info) ) fit_info <- "unres"
  if( missing(fit_method)) fit_method <- "emp"

  res <- list()

  res$formula <- formula
  res$data <- data
  res$model.frame <- stats::model.frame(formula, data, na.action = stats::na.omit)
  names(res$model.frame) <- c("obs", "forc")
  res$model.frame <- check_model.frame(res$model.frame)

  res$emp_info <- match.arg(emp_info, info_avail)
  res$empROC <- roc_empirical(res$model.frame, res$emp_info)

  method_s <- match.arg(fit_method, method_avail)
  methods <- if(method_s == "empirical") method_s else c(method_s, "empirical")

  res$MDE_info <- list(
    method = methods,
    info   = match.arg(fit_info, info_avail))
  res$MDEfit <- MDE(res$empROC, res$MDE_info)

  class(res) <- "roc"
  return(res)
}

# print("ASYMPTOTISCHE VERTEILUNG EINFUEGEN")
# print("PLOTS EINFUEGEN")
# print("STATISTICAL TESTS EINFUEGEN")
# print("UNIT TESTS EINFUEGEN")
#
# print("UEBERSICHT TESTS - EXISTIEREND UND FEHLEND - EINFUEGEN")

as.data.frame.roc <- function(x, type){
  availtypes <- c("obsforc", "roc")
  if(missing(type)) type <- "roc"
  type <- match.arg(type, availtypes)
  df <- if(type == "obsforc"){
    as.data.frame(x$model.frame)
  }else{
    as.data.frame(x$empROC)
  }
  return(df)
}
