#' Receiver Operating Characteristic (ROC) object
#'
#' \code{roc} constructs and returns an object of class \code{roc}.
#'
#' @param formula Formula specifying observation and forecast
#' @param data optional; data frame which incorporates the variables named in
#'   \code{formula}. If \code{data} is not specified, \code{roc} searches
#'   for the variables specified in \code{formula} in .GlobalEnv.
#' @param method Method specifies the type of ROC curve to be computed. Partial
#'   matching is supported and the following names are permitted
#'   "empirical" generates only the empirical ROC curve
#'   "bin2p" generates the classical 2-parameter binormal ROC model
#'   "bin3p" generates the 3-parameter binormal ROC model
#'   "beta2p" generate the 2-parameter beta ROC model
#'   "beta3p_v" generates the 3-parameter beta ROC model with a
#'     vertical straight edge at (0,0)
#'   "beta3p_h" generates the 3-parameter beta ROC model with a
#'     horizontal straight edge at (1,1)
#'   "beta4p" generates the 4-parameter beta ROC model
#'
#' @return The output is an object of class \code{roc} which is a list
#'   containing the following components:
#'   \tabular{ll}{
#'     \code{x} \tab TBD \cr
#'     \code{y} \tab TBD
#'   }
#'
#' @details TBD
#'
#' @export
#'
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
  res$model.frame <- model.frame(formula, data, na.action = na.omit)
  names(res$model.frame) <- c("obs", "forc")
  res$model.frame <- check_model.frame(res$model.frame)

  res$emp_info <- match.arg(emp_info, info_avail)
  res$empROC <- roc_empirical(res$model.frame, res$emp_info)

  method_s <- match.arg(fit_method, method_avail)
  methods <- if(method_s == "empirical") method_s else c(method_s, "empirical")

  res$MDE_info <- list(
    method = methods,
    info   = match.arg(fit_info, info_avail))
  res$MDEfit <- MDEfit()

  class(res) <- "roc"
  return(res)
}

print("FIT - INITIAL PARS - ERLEDIGT -> TESTEN")
print("FIT - MD ESTIMATION - EINFUEGEN")
print("ASYMPTOTISCHE VERTEILUNG EINFUEGEN")
print("PLOTS EINFUEGEN")
print("STATISTICAL TESTS EINFUEGEN")
print("UNIT TESTS EINFUEGEN")

