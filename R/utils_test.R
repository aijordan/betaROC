#' Removes the attribute terms from a model.frame
#'
#' @param mf Model frame
#' @return Returns a data.frame
rm_attribute_terms <- function(mf){
  if(!is.null(attributes(mf)$terms)) attr(mf, "terms") <- NULL
  return(mf)
}