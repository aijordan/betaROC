#' Observation forecast object
#'
#' \code{observationforecast} constructs and returns an object of class
#' \code{obsforc}.
#'
#' @param observation An vector containing observations where zeros represent
#'   misses and ones represent hits.
#' @param forecast A vector of type \code{numeric} (or \code{character} when
#'   convertible to \code{numeric}) that contains per forecast and observed
#'   value one predicted value
#'
#' @param dataset optional; name of the dataset
#' @param forecastname optional; name of the forecast
#'
#' @return The output is an object of class \code{obsforc}, which is a list
#'   containing the following components: \tabular{ll}{ \code{obs} \tab a vector
#'   of observations \cr \code{forc} \tab a vector or matrix of forecasts\cr }
#' @export
observationforecast <- function(observation, forecast, dataset = NULL,
                                forecastname = NULL){

  if(length(observation) != length(forecast)){
    stop("observation and forecast have different lengths.")
  }

  if(is.factor(forecast)){
    tx <- type.convert(levels(forecast))
    if(is.factor(tx)){
      stop("forecast can not be transformed into numeric vector")
    }
    x <- as.numeric(type.convert(as.character.factor(forecast)))
  }else{
    x <- type.convert(forecast)
    if(is.character(x)){
      stop("forecast can not be transformed into numeric vector.")
    }
    x <- as.numeric(type.convert(x))
  }

  if(is.factor(observation)){
    ty <- type.convert(levels(observation))
    if(is.factor(ty)){
      stop("observation can not be transformed into numeric vector")
    }
    y <- as.numeric(type.convert(as.character.factor(observation)))
  }else{
    y <- type.convert(observation)
    if(is.character(y)){
      stop("observation can not be transformed into numeric vector")
    }
    y <- as.numeric(type.convert(y))
  }

  of <- list(
    obs      = x,
    forc     = y)
  class(of) <- c("obsforc")

  # dataset <- if(!is.null(dataset)){ dataset }else{""}
  # forecastname <- if(!is.null(forecastname)){ forecastname }else{""}
  #
  # of <- list(
  #   obs      = x,
  #   forc     = y,
  #   dataset  = dataset,
  #   forecast = forecastname)
  # class(of) <- "obsforc"

  return(of)
}






