% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/roc.R
\name{roc}
\alias{roc}
\title{Receiver Operating Characteristic (ROC) object}
\usage{
roc(formula, data, emp_info, fit_method, fit_info)
}
\arguments{
\item{formula}{Formula specifying observation and forecast}

\item{data}{optional; data frame which incorporates the variables named in
\code{formula}. If \code{data} is not specified, \code{roc} searches for
the variables specified in \code{formula} in .GlobalEnv.}

\item{emp_info}{should the empirical ROC curve be \code{"concave"} or \code{"unrestricted"}.}

\item{fit_method}{Method specifies the type of ROC curve to be computed. Partial
matching is supported and the following names are permitted.
\tabular{ll}{
\code{"empirical"} \tab generates only the empirical ROC curve \cr
\code{"bin2p"} \tab generates the classical 2-parameter binormal ROC model \cr
\code{"bin3p"} \tab generates the 3-parameter binormal ROC model \cr
\code{"beta2p"} \tab generate the 2-parameter beta ROC model \cr
\code{"beta3p_v"} \tab generates the 3-parameter beta ROC model with a
vertical straight edge at (0,0) \cr
\code{"beta3p_h"} \tab generates the 3-parameter beta ROC model with a
horizontal straight edge at (1,1) \cr
\code{"beta4p"} \tab generates the 4-parameter beta ROC model
}}

\item{fit_info}{should the parametric ROC curve be \code{"concave"} or \code{"unrestricted"}.}
}
\value{
The output is an object of class \code{roc} which is a list
  containing the following components:
  \tabular{ll}{
  \code{formula} \tab The formula used within the \code{roc} command \cr
  \code{model.frame} \tab The model frame constructed from the data \cr
  \code{emp_info} \tab Information on the construction of the empirical
  ROC curve \cr
  \code{empROC} \tab \code{data.frame} containing true positive and false
  positive rates of the empirical ROC curve \cr
  \code{MDE_info} \tab \code{list} naming the required fits and the
  constraint for the MDE fit \cr
  \code{MDE_fit} \tab \code{list} with initial and final parameters and
  associated L2 distances between empirical and parametric ROC curve
  }
}
\description{
\code{roc} constructs and returns an object of class \code{roc}.
}
\details{
to be added
}
