% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{PSA_Etzioni}
\alias{PSA_Etzioni}
\title{Prostate-specific antigen (PSA) and prostate cancer occurence}
\format{
A tibble with 116 rows and 2 variables:
\describe{
  \item{\code{obs}}{a binary variable indicating prostate cancer occurence}
  \item{\code{forc}}{the ratio of free to total PSA two years prior to diagnosis}
}
}
\source{
https://research.fhcrc.org/diagnostic-biomarkers-center/en/datasets.html

Reference:
  Etzioni R, Pepe M, Longton G, Hu C, Goodman G (1999). Incorporating the time
  dimension in receiver operating characteristic curves: A case study of
  prostate cancer. Medical Decision Making 19:242-51.
}
\usage{
PSA_Etzioni
}
\description{
A modified version of the data set 'psa2b.csv' available at the source below.
}
\keyword{datasets}
