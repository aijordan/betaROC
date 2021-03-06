% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{hiv_Sing}
\alias{hiv_Sing}
\title{Support vector machine (SVM) predictions for HIV-1 coreceptor usage}
\format{
A tibble with 3450 rows and 2 variables:
\describe{
  \item{\code{obs}}{a binary variable indicating HIV-1 coreceptor usage.}
  \item{\code{forc}}{predictions made by the SVM model}
}
}
\source{
'ROCR' package, https://cran.r-project.org/package=ROCR

Reference:
  Sing T, Sander O, Beerenwinkel N, Lengauer, T (2005). ROCR: Visualizing
  classifier performance in R. Bioinformatics, 21, 3940--3941.
}
\usage{
hiv_Sing
}
\description{
A modified version of the data set 'ROCR.hiv' from the ROCR package.
}
\keyword{datasets}
