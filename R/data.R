#' S100beta biomarker concentration and health outcome
#'
#' @format A tibble with 113 rows and 2 variables:
#' \describe{
#'   \item{\code{obs}}{a binary variable indicating positive or negative health outcome.}
#'   \item{\code{forc}}{S100beta biomarker concentration}
#' }
#'
#' @description A modified version of the data set 'aSAH' from the pROC package.
#'
#' @source
#' 'pROC' package, https://cran.r-project.org/package=pROC
#'
#' Reference:
#'   Robin X, Turck N, Hainard A, Tiberti N, Lisacek F, Sanchez J-C,
#'   Müller M (2011). pROC: An open-source package for R and S+ to analyze and
#'   compare ROC curves. BMC Bioinformatics, 12, 77.
#'
"aSAH_Robin"


#' Support vector machine (SVM) predictions for HIV-1 coreceptor usage
#'
#' @format A tibble with 3450 rows and 2 variables:
#' \describe{
#'   \item{\code{obs}}{a binary variable indicating HIV-1 coreceptor usage.}
#'   \item{\code{forc}}{predictions made by the SVM model}
#' }
#'
#' @description A modified version of the data set 'ROCR.hiv' from the ROCR package.
#'
#' @source
#' 'ROCR' package, https://cran.r-project.org/package=ROCR
#'
#' Reference:
#'   Sing T, Sander O, Beerenwinkel N, Lengauer, T (2005). ROCR: Visualizing
#'   classifier performance in R. Bioinformatics, 21, 3940--3941.
#'
"hiv_Sing"

#' Prostate-specific antigen (PSA) and prostate cancer occurence
#'
#' @format A tibble with 116 rows and 2 variables:
#' \describe{
#'   \item{\code{obs}}{a binary variable indicating prostate cancer occurence}
#'   \item{\code{forc}}{the ratio of free to total PSA two years prior to diagnosis}
#' }
#'
#' @description A modified version of the data set 'psa2b.csv' available at the source below.
#'
#' @source
#' https://research.fhcrc.org/diagnostic-biomarkers-center/en/datasets.html
# Study: CARET PSA
#'
#' Reference:
#'   Etzioni R, Pepe M, Longton G, Hu C, Goodman G (1999). Incorporating the time
#'   dimension in receiver operating characteristic curves: A case study of
#'   prostate cancer. Medical Decision Making 19:242-51.
#'
"PSA_Etzioni"

#' Precipitation forcasts and occurence in the West Sahel region
#'
#' @format A tibble with 5449 rows and 2 variables:
#' \describe{
#'   \item{\code{obs}}{a binary variable indicating occurence of precipitation}
#'   \item{\code{forc}}{probability of precipition}
#' }
#'
#' @description A modified version of the data set used in the study by Vogel et al (2018).
#'
#' @source
#' Vogel P, Knippertz P, Fink AH, Schlueter A, Gneiting T (2018). Skill of
#'   global raw and postprocessed ensemble predictions of rainfall over northern
#'   tropical Africa. Weather and Forecasting, 33, 369--388.
#'
#' This data set contains modified historic products from the European Center for
#' Medium-Range Weather Forecasts (ECMWF, https://www.ecmwf.int/), specifically:
#' ensemble forecasts of precipitation that have been summarized to a probability of
#' precipitation, and historical observations for the occurence of precipitation.
#' The ECMWF licenses the use of expired real-time data products under the
#' Creative Commons Attribution 4.0 International license
#' (CC-BY 4.0, https://creativecommons.org/licenses/by/4.0/).
#'
"WS_Vogel"
