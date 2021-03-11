# betaROC

## Minimum distance estimation (MDE) for receiver operating characteristic (ROC) curves

ROC curves are used ubiquitously to evaluate scores, features, covariates or
markers as potential predictors in binary problems. There exists an equivalence
between ROC curves and cumulative distribution functions (CDFs), which supports
a subtle shift of paradigms in the statistical modelling of ROC curves
(Gneiting and Vogel 2021). The flexible two-parameter beta family can be used
in the context of fitting CDFs to empirical ROC curves, and in a range
of empirical examples the beta family fits better than the classical binormal
model, particularly under the vital constraint of the fitted curve being concave.

## Contents

The betaROC repository contains:

- an early development version of the betaROC package for the statistical programming language R,
- and replication material (folder "/replication") for the corresponding paper by Gneiting and Vogel (2021).

The folder "/data" contains:

- the data set "aSAH_Robin.RData", which is a modified version (see "/data/raw/aSAH_Robin.R") of the data set "aSAH" from the [pROC package](https://cran.r-project.org/package=pROC),
- the data set "hiv_Sing.RData", which is a modified version (see "/data/raw/hiv_Sing.R") of the data set "ROCR.hiv" from the [ROCR package](https://cran.r-project.org/package=ROCR),
- the data set "PSA_Etzioni.RData", which is a modified version (see "/data/raw/PSA_Etzioni.R") of the data set "psa2b.csv" from the "CARET PSA" study by Etzioni et al. (1999) and available from the [Diagnostic and Biomarkers Statistical (DABS) Center](https://research.fredhutch.org/diagnostic-biomarkers-center/en/datasets.html),
- the data set "WS_Vogel.RData", which is a modified version of "/data/raw/Forecast_observation_data_WS.RData" (source: Vogel et al. 2018), which uses historic products from the European Center for Medium-Range Weather Forecasts ([ECMWF](https://www.ecmwf.int/)).

## References

Etzioni R, Pepe M, Longton G, Hu C, Goodman G (1999). Incorporating the time
dimension in receiver operating characteristic curves: A case study of
prostate cancer. Medical Decision Making 19:242-51.

Gneiting T, Vogel P (2021). Receiver Operating Characteristic (ROC) Curves:
Equivalences, Beta Model, and Minimum Distance Estimation. Preprint
[arXiv:1809.04808](https://arxiv.org/abs/1809.04808).

Robin X, Turck N, Hainard A, Tiberti N, Lisacek F, Sanchez J-C,
Müller M (2011). pROC: An open-source package for R and S+ to analyze and
compare ROC curves. BMC Bioinformatics, 12, 77.

Sing T, Sander O, Beerenwinkel N, Lengauer T (2005). ROCR: visualizing
classifier performance in R. Bioinformatics, 21(20):3940-1.

Vogel P, Knippertz P, Fink AH, Schlueter A, Gneiting T (2018). Skill of
global raw and postprocessed ensemble predictions of rainfall over northern
tropical Africa. Weather and Forecasting, 33, 369--388.


