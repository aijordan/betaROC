library(dplyr)
library(here)

# Source: 'pROC' package, https://cran.r-project.org/package=pROC
# License: GPL (>= 3)
# Study: S100beta biomarker level
# Reference:
#   Robin X, Turck N, Hainard A, Tiberti N, Lisacek F, Sanchez J-C,
#   Müller M (2011). pROC: An open-source package for R and S+ to analyze and
#   compare ROC curves. BMC Bioinformatics, 12, 77.

data(aSAH, package = "pROC")

aSAH_Robin <- tibble(
  obs = as.numeric(aSAH$outcome) - 1,
  forc = as.numeric(aSAH$s100b)
)

save(aSAH_Robin, file = here("data/aSAH_Robin.RData"))
