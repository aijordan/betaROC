library(dplyr)
library(here)

# Source: 'ROCR' package, https://cran.r-project.org/package=ROCR
# License: GPL-2 | GPL-3 [expanded from: GPL (>= 2)]
# Study: HIV coreceptor usage
# Reference:
#   Sing T, Sander O, Beerenwinkel N, Lengauer, T (2005). ROCR: Visualizing
#   classifier performance in R. Bioinformatics, 21, 3940--3941.

data(ROCR.hiv, package = "ROCR")

hiv_Sing <- tibble(
  obs = 0.5 * (1 + do.call(c, ROCR.hiv$hiv.svm$labels)),
  forc = do.call(c, ROCR.hiv$hiv.svm$predictions),
)

save(hiv_Sing, file = here("data/hiv_Sing.RData"))
