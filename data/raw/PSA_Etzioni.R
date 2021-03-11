library(dplyr)
library(here)

# Data set: https://research.fhcrc.org/diagnostic-biomarkers-center/en/datasets.html
# Study: CARET PSA
# Reference:
#   Etzioni R, Pepe M, Longton G, Hu C, Goodman G (1999). Incorporating the time
#   dimension in receiver operating characteristic curves: A case study of
#   prostate cancer. Medical Decision Making 19:242-51.


CARET <-
  read.csv(
    "https://research.fredhutch.org/content/dam/stripe/diagnostic-biomarkers-statistical-center/files/psa2b.csv"
  )
PSA_Etzioni <- tibble(
  obs = CARET$d,
  forc = -log(CARET$fpsa / CARET$tpsa),
  t = CARET$t,
  id = CARET$id
) %>%
  group_by(id) %>%
  filter(t >= -2 & t <= 0) %>%
  slice(1) %>%
  ungroup() %>%
  select(obs, forc)

save(PSA_Etzioni, file = here("data/PSA_Etzioni.RData"))
