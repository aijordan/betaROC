library(dplyr)
library(here)

# Data set: file "data/raw/Forecast_observation_data_WS.RData"
# Study: Precipitation in the West Sahel region
# Reference:
#   Vogel P, Knippertz P, Fink AH, Schlueter A, Gneiting T (2018). Skill of
#   global raw and postprocessed ensemble predictions of rainfall over northern
#   tropical Africa. Weather and Forecasting, 33, 369--388.

load(here("data/raw/Forecast_observation_data_WS.Rdata"))
WS_Vogel <- select(obsforc, obs = obs, forc = "ENS")

save(WS_Vogel, file = here("data/WS_Vogel.RData"))
