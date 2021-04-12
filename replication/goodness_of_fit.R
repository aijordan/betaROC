devtools::load_all()

data("aSAH_Robin")
data("hiv_Sing")
data("PSA_Etzioni")
data("WS_Vogel")

empROC <- lapply(
  list(aSAH_Robin, hiv_Sing, PSA_Etzioni, WS_Vogel),
  function(x) {
    roc(obs ~ forc, x)
  })

MDE_info <- list(
  list(method = "bin2p", info = "unrestricted"),
  list(method = "bin2p", info = "concave"),
  list(method = "beta2p", info = "unrestricted"),
  list(method = "beta2p", info = "concave")
)

df_tests <- tibble::tibble(
  study = rep(c("Robin", "Sing", "Etzioni", "Vogel"), each = 4),
  empROC = rep(empROC, each = 4),
  MDE_info = rep(MDE_info, times = 4)
)

foldername <- "goodness_of_fit_files"
if (dir.exists(foldername)) {
  purrr::pwalk(df_tests, function(study, empROC, MDE_info) {
    filename <- sprintf("%s_%s_%s.RData",
                        study, MDE_info$method, MDE_info$info)
    filepath <- sprintf("%s/%s", foldername, filename)
    if (!file.exists(filepath)) {
      set.seed(42)
      results <- resample_test(empROC, MDE_info, 999)
      save("results", file = filepath)
    }
  })
}
