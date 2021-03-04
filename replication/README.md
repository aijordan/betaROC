Replication material: Gneiting and Vogel (2021)
================

``` r
library(tidyverse)
library(devtools)
load_all()

data("aSAH_Robin")
data("hiv_Sing")
data("PSA_Etzioni")
data("WS_Vogel")
```

``` r
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

df <- tibble(
  study = rep(c("Robin", "Sing", "Etzioni", "Vogel"), each = 4),
  empROC = rep(empROC, each = 4),
  MDE_info = rep(MDE_info, times = 4)
) %>%
  mutate(
    params = map2(empROC, MDE_info, fit_MDE)
  )
```

### Etzioni et al. (1999)

Prostate cancer antigen ratio, 116 observations

``` r
df_Etzioni <- filter(df, study == "Etzioni")

with(df_Etzioni, {
  c(
    sprintf("Binormal model - unrestricted"),
    sprintf("Parameters (mu, sigma):     (%.2f, %.2f)",
            params[[1]]$pars_fit[1],
            params[[1]]$pars_fit[2]
    ),
    sprintf("L2-distance:                 %.3f",
            params[[1]]$L2_fit),
    
    sprintf("Binormal model - concave"),
    sprintf("Parameters (mu, sigma):     (%.2f, %.2f)",
            params[[2]]$pars_fit[1],
            params[[2]]$pars_fit[2]
    ),
    sprintf("L2-distance:                 %.3f",
            params[[2]]$L2_fit),
    
    sprintf("Beta model - unrestricted"),
    sprintf("Parameters (alpha, beta):   (%.2f, %.2f)",
            params[[3]]$pars_fit[1],
            params[[3]]$pars_fit[2]
    ),
    sprintf("L2-distance:                 %.3f",
            params[[3]]$L2_fit),
    
    sprintf("Binormal model - concave"),
    sprintf("Parameters (alpha, beta):   (%.2f, %.2f)",
            params[[4]]$pars_fit[1],
            params[[4]]$pars_fit[2]
    ),
    sprintf("L2-distance:                 %.3f",
            params[[4]]$L2_fit)
  )
}) %>%
  cat(sep = "\n")
```

    ## Binormal model - unrestricted
    ## Parameters (mu, sigma):     (1.05, 0.78)
    ## L2-distance:                 0.043
    ## Binormal model - concave
    ## Parameters (mu, sigma):     (1.22, 1.00)
    ## L2-distance:                 0.056
    ## Beta model - unrestricted
    ## Parameters (alpha, beta):   (0.34, 1.32)
    ## L2-distance:                 0.042
    ## Binormal model - concave
    ## Parameters (alpha, beta):   (0.39, 1.61)
    ## L2-distance:                 0.045

``` r
with(df_Etzioni, {
  p <- plot_roc_empirical(empROC[[1]])
  p <- plot_binormal(params[[1]]$pars_fit, MDE_info[[1]], p, color = "red")
  p <- plot_binormal(params[[2]]$pars_fit, MDE_info[[2]], p, lty = 2, color = "red")
  p <- plot_beta(params[[3]]$pars_fit, MDE_info[[3]], p, color = "blue")
  p <- plot_beta(params[[4]]$pars_fit, MDE_info[[4]], p, lty = 2, color = "blue")
  p +
    theme(aspect.ratio = 1) +
    geom_segment(mapping = aes(x = 0, xend = 1, y = 0, yend = 1), lty = 2)
})
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Empirical (black), fitted binormal (red) and fitted beta (blue) ROC
curves in the unrestricted (solid) and concave (dashed) case.

### Sing et al. (2005)

Coreceptor usage SVM predictor, 3450 observations

    ## Binormal model - unrestricted
    ## Parameters (mu, sigma):     (1.58, 0.65)
    ## L2-distance:                 0.019
    ## Binormal model - concave
    ## Parameters (mu, sigma):     (2.05, 1.00)
    ## L2-distance:                 0.039
    ## Beta model - unrestricted
    ## Parameters (alpha, beta):   (0.15, 1.44)
    ## L2-distance:                 0.023
    ## Binormal model - concave
    ## Parameters (alpha, beta):   (0.17, 1.83)
    ## L2-distance:                 0.025

![](README_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

Empirical (black), fitted binormal (red) and fitted beta (blue) ROC
curves in the unrestricted (solid) and concave (dashed) case.

### Robin et al. (2011)

Clinical outcome S100\(\beta\) concentration, 113 observations

    ## Binormal model - unrestricted
    ## Parameters (mu, sigma):     (0.75, 0.72)
    ## L2-distance:                 0.033
    ## Binormal model - concave
    ## Parameters (mu, sigma):     (0.91, 1.00)
    ## L2-distance:                 0.060
    ## Beta model - unrestricted
    ## Parameters (alpha, beta):   (0.36, 0.96)
    ## L2-distance:                 0.032
    ## Binormal model - concave
    ## Parameters (alpha, beta):   (0.52, 1.48)
    ## L2-distance:                 0.050

![](README_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Empirical (black), fitted binormal (red) and fitted beta (blue) ROC
curves in the unrestricted (solid) and concave (dashed) case.

### Vogel et al. (2018)

Precipitation NWP forecast, 5449 observations

    ## Binormal model - unrestricted
    ## Parameters (mu, sigma):     (1.13, 1.22)
    ## L2-distance:                 0.008
    ## Binormal model - concave
    ## Parameters (mu, sigma):     (0.99, 1.00)
    ## L2-distance:                 0.031
    ## Beta model - unrestricted
    ## Parameters (alpha, beta):   (0.79, 2.57)
    ## L2-distance:                 0.006
    ## Binormal model - concave
    ## Parameters (alpha, beta):   (0.79, 2.57)
    ## L2-distance:                 0.006

![](README_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Empirical (black), fitted binormal (red) and fitted beta (blue) ROC
curves in the unrestricted (solid) and concave (dashed) case.
