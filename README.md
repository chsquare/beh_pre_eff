# Behavioural preview effect with faces
[![DOI](https://zenodo.org/badge/254319738.svg)](https://zenodo.org/badge/latestdoi/254319738)

This repository contains the complete analysis code and additional information for  
**Huber-Huber, C. &amp; Melcher, D. (in press) The behavioural preview effect with faces is susceptible to statistical regularities: Evidence for predictive processing across the saccade. *Scientific Reports*. http://dx.doi.org/10.1038/s41598-020-79957-w**

To recompute the results, get the raw data file from [osf.io](https://osf.io/), with [doi:10.17605/OSF.IO/TY69K](https://doi.org/10.17605/OSF.IO/TY69K), and put it into a folder with the name 'data' inside this repository.

Start from the file `beh_pre_eff.Rmd` which contains R code and markdown. Processing this file with the R package `knitr` or the knit-to-html function in [RStudio](https://rstudio.com) creates the html file `beh_pre_eff.html` also included in this repository. This html file contains all results, figures, and some additional information.

Required R packages and versions at the time `beh_pre_eff.html` has been created:
```r
R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Catalina 10.15.6

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] C

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] knitr_1.24        citr_0.3.2        stargazer_5.2.2   boot_1.3-23       emmeans_1.4.5     lme4_1.1-21       Matrix_1.2-18     gtable_0.3.0     
 [9] ggplot2_3.2.1     data.table_1.12.2

loaded via a namespace (and not attached):
 [1] gtools_3.8.1     tidyselect_1.1.0 xfun_0.9         reshape2_1.4.3   purrr_0.3.2      splines_3.6.1    lattice_0.20-38  colorspace_1.4-1
 [9] vctrs_0.3.4      generics_0.0.2   miniUI_0.1.1.1   htmltools_0.3.6  yaml_2.2.0       rlang_0.4.7      pillar_1.4.2     nloptr_1.2.1    
[17] later_0.8.0      glue_1.4.2       withr_2.1.2      plyr_1.8.4       lifecycle_0.2.0  stringr_1.4.0    munsell_0.5.0    mvtnorm_1.0-11  
[25] coda_0.19-3      evaluate_0.14    labeling_0.3     httpuv_1.5.1     highr_0.8        Rcpp_1.0.2       xtable_1.8-4     promises_1.0.1  
[33] scales_1.0.0     mime_0.7         digest_0.6.20    stringi_1.4.3    dplyr_1.0.2      shiny_1.3.2      tools_3.6.1      magrittr_1.5    
[41] lazyeval_0.2.2   tibble_2.1.3     crayon_1.3.4     pkgconfig_2.0.2  MASS_7.3-51.4    estimability_1.3 assertthat_0.2.1 minqa_1.2.4     
[49] rmarkdown_1.15   rstudioapi_0.10  R6_2.4.0         nlme_3.1-140     compiler_3.6.1  
```
