# Behavioural preview effect with faces
[![DOI](https://zenodo.org/badge/254319738.svg)](https://zenodo.org/badge/latestdoi/254319738)

This repository contains the complete analysis code and additional information for  
Huber-Huber &amp; Melcher (2020) The behavioural preview effect with faces results from predictive processing across the saccade. Manuscript submitted for publication.

To recompute the results, get the raw data file from [osf.io](https://osf.io/), with [doi:10.17605/OSF.IO/TY69K](doi.org/10.17605/OSF.IO/TY69K), and put it into a folder with the name 'data' inside this repository.

Start from the file `beh_pre_eff.Rmd` which contains R code and markdown. Processing this file with the R package `knitr` or the knit-to-html function in [RStudio](https://rstudio.com) creates the html file `beh_pre_eff.html` also included in this  repository. This html file contains all results, figures, and some additional information.

Required R packages and versions at the time `beh_pre_eff.html` has been created:
```r
R version 3.6.1 (2019-07-05)
Platform: x86_64-apple-darwin15.6.0 (64-bit)
Running under: macOS Sierra 10.12.6

Matrix products: default
BLAS:   /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRblas.0.dylib
LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

locale:
[1] C

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
[1] knitr_1.24        citr_0.3.2        stargazer_5.2.2   boot_1.3-23       emmeans_1.4.5     lme4_1.1-21       Matrix_1.2-17     ggplot2_3.2.1    
[9] data.table_1.12.2

loaded via a namespace (and not attached):
 [1] gtools_3.8.1     tidyselect_0.2.5 xfun_0.9         reshape2_1.4.3   purrr_0.3.2      splines_3.6.1    lattice_0.20-38  colorspace_1.4-1
 [9] miniUI_0.1.1.1   htmltools_0.3.6  yaml_2.2.0       rlang_0.4.0      pillar_1.4.2     nloptr_1.2.1     later_0.8.0      glue_1.3.1      
[17] withr_2.1.2      plyr_1.8.4       stringr_1.4.0    munsell_0.5.0    gtable_0.3.0     mvtnorm_1.0-11   coda_0.19-3      evaluate_0.14   
[25] labeling_0.3     httpuv_1.5.1     highr_0.8        Rcpp_1.0.2       xtable_1.8-4     scales_1.0.0     promises_1.0.1   mime_0.7        
[33] digest_0.6.20    stringi_1.4.3    dplyr_0.8.3      shiny_1.3.2      grid_3.6.1       tools_3.6.1      magrittr_1.5     lazyeval_0.2.2  
[41] tibble_2.1.3     crayon_1.3.4     pkgconfig_2.0.2  MASS_7.3-51.4    estimability_1.3 assertthat_0.2.1 minqa_1.2.4      rmarkdown_1.15  
[49] rstudioapi_0.10  R6_2.4.0         nlme_3.1-140     compiler_3.6.1
```
