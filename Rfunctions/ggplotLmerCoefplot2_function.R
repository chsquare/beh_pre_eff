
ggplot.lmer.coefplot2 <- function(...,
                                  cis = NULL,
                                  coef.names = NULL,
                                  ylim = NULL,
                                  Labels = NULL,
                                  plot.intercept = F,
                                  coef.order = NULL,
                                  model.names = NULL) {
  # Coefficient plot similar to "coefplot2", however done with ggplot instead.
  # It also plots fixed effects, but only the coefficients with confidence intervals, so it's not the same
  # as ggplot.lmer.fixef
  # 
  # Works only for lmerMod objects.
  #
  # '...' can also be one specific (!) data.frame containing the columns
  #
  # 'cis' defaults to NULL and in this case if the input '...' was a data.frame, we assume this
  # data.frame contains columns that contain 2.5% and 97.5% limits of the ci. If input '...' is not
  # a single data.frame with the cis, cis can be a number specifying the multiple of each fixed 
  # effects standard error to be plotted as confidence interval, i.e. cid=1.96 for 95% confidence
  # interval (which would probably be of the Wald type, and are usually not recommended).
  #
  # 'coef.names' can also contain expressions for greek symbols or whatever which 
  # requires 'coef.names' to be a 'list'
  #
  # 'Labels' is added to the ggplot commands, thus could be any ggplot-objects to be added
  # to the graph, e.g. labels(x = "A new name for the x-axis"), however x and y might have 
  # consider the flipped coordinate system.
  #
  # 'plot.intercept' TRUE/FALSE whether the include the intercept
  # 
  # specify 'coef.order' for the order in which coefficients should be plotted.
  #
  # 'model.names' can give a name to each model supplied in '...'
  
  suppressPackageStartupMessages(library(lme4))
  suppressPackageStartupMessages(library(ggplot2))
  
  # consider potential ylim
  if (length(ylim) == 0) {cf <- coord_flip()} else {cf <- coord_flip(ylim=ylim)}
  
  prepare <- function(model) {
    
    if (!any(class(model) %in% c("lmerMod", "glmerMod")) & !any(class(model)=="data.frame")) stop("lmerMod or data.frame required!")
    
    # if not a data.frame, get estimates from model
    if (!is.data.frame(model)) {
      pdat <- data.frame(Estimate = fixef(model))
    } else {
      pdat <- model # the model is actually a data.frame as results from boot.ci.table(.boot-object)
    } 
    
    # check names
    if (is.data.frame(model)) { # a data.frame input
      pdat$CoefNames <- pdat$Parameter # "Parameter" is assumed to be contained in a ".boots" data.frame object that can be input to this function!
    } else { # a (g)lmerMod input
      pdat$CoefNames <- names(fixef(model))
    }
    
    # add Standard Errors
    if (!is.data.frame(model)) { # ... in case input was a model
      pdat$stderr <- sqrt(diag(vcov(model)))
    }
    
    # plot intercept?
    if (!plot.intercept) {
      #   NOTE: 'CoefNames' might be an 'expression', which can NOT be compared by '!='
      if (is.list(pdat$CoefNames)) { # if coef.names contain an expression, 'coef.names' and 'pdat$CoefNames' are a list.
        CoefNameInterceptIndex <- unlist(lapply(pdat$CoefNames, function(x) {
          if (is.expression(x[[1]])) FALSE else x[[1]] == "(Intercept)"
        }))
        pdat <- subset(pdat, !CoefNameInterceptIndex)
      } else {
        pdat <- subset(pdat, CoefNames != "(Intercept)")
      }
    }
    
    # in the case that 'confint' was used, variable names have to be updated
    colnames(pdat) <- gsub(" %", "%", colnames(pdat))
    
    # check cis
    if (is.data.frame(model)) {
      # if the input 'model' was a data.frame, we assume this data.frame contains columns that contain 2.5% and 97.5% limits of the ci.
      # that's usually the case for outputs of 'boot.ci.table'
      AES <- aes(x=CoefNamesFact, y=Estimate, ymin=`2.5%`, ymax=`97.5%`)
    } else {
      if (!is.null(cis)) {
        if (is.matrix(cis)) {
          warning("Adding CIs from matrix 'cis' to a model has not been tested and might fail!")
          pdat$lowerCI <- cis[,1]
          pdat$upperCI <- cis[,2]
        } else {
          if (is.numeric(cis)) { # if 'cis' is numeric, it denotes the multiple of Standard Error that should be plotted
            pdat$lowerCI <- pdat$Estimate + cis * pdat$stderr # 'stderr' was added above.
            pdat$upperCI <- pdat$Estimate - cis * pdat$stderr
          } else {
            stop("Unrecognized class for input 'cis'!")
          }
        }
        AES <- aes(x=CoefNamesFact, y=Estimate, ymin=lowerCI, ymax=upperCI)
      } else { # is.null(cis) # so do NOT plot intervals
        AES <- aes(x=CoefNamesFact, y=Estimate, ymin=Estimate, ymax=Estimate)
      }
    }
    
    return(list(pdat = pdat, AES = AES, coef.names = coef.names))
    
  } # end 'prepare'
  
  model.list <- list(...)
  
  if (is.null(model.names))
    names(model.list) <- paste("m", 1:length(model.list), sep = "") else
      names(model.list) <- model.names
  
  prep.list <- lapply(model.list, prepare) # keeps names
  
  pdats <- lapply(prep.list, function(x) x$pdat)
  
  for (i in 1:length(pdats)) {
    pdats[[i]]$model <- names(pdats)[i]
  }
  
  pdat <- Reduce(rbind, pdats)
  
  # ensure CoefNames are sorted in the plot as originally in 'pdat' (reversing order necessary, and creating factor avoids alphabetical sorting!)
  if (is.null(coef.order)) {
    pdat$CoefNamesFact <- factor(pdat$CoefNames, levels = unique(pdat$CoefNames)[length(unique(pdat$CoefNames)):1]) # reverse order! since "lower" values would be in the bottom of the plot, although they should start at top
  } else { # manually specified order of coefficients
    pdat$CoefNamesFact <- factor(pdat$CoefNames, levels = unique(pdat$CoefNames)[coef.order][length(unique(pdat$CoefNames)):1])
  }
  
  if (nlevels(pdat$CoefNamesFact) != length(coef.names)) {
    stop(sprintf("ggplot.lmer.coefplot2 says: need as many 'coef.names' as there are coefficients to plot (%d, currently %d coef.names)! Consider (Intercept)!", nlevels(pdat$CoefNamesFact), length(coef.names)))
  }
  
  # re-do AES (see lines above, lines 103-125)
  if (is.data.frame(model.list[[1]])) {
    AES <- aes(x=CoefNamesFact, y=Estimate, ymin=`2.5%`, ymax=`97.5%`)
  } else {
    if (!is.null(cis)) {
      AES <- aes(x=CoefNamesFact, y=Estimate, ymin=lowerCI, ymax=upperCI)
    } else {
      AES <- aes(x=CoefNamesFact, y=Estimate, ymin=Estimate, ymax=Estimate)
    }
  }
  
  # create plot
  p <- ggplot(pdat, AES) + facet_grid(. ~ model) + 
    geom_hline(yintercept=0) + geom_pointrange() + cf + # cf ... coordinate flip, see above!
    labs(x = NULL) + Labels +  # adding NULL seems to be ok
    theme_bw() + theme(axis.ticks.y=element_blank(), 
                       panel.grid=element_blank())
  
  if (!is.null(coef.order)) {
    p <- p + scale_x_discrete(labels = coef.names[coef.order][length(coef.names):1])
  } else  {
    p <- p + scale_x_discrete(labels = coef.names[length(coef.names):1])
  }
  
  return(p)
}
