
anova.reml <- function(..., useAICc = FALSE,
                       suppressNegativeChiDf = F,
                       omit.fitting.method = F) {
  # Does models comparisons with LRT on the sequence of models entered in '...'
  # in the order of their occurance. Two subsequent models have to be of same
  # type (ML or REML) in order for a likelihood ratio test to be conducted.
  #
  # suppressNegativeChiDf ... if TRUE omits LRT test statistics from table, if 
  #    a negative chisq df value would be present. This is the case when a
  #    more complex model is followed by a simpler model (fewer df).
  # omit.fitting.method ... TRUE/FALSE, prints the fitting method (ML or REML)
  #    for each model in the table.
  
  list.of.... <- list(...)
  if (! is.null(names(list.of....)))
    # models have names of "", but only if there are additional input arguments
    list.of.models <- list.of....[names(list.of....) == ""] else
      # if there are no additional input arguments, the names of ... are NULL
      list.of.models <- list.of....
  
  if (length(list.of.models) <= 1) {stop("anova.reml says: More than one model is required to do model comparison!")}
  
  # check whether all models built on same dataset
  # **this check is NOT exhaustive, but it's a good approximation and probably not much
  #   different to what the default 'anova' function does**
  model.dataset.nobs <- unlist(lapply(list.of.models, function(x) nrow(x@frame)))
  model.dataset.names <- unlist(lapply(list.of.models, function(x) paste(as.character(x@call$data), collapse=" ")))
  model.dataset.rownames <- lapply(list.of.models, function(x) rownames(x@frame))
  if (length(unique(model.dataset.nobs))!=1) stop("anova.reml says: Need the same number of observations for each model!")
  if (length(unique(model.dataset.names))!=1) stop("anova.reml says: Each model has to have the same datset (at least the same name)!")
  all.subsequent.rownames.the.same <- vapply(1:(length(model.dataset.rownames)-1),
                                             function(i) all(model.dataset.rownames[[i]] == model.dataset.rownames[[i+1]]), FALSE)
  if (! all(all.subsequent.rownames.the.same)) stop("anova.reml says: Each model has to have the same datset (at least the same rownames of the dataset)!")
  
  if (! omit.fitting.method) {
    model.fitting.methods.tmp <- unlist(lapply(list.of.models, fitting.method))
    model.fitting.methods <- factor(model.fitting.methods.tmp, levels=c("REML", "ML"))
    # --> levels=c("REML", "ML") leads to 1 -> REML, 2 -> ML, see output description later on.
  } else {
    model.fitting.methods <- rep(NA, length(list.of.models))
  }
  
  atab <- data.frame()
  # the first model
  m1 <- list.of.models[[1]]
  atab <- data.frame(Df = attr(logLik(m1), "df") ,
                     AIC = AIC(m1),
                     BIC = BIC(m1),
                     logLik = as.vector(logLik(m1)),
                     deviance = -2 * as.vector(logLik(m1)),
                     Chisq = NA,
                     ChiDf = NA,
                     PrChisq = NA ) # naming variables here already does not work ...
  colnames(atab) <- c("Df", "AIC", "BIC", "logLik", "deviance", # ... so, separately add 'colnames'!
                      "Chisq", "Chi Df", "Pr(>Chisq)")
  
  # loop through list.of.models
  # always compare two subsequent models to each other,
  # but only if they were both fit with the same method ML/REML
  for (m in 1:(length(list.of.models)-1)) {
    if (same.method(list.of.models[[m]], list.of.models[[m+1]]) %in% c("ML", NA))
    {
      # Two ML-models:
      # -----
      # 'stats::anova' does resort models depending on Df, which might
      # not be always desired, compute anova manually here:
      Dfs <- unlist(lapply(list.of.models[m:(m+1)], function(x) attr(logLik(x), "df") ))
      AICs <- unlist(lapply(list.of.models[m:(m+1)], AIC))
      BICs <- unlist(lapply(list.of.models[m:(m+1)], BIC))
      LL <- unlist(lapply(list.of.models[m:(m+1)], function(x) as.vector(logLik(x))))
      deviance <- -2*LL
      # Chisq and ChiDf are really computed in opposite directions
      Chisq <- deviance[1] - deviance[2]
      ChiDf <- Dfs[2] - Dfs[1]
      if (ChiDf >= 0) { # do LRT
        pChi <- 1 - pchisq(Chisq, ChiDf)
        atab.tmp <- data.frame(Dfs[2], AICs[2], BICs[2], LL[2], deviance[2], Chisq, ChiDf, pChi)
      } else { # ChiDf < 0; -> do NOT do LRT
        atab.tmp <- data.frame(Dfs[2], AICs[2], BICs[2], LL[2], deviance[2], NA, NA, NA)
      }
      colnames(atab.tmp) <- colnames(atab)
      atab <- rbind(atab, atab.tmp)
      
    } else
      if (same.method(list.of.models[[m]], list.of.models[[m+1]]) == "REML")
      { 
        # Two REML-models
        # -----
        # Alternatively, could simply use anova(..., refit = F) and then add convergence, etc.
        # Dfs <- unlist(lapply(list.of.models[m:(m+1)], function(m) extractAIC(m)[1]))
        # --> use 'logLik' to get Dfs. logLik does NOT re-fit the model with ML, whereas 'extractAIC' does!
        Dfs <- unlist(lapply(list.of.models[m:(m+1)], function(x) attr(logLik(x), "df") ))
        AICs <- unlist(lapply(list.of.models[m:(m+1)], AIC))
        BICs <- unlist(lapply(list.of.models[m:(m+1)], BIC))
        LL <- unlist(lapply(list.of.models[m:(m+1)], function(x) as.vector(logLik(x))))
        deviance <- -2*LL
        Chisq <- deviance[1] - deviance[2] # Chisq and ChiDf are really computed in opposite directions
        
        # taking a mixture distribution (as in Welch, West, Galecki) is generally WRONG!
        # ChiDf <- c(0, 1)
        # pChi <- 0.5 * (1-pchisq(Chisq,ChiDf[1])) + 0.5 * (1-pchisq(Chisq,ChiDf[2])) # see West, Welch, Galecki (2007), e.g. p. 154 - although that's not correct according to Crainiceanu et al. 2???
        # --> so, do a CONSERVATIVE TEST instead:
        ChiDf <- Dfs[2] - Dfs[1]
        if (ChiDf < 0 & suppressNegativeChiDf) {
          ChiDf <- NA
          Chisq <- NA # overwrite
        }
        # ... which is not correct for variance components, but
        #     at least the test is conservative (i.e. the p-value of the test is higher than it should be!)
        pChi <- 1 - pchisq(Chisq, ChiDf)
        
        atab.tmp <- data.frame(Dfs[2], AICs[2], BICs[2], LL[2],
                               deviance[2], Chisq, 
                               ChiDf,
                               pChi)
        colnames(atab.tmp) <- colnames(atab)
        atab <- rbind(atab, atab.tmp)
        
      } else
        if (! same.method(list.of.models[[m]], list.of.models[[m+1]]))
        {
          # not the same method
          #   indeed, take model m+1, since m+1 is the model that should show up in the next line!
          atab.tmp <- data.frame(Df = attr(logLik(list.of.models[[m+1]]), "df") ,
                                 AIC = AIC(list.of.models[[m+1]]),
                                 BIC = BIC(list.of.models[[m+1]]),
                                 logLik = as.vector(logLik(list.of.models[[m+1]])),
                                 deviance = -2 * as.vector(logLik(list.of.models[[m+1]])),
                                 Chisq = NA,
                                 ChiDf = NA,
                                 PrChisq = NA )
          colnames(atab.tmp) <- colnames(atab)
          atab <- rbind(atab, atab.tmp )
        } else
        {
          # Just a check
          stop("anova.reml says: This case should not be possible!")
        }
  } # end for
  
  
  if (useAICc) {
    # use correct AIC values (corrected for small sample sizes)
    names(atab)[names(atab) == "AIC"] <- "AICc"
    AICcs <- unlist(lapply(list.of.models, my.AICc))
    atab$AICc <- AICcs
  }
  
  # add fitting method (which was computed already above)
  atab <- cbind(meth = model.fitting.methods,
                atab)
  
  # add model.convergence
  conv.code.list <- lapply(list.of.models, function(m) m@optinfo$conv$lme4$code)
  conv.code.list.adj <- lapply(conv.code.list, function(x) ifelse(is.null(x), 0, x)) # 'adjusted' list of converged codes
  conv.mess.list <- lapply(list.of.models, function(m) m@optinfo$conv$lme4$messages)
  atab <- cbind(conv = unlist(conv.code.list.adj), atab)
  
  # adjust model names
  model.names <- paste("m", 1:length(list.of.models), sep="")
  rownames(atab) <- model.names
  
  # set class
  class(atab) <- c("anova", "data.frame")
  
  # add attribute "heading"
  dataset.name <- unique(model.dataset.names)
  model.formulas <- lapply(list.of.models, function(x) x@call$formula)
  heading <- c("Convergence 'conv':",
               "0 -> converged",
               "<0 -> error code",
               ">0 -> (probably) only warning",
               "Fitting method 'meth':",
               "1 -> REML",
               "2 -> ML",
               paste("Data:", dataset.name),
               "Models:",
               paste(model.names, model.formulas, sep=": "),
               "",
               "LR-test for variance components is (very) conservative!",
               "")
  attr(atab, "heading") <- heading
  
  return(atab)
}


fitting.method <- function(m) {
  if ( m@resp$REML == 0 )
    meth <- "ML" else
      meth <- "REML"
  return(meth)
}


same.method <- function(m1, m2) {
  # Does return the method, if both models were fit with the same method ("ML" or "REML"), 
  # otherwise FALSE
  
  if (class(m1) == "lmerMod" & class(m2) == "lmerMod") {
    # a model is ML, iff model@resp$REML == 0
    if (m1@resp$REML == 0 && m2@resp$REML == 0)
      result <- "ML" else
        if (m1@resp$REML > 0 && m2@resp$REML > 0)
          result <- "REML" else
            result <- FALSE
  } else{
    result <- NA
  }
  return(result)
}

my.AICc <- function(m) {
  # 
  # NOTE: my.AICc does yield the same values as MuMIn::AICc!
  # MuMIn supports lmer-models (see ?MuMIn::`MuMIn-models`).
  #
  AICval <- AIC(m) # get AIC value by function (without re-fitting a REML model with ML)
  LLobj <- logLik(m) # an object of class "loglik"
  k <- attr(LLobj, "df") # number of free parameters (degrees of freedom)
  n <- attr(LLobj, "nobs") # the number of observations
  LL <- LLobj[[1]] # just extract the numeric value for the loglikelihood from the object
  aicc <- - 2 * LL + 2 * k * (n / (n - (k+1)))
  names(aicc) <- "AICc"
  aicComputed <- - 2* LL + 2* k
  if (AICval != aicComputed) stop("my.AICc says: AICval and computed AIC should be the same!")
  attr(aicc, "LL") <- LL
  attr(aicc, "AIC") <- AICval
  attr(aicc, "k") <- k
  attr(aicc, "n") <- n
  return(aicc)
}

