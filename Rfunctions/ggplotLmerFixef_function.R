
# get.beta is required by ggplot.lmer.fixef
get.beta <- function(fixEff, charVect) {
  # Extracts the according beta for a character vector of predictor names.
  # The order of the predictor names in the character vector is irrelevant.
  perms <- gtools::permutations(length(charVect), length(charVect), charVect)
  for (i in 1:nrow(perms)) {
    beta <- fixEff[paste(perms[i,],collapse=":")] # analogously here
    if (!is.na(beta)) break
  }
  if (is.na(beta)) message(sprintf("Generated NA as beta for predictor %s!", paste(charVect, collapse=":")))
  return(beta)
}

# to plot just a random sample of individual trial data points
sample.prop <- function(dat, prop = 5, seed = 0) {
  # For example, prop = 5 will return a random _fifth_ of all observations in 'dat'.
  set.seed(seed)
  dat <- as.data.frame(dat)
  dat[sample(nrow(dat), round(nrow(dat) / prop) ),]
}



# the main function here
ggplot.lmer.fixef <- function(model,
                              fixef2plot = c(),
                              facets = NULL,
                              facets_labeller = ggplot2::label_value,
                              mappingAdd = NULL,
                              mappingAddForPoints = mappingAdd, # defaults to the same as 'mappingAdd'
                              xlab = NULL,
                              xlimits=NULL,
                              ylab="Response time trial n [ms]",
                              ylimits=NULL,
                              labsCommand = "",
                              noPoints=F,
                              noTitle=F,
                              pps=2.5, # plot point size
                              pls=.75, # plot line size
                              pts=14, # plot text size!!!
                              ebw=0,
                              ebp=NULL, # error bar position aesthetic, e.g. position_dodge(width = 0.1), without quotation marks
                              linecolour=NULL,
                              theme = theme_bw(base_size=16), # this is also some text size somehow!
                              dv.fnc = function(x) x, # default: no transformation
                              cp.fnc = function(x) x,
                              remallef = T, # remove all effects except the to-be-plotted ones
                              pointAlpha = 0.3,
                              pointShape = 19, # '19' is the default "point shape", a filled disc; use '1' for point without a fill!
                              pointColour = "black",
                              withErrorBar = F,
                              ErrorBarFactor = 1,
                              error.bar.lmer.df = "asymptotic",
                              fixef2plot.are.all.estimates = F,
                              sample.proportion = NULL,
                              addConditionalModes = NULL,
                              condModesAlpha = .75,
                              random.slopes.mm.pattern = 'm1.mm\\[, "|"\\]') {
  # Some limited explanations:
  #   pts ... plot text size
  #   factors ... needs to include _all_ factors' names, even the ones supplied to 'facetby'!
  #   facets ... is actually a string that is used as input to facet_grid, like "factorB ~ factorA"
  #   labsCommand ... a character string for modifying the labels of the plot which will be forwarded to ggplot2::labs(...)
  #   withErrorBar ... still experimental and works (probably) only for plotting effects of factors
  #   ErrorBarFactor ... the factor that the standard error of fixed effects is multiplied with.
  #   ebp ...  error bar position aesthetic, e.g. position_dodge(width = 0.1), without quotation marks
  #   mappingAdd ... lets you customize the ggplot-mapping for the independent predictors.
  #                  only the dv is unchangeable mapped to y-axis! Everything else can be customized with 'mappingAdd.'
  #                  specify 'mappingAdd' as a string, like 'facets'.
  #   mappingAddForPoints ... defaults to same as 'mappingAdd', but if specified defines the mapping for 
  #                           the single trial data plotted with the points.
  #   max.regline.resolution ... if TRUE, all factor level combinations will be combined with all
  #                              values of the continuous predictors to get maximum data points for the
  #                              regression line. If FALSE, only the value combinations of model.matrix(model)
  #                              will be evaluated for computing y-values of the regression line.
  #   cp.fnc ... function to be applied on the continuous predictor, currently only for one!
  #   linecolour ... colour of regression lines
  #   remallef ... TRUE per default, here!
  #   fixef2plot.are.all.estimates ...  name and function of this argument probably not that clear.
  #                   if set to TRUE, then all fixed effect estimates will simply be taken 
  #                   from fixef(model), without a check of which effects should actually be 
  #                   plotted according to 'fixef2plot'. If a model does contain not all
  #                   interactions that are in theory possible form the individual factors
  #                   this argument can be set to TRUE to plot all effects of the model
  #                   regardless of whether the pattern of interactions is complete.
  #                   one example model would be:
  #             y ~ A * B + some.model.matrix[, "Cc2-c1:Bb2-b1"] + ( A * B | subj)
  #                   this model includes an interaction with a third factor C for which the
  #                   main effect is not part of the fixed effects structure of the model,
  #                   which actually violates the principle of marginality, so this is not
  #                   a very useful example.
  #
  #   sample.proportion ... for the geom_point() part of the plot, draw a random 
  #                         sample of observations from the whole dataset for the plot.
  #
  #   error.bar.lmer.df ... determines how to calculate the confidence intervals for plots
  #                         involving only factors. possible values are:
  #                           "kenward-roger", "satterthwaite", and "asymptotic"
  #                         see documentation of the 'lmer.df' argument in the 'emmeans' package for details
  #                         and > vignette("models", "emmeans")
  #
  #   addConditionalModes ... to add conditions modes, i.e. the model predictions for individual participants
  #                         that's more or less adding individual participants data, however, given the 
  #                         model parameters (variance, etc.); see 'shrinkage'.
  #                         only works if there are no continuous predictors (at the moment).
  #                         a character string specifying the name of the random factor for
  #                         which conditional modes are to be illustrated.
  #
  # random.slopes.mm.pattern ... defaults to 'm1.mm\\[, "|"\\]', a regex pattern.
  #                         Some random effect/slopes names might have been created from a model.matrix
  #                         column (in order to restrict particular covariances to zero). Thus, the
  #                         the random slopes names in 'ranef(model)' can contain some prefix and/suffixes.
  #                         This argument specifies which pre/suffixes should be removed.
  # 
  # requires the 'remef' function from
  # http://read.psych.uni-potsdam.de/attachments/article/134/remef.v0.6.10.R
  
  if (any(class(model) %in% c("lm"))) {
    # 'fixef' will not work on object of type 'lm', so use function 'coef' instead
    fixef <- coef
    isLM <- T
  } else isLM <- F
  
  facetNames <- unlist(stringr::str_extract_all(facets, "[a-zA-Z0-9.]+"))
  # since "dot" could also be part of variable name consider it for str_extract, but remove single dot here:
  facetNames <- facetNames[facetNames != "."]  # in formuals, single dot denotes all available variables
  if (any(!(facetNames %in% fixef2plot))) {
    fixef2plot <- c(fixef2plot, facetNames[!(facetNames %in% fixef2plot)])
    message(paste("Because of facets, extending 'fixef2plot' to:", paste(fixef2plot, collapse=", ")))
  }
  
  # get data from model
  if (isLM) dat <- model$model else dat <- model@frame
  
  if (any(!(fixef2plot %in% names(dat)))) stop(sprintf("Variable %s not part of the model's data!\n", fixef2plot[!(fixef2plot %in% names(dat))] ))
  
  # ---- get info about to-be-plotted factors/predictors
  nEffs <- length(fixef2plot)
  fixefIsFactor <- vapply(fixef2plot, function(f) is.factor(dat[[f]]), logical(1))
  fixefLevels <- lapply(fixef2plot[fixefIsFactor], function(f) levels(dat[[f]]))
  names(fixefLevels) <- fixef2plot[fixefIsFactor]
  fixefNLevels <- lapply(fixef2plot, function(f) nlevels(dat[[f]]))
  names(fixefNLevels) <- fixef2plot
  fixefContrasts <- lapply(fixef2plot[fixefIsFactor], function(f) contrasts(dat[[f]]))
  names(fixefContrasts) <- fixef2plot[fixefIsFactor]
  
  # ---- get names of estimates, and the estimates themselves
  if ( fixef2plot.are.all.estimates ) { # this is rather a hack (see notes to this argument above)
    fixefEstimates <- fixef(model)
  } else { # plot effects selected by 'fixef2plot'
    allEstimates <- fixef(model)
    fixefEstimateNames <- list()
    for (fN in c(fixef2plot)) {
      fixefEstimateNames[[fN]] <- paste(fN, colnames(fixefContrasts[[fN]]), sep="")
    }
    coef.names <- c("(Intercept)",
                    unlist(fixefEstimateNames))
    if (length(fixefEstimateNames) > 1) {
      for (i in 2:(length(unlist(fixefEstimateNames))) ) {
        coef.names <- c(coef.names, 
                        apply(gtools::combinations(length(unlist(fixefEstimateNames)), i, unlist(fixefEstimateNames)), 
                              1, function(x) list(x)) )
      }
    }
    # remove 'coef.names' that contain more than one "sub-effect" of one factor, that pertains to factors with more than 2 levels!
    todelete <- logical(length(coef.names))
    for (i in length(coef.names):1) { # from back to front ;-)
      for (fes in fixefEstimateNames) {
        if (sum(fes %in% coef.names[[i]][[1]]) > 1) todelete[i] <- TRUE
      }
    }
    coef.names[todelete] <- NULL
    fixefEstimates <- unlist(lapply(unname(coef.names), function(x) get.beta(allEstimates, x[[1]])))
  }
  
  # ---- create dataset for plot
  # compute to-be-plotted values from estimates and modelmatrix (which contains all contrasts and predictor values)
  
  # only take requested fixed effects, also from model.matrix only extract requested ones.
  dat$dvm <- as.vector(fixefEstimates %*% t( model.matrix(model)[,names(fixefEstimates)] )) 
  # dvm ... dv according to model
  
  # transform the dv
  dat$dvmt <- dv.fnc(dat$dvm)
  
  # compute dv also from remef, in case there is at least one continuous predictor (for plotting single data points)
  # this appears to be more convenient than calculating from model parameters directly
  if (any(!fixefIsFactor)) {
    if (remallef) { # remove all effects except the to-be-plotted ones (their names are in fixefEstimateNames)
      if (!isLM) {
        # however, always plot intercept!
        dat$dvremef <- remef(model, fix = names(fixefEstimates), ran = NULL, keep = T, plot=F, family = family(model))
        # NOTE: names(fixefEstimates) != fixefEstimateNames !!!
      } else { # isLM
        dat$dvremef <- model.response(model.frame(model)) # the original 'dv' as stored in the data used for the model
      }
    } else { # remove only random effects
      if (!isLM) {
        ranefModel <- ranef(model)
        dat$dvremef <- remef(model, fix = NULL, ran = lapply(ranefModel, ncol), keep = F, plot=F)
      } else { # isLM
        dat$dvremef <- model.response(model.frame(model)) # the original 'dv' as stored in the data used for the model
      }
    }
    # transform accordingly
    dat$dvremeft <- dv.fnc(dat$dvremef)
  }
  
  # There might be some redundancy in the dataset (due to same value for each random-factor level), 
  #   so reduce the values in order to not plot each datapoint more than once:
  datr <- unique(dat[c(fixef2plot, "dvmt")])
  
  
  if (!is.null(addConditionalModes)) {
    
    if (! all(fixefIsFactor)) stop("Addition conditional modes only works for factors, but not (yet) if continuous predictors are to be plotted as well!")
    
    if (! addConditionalModes %in% names(ranef(model))) stop(sprintf("Requested condition modes for factor '%s' cannot be found in the model!", addConditionalModes))
    
    # get the conditional modes (cm), i.e. quasi the individual participants data given the model parameters
    fe.names <- names(fixef(model)) # in fixef names there is nothing to replace
    re.names <- colnames(ranef(model)[[addConditionalModes]])
    re.names.repl <- unlist(lapply(re.names, function(x) gsub(random.slopes.mm.pattern, "", x)))
    # the 'each' in rep(..., each = ) is crucial!
    delu.names <- rep(re.names, each = nrow(ranef(model)[[addConditionalModes]]) )
    delu.names.repl <- unlist(lapply(delu.names, function(x) gsub(random.slopes.mm.pattern, "", x)))
    
    # fixed effects
    delb <- matrix(model@pp$delb, nrow = 1) # == fixef(model)
    Xt <- t(model@pp$X)
    bidx <- fe.names %in% names(fixefEstimates)
    
    # random effects
    delu <- matrix(model@pp$delu, nrow = 1)
    Zt <- as(model@pp$Zt, "matrix")
    uidx <- delu.names.repl %in% names(fixefEstimates)
    
    # create a dataset with conditional mode levels, factors, and dv with 
    # the to-be-plotted fixed and random effects
    datcm.all <- dat[, c(addConditionalModes, fixef2plot)]
    # fixed + random is the model prediction including conditional modes
    datcm.all$dvm <- as.vector( delb[,bidx] %*% Xt[bidx,] + delu[,uidx] %*% Zt[uidx,] )
    datcm.all$dvmt <- dv.fnc(datcm.all$dvm) # transform the dv
    datcm <- unique(datcm.all) # get only unique rows
    
  } else {
    datcm <- NULL
  }
  
  
  # ---- create datset for plotting error bars
  
  if (withErrorBar) {
    
    # Note:
    # Perhaps with 'emmeans' we could get CI/stderr also around regression lines? Keep that for later...
    # ~~~~~~~~~~
    # We want to get means given that continuous predictors are 0, and not, as by default, mean(cont.pred)!
    # get name of continuous predictor, even if it was not part of 'fixef2plot'
    # Note, model@frame might also include separate variables used for random slopes, therefore rely on attr(terms(...), "variables")
    var.names.all <- as.character(attr(terms(model), "variables"))
    var.names <- var.names.all[-1:-2] # the first two are litterally "list" (don't know why) and "the dependent variable"
    # check which of the variables is "numeric"
    var.class <- unlist(lapply(var.names, function(x) class(model@frame[[x]])))
    var.is.cont <- var.class %in% c("numeric", "double")
    # create a list of "0" with variable names the names of the numeric predictors
    at.list <- as.list(rep(0, sum(var.is.cont)))
    names(at.list) <- var.names[var.is.cont]
    # ~~~~~~~~~~~~~
    # run 'emmeans'
    # ~~~~~~~~~~~~~
    ci.names <- switch(family(model)$family,
                       gaussian = c("lower.CL", "upper.CL"),
                       binomial = c("asymp.LCL", "asymp.UCL"),
                       stop(sprintf("ci names for model family '%s' not defined!", family(model)$family)))
    emm <- tryCatch(emmeans::emmeans(model, fixef2plot, at = at.list, lmer.df = error.bar.lmer.df),
                    error = function(e) stop("The dataset used to to create the model is required in the GlobalEnv. Load it and try again! If the error persists, something else is wrong..."))
    # NOTE: only the 'summary(emm)' can be converted to a data.frame!
    emm.tab <- as.data.frame(summary(emm))
    if (is.null(emm.tab[[ci.names[1]]])) {
      # There was (very likely) this note that "D.f. calculations have been disabled...",
      # meaning that the confidence intervals requiring D.f. are not available.
      # So we simply select the asymptotic confidence intervals, which are available instead.
      ci.names <- c("asymp.LCL", "asymp.UCL")
    }
    emm.tab$emmeanT <- dv.fnc(emm.tab$emmean) # possibly transform values back to original scale
    emm.tab$dvErrLow <- emm.tab[[ci.names[1]]]
    emm.tab$dvErrUpp <- emm.tab[[ci.names[2]]]
    emm.tab$dvErrLowT <- dv.fnc(emm.tab$dvErrLow)
    emm.tab$dvErrUppT <- dv.fnc(emm.tab$dvErrUpp)
    # add everything to 'datr', also the column 'emmean' to enable comparison between what emmeans provides
    # and what the in-house method within this function does
    datr <- merge(datr, emm.tab[,c(fixef2plot, "emmean", "emmeanT", "dvErrLow", "dvErrUpp", "dvErrLowT", "dvErrUppT")]) # add the error bar data to the dataset that will be used for plotting.
    if (any(datr$dvmt != datr$emmeanT)) {
      # only warn if the difference is not negligible, i.e. greater than 1e-12 (so, not likey due to rounding error)
      if (max(abs(datr$dvmt - datr$emmeanT)) > 1e-12) {
        cat("Difference in marginal means between 'emmean' and 'in-house' method ('dvmt') is > 1e-12.\n")
        cat("Possibly due to rounding error, check this table:\n")
        print(datr)
        warning("Check difference in marginal means!")
      }
    }
  }
  
  # ---- actually plot the data
  if (any(!fixefIsFactor)) { # at least one continuous predictor...
    rpd <- datr # the reduced data is actually the data for the regression lines: rpd ... regression plot data.
    if (!is.null(xlimits)) {rpd <- rpd[rpd[fixef2plot[!fixefIsFactor]] > xlimits[1] &
                                        rpd[fixef2plot[!fixefIsFactor]] < xlimits[2], ]
                            if (nrow(rpd) == 0) stop("ggplot.lmer.fixef says: no observations outside 'xlimits'!")}
    if (!is.null(ylimits)) {rpd <- rpd[rpd[fixef2plot[!fixefIsFactor]] > ylimits[1] &
                                        rpd[fixef2plot[!fixefIsFactor]] < ylimits[2], ]
                            if (nrow(rpd) == 0) stop("ggplot.lmer.fixef says: no observations outside 'ylimits'!")}
    
    # as "plot data" (pdat), we want original single trial data, with random effects removed, or not (see above)
    if (!is.null(sample.proportion)) { # only sample a proportion of this dataset for the plot
      stopifnot(is.numeric(sample.proportion)) # make sure it's numeric
      pdat <- sample.prop(dat, sample.proportion) # draw random sample
    } else pdat <- dat
    
    if (sum(!fixefIsFactor)==1) { # ---------- 1 continuous predictor ---------------
      contpredName <- fixef2plot[!fixefIsFactor]
      # apply function to continuous predictor on regression line data and on data for individual points
      rpd[contpredName] <- cp.fnc(rpd[contpredName])
      pdat[contpredName] <- cp.fnc(pdat[contpredName])
      if (is.null(mappingAdd)) {
        factors2plot <- fixef2plot[fixefIsFactor]
        if (length(factors2plot) > 0) { # at least one factor in addition to continuous predictor
          mappingCom <- switch(length(factors2plot),
                               paste("aes(x=`", contpredName, "`, linetype=`", factors2plot[1], "`, 
                                   y=dvremeft)", sep = ""),
                               paste("aes(x=`", contpredName, "`, linetype=`", factors2plot[1], "`, 
                                   group=interaction(`", factors2plot[1], "`,`", factors2plot[2], "`), 
                                   colour=`", factors2plot[2], "`, y=dvremeft)", sep = ""),
                               paste("aes(x=`", contpredName, "`, linetype=`", factors2plot[1], "`, 
                                   group=interaction(`", factors2plot[1], "`,`", factors2plot[2], "`), 
                                   colour=`", factors2plot[2], "`, y=dvremeft)", sep = ""),
                               NA) # option 2 and 3 are identical! that necessary!
        } else { # only one continuous predictor and no factors
          mappingCom <- paste("aes(x=`", contpredName, "`, y=dvremeft)", sep = "")
        }
        if (is.null(mappingAddForPoints)) mappingComForPoints <- mappingCom else
          mappingComForPoints <- paste("aes(y=dvremeft,", mappingAddForPoints, ")", sep = "")
        if (length(factors2plot)==3 & is.null(facets)) facets <- paste(". ~ ", factors2plot[3], sep="")
        
      } else { # customized mapping
        mappingCom <- paste("aes(y=dvremeft,", mappingAdd, ")", sep = "")
        if (mappingAdd == mappingAddForPoints) mappingComForPoints <- mappingCom else
          mappingComForPoints <- paste("aes(y=dvremeft,", mappingAddForPoints, ")")
      }
      
      mapping <- eval(parse(text=mappingCom))
      mappingForPoints <- eval(parse(text=mappingComForPoints))
      
      if (is.null(xlab)) xlab <- contpredName
      p0 <- ggplot(pdat, mappingForPoints) # 'mapping(ForPoints)' is a variable and can also be supplied as input argument!
      p1 <- p0 + 
        labs(x=xlab,
             y=ylab) + 
        coord_cartesian(xlim = xlimits, ylim = ylimits) + 
        theme + theme(text=element_text(size=pts)) # here, 'theme' exists as variable and as function ;-)
      if (!noPoints) {
        # plot points, i.e. individual trial model predictions
        if (family(model)$family == "binomial") {
          warning("The model is binomial. Are you sure you want to plot individual trials predictions? It's not entirely clear yet what they mean.")
        }
        if (!is.null(mappingAddForPoints)) {
          if (grepl("shape ?=", mappingAddForPoints)) # not to overwrite the 'shape' definition from the mapping
            p1 <- p1 + geom_point(size=pps, alpha = pointAlpha) else
              p1 <- p1 + geom_point(size=pps, alpha = pointAlpha, shape = pointShape, colour = pointColour)
        } else {
          p1 <- p1 + geom_point(size=pps, alpha = pointAlpha, shape = pointShape, colour = pointColour)  
        }
      }
      
      # different dv for regression lines, so create separate 'mappingRPD' (RPD...regression plot data)
      if (is.null(mappingAdd)) {
        mappingRPD <- eval(parse(text = gsub("dvremeft", "dvmt", mappingCom)))
      } else {
        com <- paste("aes(y=dvmt,", mappingAdd, ")")
        mappingRPD <- eval(parse(text=com))
      }
      if (!is.null(linecolour)) {
        p2 <- p1 + geom_line(mapping=mappingRPD, data=rpd, size=pls, colour=linecolour)
      } else {
        p2 <- p1 + geom_line(mapping=mappingRPD, data=rpd, size=pls)
      }
      
    } else { # ... 2 continuous predictors
      if (sum(!fixefIsFactor) > 2) stop("Not more then two continuous predictors possible at the moment!")
    }
    
  } else {
    # ----------- no continuous predictor, only factors --------------
    pdat <- datr # in this case, the reduced data is already the complete plot data: pdat.
    # check, if exactly one value per factor-level combination
    nFactorLevelCombs <- nrow(expand.grid(lapply(dat[fixef2plot], unique)))
    if (nFactorLevelCombs != nrow(pdat)) stop("ggplot.lmer.fixef says: Computing to-be-plotted values from estimates and model.matrix, or checking number of factor-level combinations did not work!")
    # plot
    factors2plot <- fixef2plot[fixefIsFactor]
    if (is.null(mappingAdd)) {
      if (is.null(addConditionalModes)) {
        com <- switch(length(factors2plot),
                      paste("aes(x=", factors2plot[1], ", linetype=", factors2plot[1], ", group=", factors2plot[1], ", y=dvmt)"),
                      paste("aes(x=", factors2plot[2], ", linetype=", factors2plot[1], ", group=", factors2plot[1], ", y=dvmt)"),
                      paste("aes(x=", factors2plot[2], ", linetype=", factors2plot[1], ", group=", factors2plot[1], ", y=dvmt)"),
                      NA) # option 2 and 3 are identical! that necessary!
      } else {
        # will add conditional modes below; here we prepare the aesthetics mapping
        com <- switch(length(factors2plot),
                      paste("aes(x=", factors2plot[1], ", linetype=", factors2plot[1], ", group=", factors2plot[1], ", y=dvmt)"),
                      paste("aes(                         linetype=", factors2plot[1], ", group=", factors2plot[1], ", y=dvmt"),
                      NA)      # factor 2 goes into facets, see below.
      }
      mapping <- eval(parse(text=com))
      if (length(factors2plot)==3) facets <- paste(". ~ ", factors2plot[3], sep="")
    } else {
      com <- paste("aes(y=dvmt,", mappingAdd, ")")
      mapping <- eval(parse(text=com))
    }
    if (is.null(xlab) & is.null(mappingAdd)) xlab <- factors2plot[2]
    p0 <- ggplot(pdat, mapping) # 'mapping' is a variable and can also be supplied as input argument!
    p1 <- p0 + 
      labs(x=xlab,
           y=ylab) + 
      coord_cartesian(xlim = xlimits, ylim = ylimits) + 
      theme + theme(text=element_text(size=pts)) # here, 'theme' exists as variable and as function ;-)
    if (is.null(ebp))
      p2 <- p1 + geom_point(size=pps) + geom_line(size=pls) else
        p2 <- p1 + geom_point(size=pps, position = ebp) + geom_line(size=pls, position = ebp)
    # add error bars
    if (withErrorBar) if (is.null(ebp))
      p2 <- p2 + geom_errorbar(mapping = aes(ymin=dvErrLowT, ymax=dvErrUppT),
                               size=pls, width=ebw) else
                                 p2 <- p2 + geom_errorbar(mapping = aes(ymin=dvErrLowT, ymax=dvErrUppT),
                                                          size=pls, width=ebw, position=ebp)
    # if "dodged" position, we remove the vertical lines in the plot
    # (we want one 'if' evaluated after the other and don't continue evaluation in case one 'if' is FALSE)
    if (any(class(ebp)=="PositionDodge"))
      if (!is.null(ebp$width))
        if ((ebp$width != 0))
          p2 <- p2 + theme(panel.grid.major.x = element_blank(),
                           axis.ticks.x = element_blank())
    
    if (!is.null(addConditionalModes)) {
      
      if (length(fixef2plot) == 2) {
        # we need facets, otherwise plotting individual data is too much limited by how the 
        # aesthetics mapping works (here, done very ugly)
        # here, also take the 'facets_labeller' from the standard arguments, which is probably
        # confusing.
        p2 <- p2 + facet_grid(cols = eval(parse(text = sprintf("vars(%s)", factors2plot[2]))),
                              labeller = facets_labeller)
      } else {
        if (length(fixef2plot) > 2) {
          stop("Plot design with conditional modes for more than 2 factors not yet implemented!")
        }
      }
      
      # add the aesthetics for the random factor (for which conditional modes are requested)
      # which means: define the 'group'
      comCondModes <- paste(substr(com, 1, nchar(com)-1), 
                            sprintf(", group = %s)", addConditionalModes),
                            sep = "")
      mappingCondModes <- eval(parse(text = comCondModes))
      # conditions modes are plotted with smaller symbols, line sizes, etc. than the means
      plsCondModes <- pls / 3
      ppsCondModes <- pps / 3
      # pre-generating geom_point and adding to p2 doesn't work, so have to do it like that:
      if (is.null(ebp)) {
        p2 <- p2 + geom_point(mapping = mappingCondModes, data = datcm, 
                              size = ppsCondModes,
                              alpha = condModesAlpha)
      } else {
        # if error bars get a specific 'position', do the same with conditional modes as well
        positionCondModes <- position_dodge(width = ebp$width) # or a different width?
        p2 <- p2 + geom_point(mapping = mappingCondModes, data = datcm, 
                              size = ppsCondModes,
                              position = positionCondModes,
                              alpha = condModesAlpha)
        # for the conditional mode lines, get rid of the 'linetype' in the aesthetics
        comCondModesLines <- gsub("linetype\\s*=\\s*\\w*\\s*,", "", comCondModes)
        mappingCondModesLines <- eval(parse(text=comCondModesLines))
        p2 <- p2 + geom_line(inherit.aes = F,
                             mapping = mappingCondModesLines,
                             data = datcm,
                             size = plsCondModes,
                             alpha = condModesAlpha,
                             position = positionCondModes)
      }
      
    }
    
  }
  # ---- end separate plotting section
  
  if (!is.null(facets)) p2 <- eval(parse(text=paste("p2 + facet_grid(", facets, ", labeller=facets_labeller)")))
  if (length(labsCommand)>1) stop("'labsCommand' must only be one string!")
  if (nchar(labsCommand)>0) p2 <- eval(parse(text=paste("p2 + labs(", labsCommand, ")")))
  
  # extract model name from function call and add as title to the plot
  call <- as.character(sys.call())
  model.name <- call[2]
  if (!noTitle & !any(grep("title", names(p2$labels))))
    pFinal <- p2 + labs(title=model.name) else pFinal <- p2
  
  # add the regression-plot data to the output as attribute, if there is any continuous predictor!
  if (any(!fixefIsFactor)) attr(pFinal, "rpd") <- rpd
  
  return(pFinal)
}

