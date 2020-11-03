
stargazer.anova <- function(...,
                            out = NULL,
                            table.placement="H",
                            type = "html",
                            table.layout = "=!ld#-!t-!s=!n", # might not work with different layout! 'n' adds notes in the end, if there is input to "notes="!
                            ci = F,
                            ci.custom = NULL,
                            p = NULL, # forwarded to stargazer()
                            t = NULL,
                            t.auto = F, # FALSE ... stargazer will use model's default values, if argument "t" is NULL
                            star.cutoffs = NULL, # c(0.05), # only 0.05 would be required for stars based on bootstrapped cis
                            report = "vcs",
                            keep.stat = c("n"),
                            notes = "", 
                            notes.label = "",
                            notes.append = F, # required in order for 'notes' to replace the default note!
                            anova.round.digits = 3,
                            precise.p.value = T,
                            p.value.precision = anova.round.digits,
                            without.anova = F,
                            enableREML = T,
                            omit.fitting.method = F,
                            ranef.header = F,
                            useAICc = T,
                            column.separate = NULL,
                            column.labels = NULL,
                            model.numbers = NULL,
                            random.effects.table = NULL,
                            random.factor.names = NULL,
                            random.slope.names = NULL,
                            fixed.effect.names = NULL,
                            suppressNegativeChiDf = F,
                            denoteCorrelationHTML = "<sup>r</sup>") {  
  # Attention: This function is quite a hack.
  # In particular the way it inserts custom-made parts into a standard 'stargazer' output table is 
  # very hacky.
  #
  # This function creates a model comparison table with fixed and random effect variance components.
  # Note that it does not display the correlations among random effects.
  # The table printed to command line does NOT contain the anova comparison.
  # The anova comparison is only available in the output argument, and in the output file that is 
  # created by this function.
  #
  # The 'without.anova' options allows creating a table with same standard options but without the 
  # actual ANOVA comparison, but with AIC(c) values. This functionality could be useful for non-nested
  # models!
  #
  # precise.p.value ... logical, determines whether p-values should be printed precisely.
  #                     nevertheless "<" is used for values smaller than specified by p.value.precision
  # p.value.prevision ... the number of post-comma digits to be printed for p-values (leading 0 is ommited anyway)
  # ranef.header ... include the words "Random effects structure" in the header just before the "column.labels"
  # random.effects.table ... (very hacky) either NULL, indicating do NOT print random effects, or
  #                          an integer, indicating where to insert the random effects part of the table.
  #                          The appropriate value has to be determined by trial and error!
  # random.factor.names ... named character vector
  #                         the names(random.factor.names) are the original names and the elements of 
  #                         random.factor.names are the new names that should be used in the table.
  #                         variance components will be grouped together in the table of unique 
  #                         'random.factor.names' and not 'names(random.factor.names)', which means
  #                         that e.g. names like "participant.1" and "participant.2" can be grouped 
  #                         together as "Participant.". That's useful if a model does _not_ contain
  #                         all correlations between random slopes of the same random factor.
  # random.slope.names .... can be a named vector or an unnamed vector of random slope names.
  #                         if named, the names have to correspond to the original random slope names!
  # denoteCorrelationHTML ... a string (one-element character vector) which should be inserted 
  #                         after each random slope variance for which a correlation to other 
  #                         slopes had been estimated.
  # fixed.effect.names ... a named character vector of regular expressions that will be executed
  #                        with 'gsub' on the result of running 'stargazer' for the first time within
  #                        this function.
  #                        This has the effect of replacing the name of an element of 'fixed.effect.names'
  #                        with its contents, therefore e.g. changing the names of the fixed effects.
  #                        Other changes to the table are of course possible as well with this argument.
  #
  # NOTE ABOUT CONVERGENCE PROBLEMS
  #   A dagger sign in the last line after the p-value line indicates convergence problems for the
  #   corresponding model (the model in the column of the dagger sign).
  #   If there are no problems, the p-value line is the last line.
  #
  # NOTE ON TABLE LAYOUT
  #   default table layout copied from "stargazer-internal.R" source code:
  #     c("=!","dependent variable label","dependent variables","models","columns","numbers","objects","-","coefficients","-","omit","-","additional","N","R-squared","adjusted R-squared","max R-squared","log likelihood","sigma2","theta(se)*", "AIC","BIC","UBRE","rho(se)*","Mills(se)*", "SER(df)","F statistic(df)*","chi2(df)*","Wald(df)*","LR(df)*","logrank(df)*","=!","notes")
  #   for the abbreviations required in argument table.layout, see
  #   ?stargazer_table_layour_characters
  #   Table layout design in this function is quite hacky. Any changes to input argument 'table.layout' 
  #   might actually not work.
  
  # A function that returns which method REML or ML has been used to fit model 'm'.
  fitting.method <- function(m) {
    if ( m@resp$REML == 0 )
      meth <- "ML" else
        meth <- "REML"
      return(meth)
  }
  
  rm.leading.zero <- function(val, precision=3) {
    # ATTENTION: This function does also convert numeric to character!
    format <- sprintf("%%.%df", precision)
    sub("^(-?)0\\.", "\\1.", sprintf(format, val))
  }
  
  if (length(notes) > 1) warning("stargazer.anova says: 'notes' with more than one character vector element does probably not work!")
  
  if (without.anova) keep.stat <- c(keep.stat, "aic") # add AIC, since that would not be displayed for the 'without.anova'-version
  
  if (!is.null(fixed.effect.names))
    if (!is.null(names(fixed.effect.names)))
      if (any(names(fixed.effect.names) == ""))
        stop("stargazer.anova says: all elements of 'fixed.effect.names' have to be named!")
  
  # ---- create stargazer model table ----
  if (!is.null(model.numbers) & !is.logical(model.numbers)) {
    model.numbers.stargazer <- TRUE
  } else {
    model.numbers.stargazer <- model.numbers # in the "else-case": model.numbers stargazer can be NULL, TRUE, FALSE
  }
  
  # remove model numbers (#) and mandatory preceding horizontal line (-!) from 'table.layout'
  if (!is.null(model.numbers.stargazer)) {
    if (! model.numbers.stargazer)
      table.layout <- gsub("-!#", "", table.layout)
  }
  
  # get all models, to enable access to fitting method and other stuff
  list.of.... <- as.list(list(...))
  if (! is.null(names(list.of....)))
    # Trick: models do not have names, in contrast to specified parameters, 
    # but only if there are additional input arguments:
    list.of.models <- list.of....[names(list.of....) == ""] else
      # if there are no additional input arguments, the names of ... are NULL
      list.of.models <- list.of....
  nmod <- length(list.of.models) # number of models
  
  if (!is.logical(model.numbers) & !is.null(model.numbers))
    if (length(model.numbers) < nmod) {
      model.numbers.from <- model.numbers[length(model.numbers)]+1
      model.numbers.to <- model.numbers[length(model.numbers)] + (nmod-length(model.numbers))
      model.numbers <- c(model.numbers, seq(model.numbers.from, by=1, to=model.numbers.to) )
      warning(sprintf("Too few model numbers - filling them up with numbers %d to %d to have as many as there are models!",
                      model.numbers.from, model.numbers.to))
    } else {
      if (length(model.numbers) > nmod) stop("Too many model numbers!")
    }
  
  # call the real stargazer() to get the template
  sg <- stargazer(...,
                  out = out,
                  table.layout = table.layout,
                  type = type, # the TYPE argument in addition to OUT as html is NECESSARY!
                  ci = ci, # T means: in general print confidence intervals instead of std.err
                  ci.custom = ci.custom,
                  p = p, # could be no real p-value!
                  t = t,
                  t.auto = t.auto,
                  star.cutoffs = star.cutoffs,
                  report = report, # or "vcst*"? show (v)ariable names, (c)oefficients, (s)tandard errors/confidence intervals, maybe (t)-value, and (*) stars for stargazer-internally computed significance
                  # apply.p = function(p) 1, # set all p-values to 1, so there are NO 'stars'
                  keep.stat = keep.stat, # btw, chi2 and lr do NOT work
                  table.placement="H",
                  column.labels = column.labels,
                  column.separate = column.separate,
                  notes.label = notes.label,
                  notes.append = notes.append,
                  notes = notes,
                  model.numbers = model.numbers.stargazer
  )
  
  # adjust names of fixed effects
  if (!is.null(fixed.effect.names)) {
    # have to loop in opposite direction to first replace most complicated expressions, which usually works
    for (i in length(fixed.effect.names):1) {
      sg <- gsub(names(fixed.effect.names[i]), fixed.effect.names[i], sg)
    }
  }
  
  # adjust header
  sg[c(3,4)] <- sg[c(4,3)] # swap horizontal line and DV description
  
  # --- print random effects structure head line to header
  idx <- 4 # index to element in 'sg' after which according header line should be inserted
  if (ranef.header) {
    sg <- append(sg,
                 sprintf("<tr><td style=\"text-align:left\"></td><td colspan=\"%d\"><em>Random effects structure:</em></td></tr>", nmod),
                 after = idx)
    # remove horizontal line in the beginning of next element of 'sg'
    sg[idx+2] <- sub("(<tr>.*?</tr>)", "", sg[idx+2]) # 'sub' replaces only the FIRST match!
    # create hline for each set of columns with same random effects, as indicated in 'column.separate'
    ranef.hline <- paste("<tr><td></td>", # column 1: contains only fixed effect labels
                         paste(sprintf("<td colspan=\"%d\" style=\"border-bottom: 1px solid black\"> </td>", column.separate), # 1px solid black
                               sep="", collapse=""),
                         sep="", collapse="")
    sg <- append(sg,
                 ranef.hline,
                 after = idx+2)
  }
  
  # --- print fitting method to header
  # define html-horizontal line that spans only across fixed effects columns (i.e. all columns except for the left-most column)
  hline.colspan <- sprintf("<tr><td></td><td colspan=\"%d\" style=\"border-bottom: 1px solid black\"></td></tr>", nmod)
  # add fitting method to the header
  if (! omit.fitting.method) {
    if (ranef.header) idx <- idx + 3
    fms <- unlist(lapply(list.of.models, fitting.method)) # fms ... fitting methods
    fms.line1 <- sprintf("<tr><td style=\"text-align:left\"></td><td colspan=\"%d\"><em>Fitting method:</em></td></tr>", nmod)
    fms.line2 <- paste("<tr><td style=\"text-align:left\"></td><td>", paste(fms, "</td><td>", sep="", collapse=""), "</td></tr>")
    sg <- append(sg,
                 c(fms.line1, fms.line2, hline.colspan),
                 after = idx)
  }
  
  
  if (!without.anova) { # add anova table
    
    # warn, since adding anova table when there are notes might fail
    if (nchar(notes) > 0 || nchar(notes.label) > 0) warning("stargazer.anova says: adding anova table when there is a note does probably NOT work!")
    
    
    if (!enableREML) {
      # ----- anova table -------
      # NOTE: always compare two subsequent model, to get the same order in the model table as there is for the stargazer table
      # extract models from input argument
      # atab <- anova(...) # additional arguments which aren't models are simply NOT evaluated inside anova, that's nice!
      # we need more than 1 model...
      if (length(list.of.models) <= 1) {stop("stargazer.anova says: More than one model is required to do model comparison!")}
      # ... compare the first two models ...
      atab <- anova(list.of.models[[1]], list.of.models[[2]])
      if (length(list.of.models) > 2) { # ... compared further models...
        for (m in 2:(length(list.of.models)-1)) { # for models 2 to one-but-last, compare it to the following model
          atab.tmp <- anova(list.of.models[[m]], list.of.models[[m+1]])
          atab <- rbind(atab, atab.tmp["list.of.models[[m + 1]]",] ) # in anova, models are sorted according to Df, so select the second one.
          # there will be empty Chisq, and p-value fields, if a model that is higher in Df occurs before a model that is lower in Df.
          # That indicates some error.
        }
      }
      atab.df <- as.data.frame(atab) # does not have original model names as rownames, but "..#number"
      rownames(atab.df) <- 1:nrow(atab.df) # simply use plain number, without ".."
      
    } else  { 
      # enable REML
      atab <- anova.reml(..., useAICc = useAICc, suppressNegativeChiDf = suppressNegativeChiDf, omit.fitting.method = omit.fitting.method)
      atab.df <- as.data.frame(atab)
    }
    
    # model convergence
    if (enableREML) {
      mod.conv <- atab$conv
    } else {
      conv.code.list <- lapply(list.of.models, function(m) m@optinfo$conv$lme4$code)
      conv.code.list.adj <- lapply(conv.code.list, function(x) ifelse(is.null(x), 0, x))
      mod.conv = unlist(conv.code.list.adj)
    }
    # --> could lme4$code have more than one element?
    
    # only select certain parameters
    anova.param.names.orig <- rownames(atab.df) # parameter names, like "AIC", etc.
    if (useAICc) aicName <- "AICc" else aicName <- "AIC"
    atab.df.selected <- atab.df[, c(aicName, "logLik", "deviance", "Df", "Chisq", "Chi Df", "Pr(>Chisq)")]
    anova.param.names.formatted <- c(aicName, "Log Likelihood", "Deviance", "Df", "<em>&chi;<sup>2</sup></em>",
                                     "<em>&chi;<sup>2</sup></em> Df", "<em>p</em>")
    
    # during formatting, the anova table is TRANSPOSED to result in one model per column!
    anova.part <- c()
    for (c in 1:ncol(atab.df.selected)) { # columns == types of selected parameter, e.g. AIC
      line <- paste("<tr><td style=\"text-align:left\">", anova.param.names.formatted[c], "</td>", sep="") # name of the value/parameter
      for (r in 1:nrow(atab.df.selected)) { # rows == number of models, i.e. number of values for each parameter
        if (c < ncol(atab.df.selected)) { # for all except the last column (p-value)
          if (!is.na(atab.df.selected[r,c])) {
            line <- paste(line, "<td>", round(atab.df.selected[r,c],anova.round.digits), "</td>", sep=" ") # NOTE: If there are only zeros after comma, they are not shown after rounding!
          } else {
            line <- paste(line, "<td></td>", sep=" ") # if NA -> empty field
          }
        } else { # the p-value line, i.e. the last row
          p <- atab.df.selected[r,c]
          if (!is.na(p)) {
            if (precise.p.value) { # precise p-value
              p.threshold.val <- 10^(-p.value.precision)
              if (p < p.threshold.val) {
                p <- paste("<", rm.leading.zero(p.threshold.val, p.value.precision) ) # rm.leading.zero does convert from numeric to character!
              } else {
                p <- rm.leading.zero(p, p.value.precision)
              }
            } else { # imprecise p-value
              if (p < 0.001) p <- "< .001" else
                if (p < 0.01) p <- "< .01" else
                  if (p < 0.05) p <- "< .05" else {
                    p <- rm.leading.zero(p, p.value.precision) # if "< .05" is possible, at max 2 post-comma digits make sense!
                  }
            } # p-value formatting finished
            line <- paste(line, "<td>", p, "</td>", sep=" ")
          } else { # p-value is NA
            line <- paste(line, "<td></td>", sep=" ") # if NA -> empty field
          }
        }
      }
      line <- paste(line, "</tr>", sep="")
      anova.part <- c(anova.part, line)
    }
    
    # append anova table right before the last horizontal line
    # look for the last horizontal line
    horzline.pattern <- "<tr><td .* style=\\\"border-bottom: \\d+px solid black\\\"></td></tr>"
    sg.horzlines.idx <- grepl(horzline.pattern, sg)
    sg.horzline.last.idx <- which(sg.horzlines.idx)[length(which(sg.horzlines.idx))]
    # ---> would have to split the elements of 'sg', to obtain a separate line element.
    sg.horzline.last <- sg[sg.horzline.last.idx]
    suffix <- "</td></tr>"
    prefix <- "<tr><td" # sic: without ">" in the end!
    tmp <- strsplit(sg.horzline.last, paste(suffix, prefix, sep=""))
    if (length(tmp[[1]]) > 1) {
      # this step is actually NOT necessary!
      sg.horzline.lastA <- paste(tmp[[1]][1], suffix, sep="")
      sg.horzline.lastB <- paste(prefix, tmp[[1]][2], sep="")
      sg[sg.horzline.last.idx] <- sg.horzline.lastA
      sg <- append(sg, sg.horzline.lastB, after=sg.horzline.last.idx)
    }
    sg.new <- append(sg, anova.part, after = sg.horzline.last.idx - 1) # BEFORE the last horizontal line!
    
  } else { # without.anova == T, so NO anova table
    # get model convergences nevertheless (code copied from above)
    conv.code.list <- lapply(list.of.models, function(m) m@optinfo$conv$lme4$code)
    conv.code.list.adj <- lapply(conv.code.list, function(x) ifelse(is.null(x), 0, x))
    mod.conv = unlist(conv.code.list.adj)
    sg.new <- sg
  }
  
  # update model numbers
  if (!is.logical(model.numbers) & !is.null(model.numbers)) {
    get.modnum.line <- function(nrs) {
      return(paste(sprintf('<tr><td style="text-align:left"></td>'),
                   paste(paste("<td>(", nrs, sep=""), ")</td>", sep="", collapse=""),
                   "</tr>", sep="", collapse=""))
    }
    modnum.line.pattern <- get.modnum.line(1:nmod)
    modnum.line.new <- get.modnum.line(model.numbers)
    # remove border before model number line
    modnum.line.new <- gsub("<tr>.*?border-.*:.*?</tr>", "", modnum.line.new)
    sg.new <- stringr::str_replace_all(sg.new, stringr::fixed(modnum.line.pattern), modnum.line.new)
  }
  
  # indicate convergence problems, if there are any
  if (any(mod.conv != 0)) {
    mod.conv.line <- "<tr><td style=\"text-align:left\">Convergence problems</td>"
    for (mc in mod.conv) {
      mod.conv.line <- append(mod.conv.line, ifelse(mc!=0, "<td>&dagger;</td>", "<td></td>"))
    }
    mod.conv.line <- append(mod.conv.line, "</tr>")
    mod.conv.line <- paste(mod.conv.line, collapse="")
    # where to put the line depends on whether there is a note or not
    if (!is.null(notes)) { # there is a note, even if it is a 1-element character vector consisting of an empty string ""
      mod.conv.line.append.after <- length(sg.new)-3 # before note and before the last horizontal line
    } else { # there is no note
      mod.conv.line.append.after <- length(sg.new)-2 # before last horizontal line
    }
    sg.new <- append(sg.new,
                     mod.conv.line,
                     after =  mod.conv.line.append.after)
  }
  
  # indicate singularity, if there are any
  model.is.singular <- unlist(lapply(list.of.models, isSingular))
  if (any(model.is.singular)) {
    mod.sing.line <- "<tr><td style=\"text-align:left\">Model is singular</td>"
    for (ms in model.is.singular) {
      mod.sing.line <- append(mod.sing.line, ifelse(ms==T, "<td>&dagger;</td>", "<td></td>"))
    }
    mod.sing.line <- append(mod.sing.line, "</tr>")
    mod.sing.line <- paste(mod.sing.line, collapse="")
    # where to put the line depends on whether there is a note or not
    if (!is.null(notes)) { # there is a note, even if it is a 1-element character vector consisting of an empty string ""
      mod.sing.line.append.after <- length(sg.new)-3 # before note and before the last horizontal line
    } else { # there is no note
      mod.sing.line.append.after <- length(sg.new)-2 # before last horizontal line
    }
    sg.new <- append(sg.new,
                     mod.sing.line,
                     after = mod.sing.line.append.after )
  }
  
  # add random effects part to the table
  if (!is.null(random.effects.table)) {
    # how many columns are there?
    sg.tmp <- !grepl("colspan|caption", sg) & nchar(sg) > 0 # get index to all rows that do not have any column that spans more than 1 column, should also not contain "caption" which denotes the title element
    sg.ncol <- length(stringr::str_extract_all(sg[which(sg.tmp)[1]], "<td")[[1]]) # take the first such row, and count the occurance of <td> which indicate start of new column
    # use "<td" instead of "<td>", because there could be some style options "<td style..."
    # sg and sg.new have same number of columns!
    
    sg.re <- c(sprintf('<tr><td colspan="%s" style="border-bottom: 1px solid black"></td></tr>', sg.ncol),
               sprintf('<tr><td style="text-align:left"><b>Random effects variances</b></td><td colspan="%s"></td></tr>', sg.ncol-1))
    
    # get random effects structures of all models
    varcorrs <- lapply(list.of.models, VarCorr) # printed as Std.Dev., but internally stored primarily as Variance
    
    # which unique random factor names will there be eventually?
    random.factor.names.print.unique <- unique(random.factor.names) # result is unnamed
    
    # helper function required soon below
    cbind.matrix.fill <- function(a, b) {
      # hack to get NA values for random slopes that are missing in some models but present in others
      # for the same random factor
      if (is.null(a)) return(b)
      if (is.null(b)) return(a)
      a.colnames <- colnames(a)
      b.colnames <- colnames(b)
      # rownames will be conserved anyway
      a.t.dt <- data.table(t(a))
      b.t.dt <- data.table(t(b))
      c.t.dt <- rbind(a.t.dt, b.t.dt, fill = T)
      c <- t(c.t.dt) # t(data.table(...)) results in a matrix!
      colnames(c) <- c(a.colnames, b.colnames)
      return(c)
    }
    
    # create matrices of random slope variances
    # for each random factor one matrix with random effects in the rows and models in the columns
    re.mats <- has.corr.mats <- list()
    for (rfnpu in random.factor.names.print.unique) { # loop through finally used random factor names
      for (mi in 1:nmod) {
        # each finally used random factor name can correspond to several random factor 
        # names in the model object, so get all regex patterns that determine the factor names in the model
        rfn.patterns <- names(random.factor.names)[random.factor.names == rfnpu]
        re.vect <- c()
        has.corr.vect <- c()
        for (rfno in names(varcorrs[[mi]])) { # loop through original random factor names as they are in the model object
          if (any(unlist(lapply(rfn.patterns, function(p) grepl(p, rfno))))) { # does one of the patterns match the original random factor name?
            # current 'rfno' is relevant
            # get the VARIANCES of the random effects component for the current random factor and model
            re.vect.tmp <- diag(varcorrs[[mi]][[rfno]])
            # determine whether a component had correlations estimated
            has.corr.tmp <- rep( length(varcorrs[[mi]][[rfno]]) > 1, nrow(varcorrs[[mi]][[rfno]]) )
            # makes sure the current random slope does not yet exists for the factor name to be printed, 
            # cause that would be a problem meaning there is an error in the model structure/bad model.
            # consider final random slope names instead of the original ones
            for (i in 1:length(re.vect.tmp)) {
              rvtn.grepl.tmp <- unlist(lapply(1:length(random.slope.names), 
                                              function(pi) grepl(names(random.slope.names)[pi], names(re.vect.tmp)[i])))
              if (sum(rvtn.grepl.tmp) > 1) stop(sprintf("Model %d, random factor '%s', random slope '%s': pattern in 'random.slope.names' fits to more than one random slope! That should not be the case!", 
                                                        mi, rfno, names(re.vect.tmp)[i]))
              if (sum(rvtn.grepl.tmp) == 1) {
                # replace the original random slope name with the to-be-printed random slope name
                names(re.vect.tmp)[i] <- gsub(names(random.slope.names[rvtn.grepl.tmp]),
                                              random.slope.names[rvtn.grepl.tmp],
                                              names(re.vect.tmp)[i])
              } else {
                cat(sprintf("No printed random slope name defined for: %s", names(re.vect.tmp)[i]))
              }
            }
            if (! any(names(re.vect.tmp) %in% names(re.vect))) {
              re.vect <- c(re.vect, re.vect.tmp)
              has.corr.vect <- c(has.corr.vect, has.corr.tmp)
            } else {
              stop(sprintf("Model %d, random factor '%s': random slopes '%s' seem to exist multiple times for the same random factor, which means there is an error in the model structure or it's a bad model!", 
                           mi, rfno, names(re.vect)))
            }
          } else {
            # current 'rfno' is not relevant for the current model
          }
        }
        re.mat.tmp <- as.matrix(re.vect, ncol = 1)
        colnames(re.mat.tmp) <- model.numbers[mi]
        re.mats[[rfnpu]] <- cbind.matrix.fill(re.mats[[rfnpu]], re.mat.tmp)
        # similar for the matrix that tells whether correlations have been estimated
        has.corr.mat.tmp <- as.matrix(has.corr.vect, ncol = 1)
        colnames(has.corr.mat.tmp) <- model.numbers[mi]
        rownames(has.corr.mat.tmp) <- rownames(re.mat.tmp)
        has.corr.mats[[rfnpu]] <- cbind.matrix.fill(has.corr.mats[[rfnpu]], has.corr.mat.tmp)
      }
    }
    
    # a helper function
    rm.table.and.borders <- function(sg.output) {
      a <- gsub("</?table.*?>", "", sg.output) # remove html-table-environment
      b <- gsub("<tr>.*?border-bottom:.*?</tr>", "", a) # remove borders/lines
      c <- b[nchar(b) != 0] # remove empty elements
      return(c)
    }
    
    # for each random factor, put a separate section into the final table (store that intermediately in 'sg.re')
    for (rfn in random.factor.names.print.unique) {
      
      # add random factor name to table, in italics!
      if (rfn == random.factor.names.print.unique[1])
        # for the first one: NO border!
        sg.re <- append(sg.re, sprintf('<tr><td style=\"text-align:left\"><i>%s</i></td> <td colspan=\"%d\"></td> </tr>',
                                       rfn, sg.ncol-1)) else
           # for all other ones add border (horizontal line)
           sg.re <- append(sg.re, sprintf('<tr><td style=\"text-align:left\"><i>%s</i></td> <td colspan=\"%d\" style=\"border-top: 1px solid black\"></td> </tr>', 
                                          rfn, sg.ncol-1))
      
      # add table of variance components
      sg.re.tmp.raw <- stargazer(re.mats[[rfn]], type = "html", rownames = T, colnames = F)
      sg.re.tmp <- rm.table.and.borders(sg.re.tmp.raw) # puts each line in one element of character vector
      # add sign for whether component is correlated
      for (i in 1:nrow(has.corr.mats[[rfn]])) { # i ... rows
        for (j in 1:ncol(has.corr.mats[[rfn]])) { # j ... columns
          if (!is.na(has.corr.mats[[rfn]][i,j])) {
            if (has.corr.mats[[rfn]][i,j]) { # add sign for correlations to printed table
              td.starts.tmp <- gregexpr("</td", sg.re.tmp[[i]])[[1]]
              sg.re.tmp[i] <- paste(substr(sg.re.tmp[i], 1, td.starts.tmp[j]-1), 
                                    denoteCorrelationHTML,
                                    substr(sg.re.tmp[i], td.starts.tmp[j], nchar(sg.re.tmp[i])),
                                    sep = "")
            }
          }
        }
      }
      
      sg.re <- append(sg.re, sg.re.tmp)
      
    } # end adding separate section for each random factor
    
    # add a closing partial horizontal line to delimit random effect from residual variances
    sg.re <- append(sg.re, sprintf('<tr><td style=\"text-align:left\"></td> <td colspan=\"%d\" style=\"border-top: 1px solid black\"></td> </tr>', 
                                   sg.ncol-1))
    
    # add residual variances
    residual_stddev <- unlist(lapply(list.of.models, sigma))
    residual_var <- residual_stddev ^ 2
    residual_var_matrix <- t(matrix(residual_var)) # just a 1-by-nmod matrix!
    rownames(residual_var_matrix) <- "Residual Variance"
    
    sg.resid.var <- rm.table.and.borders(stargazer(residual_var_matrix, type = "html"))
    sg.re <- append(sg.re, sg.resid.var)
    
    # add fixed effects headline
    sg.re <- append(sg.re,
                    sprintf('<tr><td colspan=\"%d\" style=\"border-bottom: 1px solid black\"></td></tr><tr><td style=\"text-align:left\"><b>Fixed effects</b></td><td colspan=\"%s\"></td></tr>', sg.ncol, nmod))
    
    # get row number for each row of 'sg.new'
    sg.new.rownumber.tmp <- grepl("<tr>", sg.new)
    sg.new.rownumbers <- cumsum(sg.new.rownumber.tmp)
    sg.new.rownumbers[1+which(diff(sg.new.rownumbers)==0)] <- 0
    
    # insert random effects part of the table
    sg.new <- append(sg.new, sg.re, after = random.effects.table)
    # remove the border/line that might appear after the table
    sg.new[random.effects.table + length(sg.re) + 1] <- gsub("<tr>.*?border-bottom:.*?</tr>", "",
                                                             sg.new[random.effects.table + length(sg.re) + 1])
    
  } # finished adding random effects
  
  if (!is.null(out)) {
    cat(paste(sg.new, "\n"), file=out) # print to file
  }  
  
  if (!without.anova) attr(sg.new, "anova") <- atab else attr(sg.new, "anova") <- NULL
  
  return(sg.new)
}

