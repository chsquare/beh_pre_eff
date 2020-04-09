
stargazer.anova.Rmd <- function(..., this.caption, constant.name = "Grand mean", 
                                omit.fitting.method = F, add.VarCorr.tables = T) {
  # Print a stargazer anova table and trailing tables with content of VarCorr.as.data.frame().
  # This functions prints the stargazer table directly to output, thus, 
  # the table can nicely be included in Rmd file with knitr option results='asis'.
  #
  # Use 'this.caption' to get a title for the table (instead of 'title' which would be passed
  # to stargazer, and confuses the hacky modifications to upper part of table).
  #
  # constant.name ... a name to replace "(Intercept)", aka "Constant"
  #
  # This function requires other function:
  #   stargazer.anova()
  #   VarCorr.as.data.frame() ... only needed if add.VarCorr.tables = T
  
  options(knitr.kable.NA = "") # this is required to print nothing instead of "NA" in the VarCorr table
  
  capture.output(sga <- stargazer.anova(..., omit.fitting.method = omit.fitting.method)) # prevent printing anything yet
  # For debugging, use this line instead:
  # sga <- stargazer.anova(..., omit.fitting.method = omit.fitting.method)
  
  # Replace the term 'Constant' by different name, if requested
  if (!is.na(constant.name))
    sga <- gsub("Constant", constant.name, sga)
  
  # insert table caption
  sga[2] <- gsub("(<table.*?>)(.*)", paste("\\1<caption><strong>", this.caption, "</strong></caption>\\2"), sga[2])
  
  # which models are there?
  list.of.... <- as.list(list(...))
  if (! is.null(names(list.of....)))
    # Trick: models do not have names, in contrast to specified parameters, 
    # but only if there are additional input arguments:
    list.of.models <- list.of....[names(list.of....) == ""] else
      # if there are no additional input arguments, the names of ... are NULL
      list.of.models <- list.of....
  
  # how many?
  nmod <- length(list.of.models) # number of models
  
  # what about their number
  model.numbers <- list.of....[[which(names(list.of....) == "model.numbers")]]
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
  
  # get their names, assuming that the first inputs were the models
  call.char <- as.character(sys.call())
  # element 2 contains the first argument, if this was not '...' take it and the following ones as model names
  if (call.char[2] != "...") {
    model.names <- names(list.of....[names(list.of....) == ""]) 
  } else {
    # try to get names from calling function, quite hacky...
    model.names <- tryCatch( {
      call.1.char <- as.character(sys.call(-1));
      if (!is.null(call.1.char) & call.1.char[2] != "...") # so these might be the model names, get them from the 'call' instead of trying to get the '...' from the parent function
        call.1.char[2:(nmod+1)] else NULL},
      error = function(e) {cat("Could not get model.names!")})
  }
  
  # Append tables with variance-covariance structure to the output
  alltabs <- sga
  model.numbers.uniq <- unique(model.numbers)
  
  if (add.VarCorr.tables) {
    for (mn in model.numbers.uniq) {
      # current model(s), possibly more
      cms <- list.of.models[model.numbers == mn]
      # if there are more instances for one model number, the model instances are expected to be identical!
      cml <- unique(cms) # still a list
      if (length(cml) > 1) stop("One model number refers to more than one model! That should not be the case!")
      cm <- cml[[1]] # take the model out of the list
      # names for random factors and random slopes
      rfns <- list.of....[names(list.of....) == "random.factor.names"][[1]] # get list-element, without enclosing list object
      random.slope.names <- list.of....[names(list.of....) == "random.slope.names"][[1]] # get list-element, without enclosing list object
      # varcor as data.frame
      vcdf <- VarCorr.as.data.frame(cm, rfns, random.slope.names)
      # convert to html table with caption, with ktable
      # vct <- kable(vcdf, "html", caption = sprintf("Model %d (%s), random effects structure.", mn, cn))
      # ... better stargazer to get uniform layout
      # if model-uniqueness check was fine, the name is also fine:
      if (!is.null(model.names)) # create a model id: "number (name, if available)"
        mid <- sprintf("%d (%s)", mn, unique(model.names[model.numbers == mn])) else
          mid <- sprintf("%d", mn)
      capture.output(vct <- stargazer(vcdf, summary=F, type="html", rownames = F,
                                      title = sprintf("Model %s, random effects structure.", mid)))
      # adjust column names in case of correlations - quite a hack
      # how many trailing correlation columns without colname are there?
      ntrailcor <- sum(names(vcdf)=="")
      vct[3] <- gsub("<td>Correlations</td>.*</tr>", 
                     sprintf('<td>Correlations</td><td colspan=\"%d\"></td></tr>', ntrailcor),
                     vct[3])
      
      alltabs <- append(alltabs, c("<p>", vct, "</p>")) # vct within a separate paragraph
    }
  }

  cat(paste(alltabs, "\n"))
  
  return(invisible(NULL))
}
