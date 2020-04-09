
rt.filter.mad <- function(df, form, madFactor = 2.5, index=NULL, skip.factor.check=F, ...) {
  # Replaces valus of a variable with NA if they are further away from median than 
  # 'madFactor' median absolute deviation for each factor-level combination specified 
  # on the right side of the forumla 'form'. The left side of 'form' specifies the 
  # variable where values are replaced by NA. Returns the resulting variable with NA
  # values and some useful attributes.
  #
  # This function filters with "split", "loop over elements", "unsplit". 
  # Works also if the numbers of observations per cell are unequal.
  #
  # df ... a data.frame
  # form ... the formula used for filtering; the dependent variable on the left side,
  #          the independent variables on the right side separated by pluses '+', e.g.
  #             RT ~ Size + Training + ParticipantID
  # index ... (logical) only take indicated rows for filtering from 'df'. Rows that are
  #           "not indicated", i.e. that have logical value FALSE, will be NA in the output 
  #           vector.
  # madFactor ... 3 very conservative, 2.5 moderatly conservative, 2 poorly conservative
  #               see further reference cited in:
  #             Leys C, Ley C, Klein O, Bernard P, Licata L (2013) Detecting outliers: 
  #                 Do not use standard deviation around the mean, use absolute deviation
  #                 around the median. J Exp Soc Psychol 49:764-766 doi:
  #                 http://dx.doi.org/10.1016/j.jesp.2013.03.013.
  # "..." ... futher arguments passed to 'mad'
  
  if (any(class(df)=="data.table")) # convert to data.frame, otherwise it would not work!
    df <- data.frame(df)

  # only take indicated rows
  if (!is.null(index)) {
    if (length(index) != nrow(df)) stop("'index' has to have as many elements as there are rows in 'df'! That's not the case!")
    df <- subset(df, index)
  }
  
  # extract names of dependent and independent variables from formula "form"
  dep.var <- as.character(form[[2]])
  indep.var <- strsplit(as.character(form[3]), split=" ?[+*] ?")[[1]]
  
  # check that all indep.vars are really factors!
  if (!skip.factor.check) {
    for (iv in indep.var) {
      if (class(df[[iv]]) != "factor") {
        cat(iv, " is of type ", class(df[[iv]]))
        stop("All specified independent variables have to be of class 'factor'!")
      }
    }
  }
  
  # the values of the dependent variable
  dep.var.vals <- df[,dep.var]
  
  # create a list of factors to be used with split
  flist <- eval(parse(text = paste("list(", paste("df$", indep.var, sep="", collapse=", "), ")") ))
  
  # split the values of the dependent variable
  dep.var.splt <- dep.var.splt.orig <- split(dep.var.vals, flist) # '.orig' is used for the output message!
  
  # compute medians and mads
  median.fnc <- function(x) median(x, na.rm = T)
  mad.fnc <- function(x) mad(x, na.rm = T, ...)
  
  dep.var.medians <- lapply(dep.var.splt.orig, median.fnc)
  dep.var.mads <- lapply(dep.var.splt.orig, mad.fnc)
  
  dep.var.splt.logical.idx <- lapply(dep.var.splt.orig, function(x) rep(F, length(x)))
  
  for (i in 1:length(dep.var.splt)) { # might be done with 'lapply', or is that impossible here?
    idx.tmp1 <- dep.var.splt[[i]] < (dep.var.medians[[i]] - madFactor * dep.var.mads[[i]])
    idx.tmp2 <- dep.var.splt[[i]] > (dep.var.medians[[i]] + madFactor * dep.var.mads[[i]])
    dep.var.splt[[i]][idx.tmp1 | idx.tmp2] <- NA
    dep.var.splt.logical.idx[[i]] [ idx.tmp1 | idx.tmp2] <- T
  }
  
  # put filtered variable values together again
  dep.var.filt <- unsplit(dep.var.splt, flist)
  dep.var.filt.logical.idx <- unsplit(dep.var.splt.logical.idx, flist)
  
  # print info about how many values were replaced by NA
  nvals.prefilt = sum(!is.na(unlist(dep.var.splt.orig)))
  nvals.postfilt = sum(!is.na(unlist(dep.var.splt)))
  percent.filtered = (nvals.prefilt - nvals.postfilt) / nvals.prefilt * 100
  # NOTE: Take 'unlist(dep.var.splt)' instead of 'dep.var.vals' and 'dep.var.filt', since there might be factors
  # with missing values been used for splitting, which would result in much more NaN values in dep.var.filt compared
  # to how many NaN values would be there just because of the filter, since values in the dependent variable, where 
  # at least one factor has missing values are Nan after 'unsplit', although they were a valid value before 'split', 
  # i.e. before the filter was applied.
  message( paste( "rt.filter.mad: Replaced", nvals.prefilt - nvals.postfilt, "values by NA,",
                  "i.e. removed ", percent.filtered, "%", "\n") )
  
  # return filtered values depending on 'index'
  if (!is.null(index)) {
    dep.var.filt.idx <- rep(NA, length(index)) # index has same length as there were rows in df, that's ensured above!
    dep.var.filt.idx[index] <- dep.var.filt
    dep.var.filt.idx.logical.idx <- rep(F, length(index))
    dep.var.filt.idx.logical.idx[index] <- dep.var.filt.logical.idx
    # add info about how many filtered as attribute to the output
    attr(dep.var.filt.idx, "percent removed") <- percent.filtered
    attr(dep.var.filt.idx, "N removed") <- nvals.prefilt - nvals.postfilt
    attr(dep.var.filt.idx, "logical.index.to.filtered.values") <- dep.var.filt.idx.logical.idx
    return(dep.var.filt.idx)
  } else {
    attr(dep.var.filt, "percent removed") <- percent.filtered
    attr(dep.var.filt, "N removed") <- nvals.prefilt - nvals.postfilt
    attr(dep.var.filt, "logical.index.to.filtered.values") <- dep.var.filt.logical.idx
    return(dep.var.filt)
  }  
}

