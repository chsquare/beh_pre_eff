
fixef.tab <- function(model) {
  # returns a table of fixed effects from an lme4-model
  estimates <- fixef(model)
  varnames <- names(estimates)
  sds <- sqrt(diag(vcov(model)))
  tvals <- estimates/sds
  rnames <- names(estimates)
  cnames <- c("Estimate", "Std. Error", "t value")
  tab <- data.frame(a = estimates,
                    b = sds,
                    c = tvals)
  rownames(tab) <- rnames
  colnames(tab) <- cnames
  return(tab)
}