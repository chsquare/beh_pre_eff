
import.data <- function(regexPattern, rawDataPath) {
  
  fnmsAll <- list.files(path = rawDataPath)
  fnms <- fnmsAll[grepl(regexPattern, fnmsAll)]
  
  cat(sprintf("Importing from the following %d files:\n", length(fnms)))
  cat(paste("   ", fnms, sep = "", collapse = "\n"))
  cat("\n")
  
  Sys.time()
  dat <- data.table()
  for (fi in 1:length(fnms)) {
    dattmp <- data.table(read.table(paste(rawDataPath, fnms[fi], sep="/"), header = T, sep = "\t"))
    dat <- rbind(dat, dattmp, fill = TRUE) # fill == TRUE fills up missing columns with NA
  }; rm(dattmp)

  Sys.time()
  cat("Done.\n")
  
  return(copy(dat))
}