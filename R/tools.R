# creat directory if doesn't exist
# dic, completed directory
validateAndCreatDir <- function(dir){
  if(!dir.exists(dir))
    dir.create(dir)
}

# bind two dataframes, won't check data type of the same column
bindbyrow <- function(dt1, dt2){
  ns1 = names(dt1)
  ns2 = names(dt2)
  KEYS = unique(c(ns1, ns2))
  if(!all(sapply(c(ns1, ns2), length) == length(KEYS))){
    missingNames = KEYS[!(KEYS %in% ns1)]
    dt1[missingNames] = NA
    missingNames = KEYS[!(KEYS %in% ns2)]
    dt2[missingNames] = NA
  }
  rbind(dt1, dt2)
}
