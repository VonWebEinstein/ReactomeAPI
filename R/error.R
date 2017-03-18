# print error message, and return FALSE
errorMessage <- function(cont, silent){
  if(!silent){
    cat(cont$code, ' ')
    cat(cont$reason, '\n')
    lapply(cont$messages, cat, '\n')
  }
  return(FALSE)
}
