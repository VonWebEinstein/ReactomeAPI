# print error message, and return FALSE
errorMessage <- function(cont){
  cat(cont$code, ' ')
  cat(cont$reason, '\n')
  lapply(cont$messages, cat, '\n')
  return(FALSE)
}
