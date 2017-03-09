#' @import httr
#' @import stringr
#' @include error.R
eventsHierarchy <- function(species = "9606"){
  url = str_c("http://www.reactome.org/ContentService/data/eventsHierarchy/", species)
  str = GET(url, accept_json())
  # reponse code
  cont = content(str)
  if(!str$status_code == 200){
    return(errorMessage(cont))
  }
  return(list(list(children = cont)))
}


