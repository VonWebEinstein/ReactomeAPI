#' @import httr
#' @import stringr
eventsHierarchy <- function(species = "9606"){
  url = str_c("http://www.reactome.org/ContentService/data/eventsHierarchy/", species)
  str = GET(url, accept_json())
  return(content(str))
}
