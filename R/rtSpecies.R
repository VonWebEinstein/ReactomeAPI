#' Species related queries
#'
#' This method retrieves the dataframe of all species or main species
#' in Reactome knowledgebase, sorted by name.
#'
#' @param species string. "all":retrieves the dataframe of all species.
#' "main":retrieves the dataframe of main species
#'
#' @return dataframe.
#'
#' @examples
#' all_species = rtSpecies("all")
#' main_species = rtSpecies("main")
#'
#' @include error.R
#' @include url2dataframe.R
#' @import jsonlite
#' @import stringr
#' @import httr
#' @export
#'
#' @rdname species
#'
# 参数species -> main = TRUE
reactomeSpecies <- function(species = 'main', silent = FALSE){
  if(is.null(species))
    stop("species can't be null")
  url = list("http://www.reactome.org/ContentService/data/species",species)
  dt = url2dataframe(urlComponent = url, silent = silent)
  return(dt)
  # res = try(fromJSON(url),silent = TRUE)
  # if(inherits(res,"try_error")){
  #   tmp = content(GET(url),content_type_json())
  #   return(errorMessage(tmp, silent = silent))
  # }
  # else{
  #   return(res)
  # }
}

#' @export
#' @rdname species
#'
rtSpecies <- function(species){
  return(reactomeSpecies(species))
}
