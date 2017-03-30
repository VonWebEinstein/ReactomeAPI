#' Species related queries
#'
#' This method retrieves the dataframe of all species or main species
#' in Reactome knowledgebase, sorted by name.
#'
#' @param main logical.\code{TRUE}:retrieves the dataframe of main species.
#'                     \code{FALSE}:retrieves the dataframe of all species.
#'
#' @return dataframe.
#'
#' @examples
#' main_species = rtSpecies(TRUE)
#' all_species = rtSpecies(FALSE)
#'
#' @include error.R
#' @include url2dataframe.R
#' @export
#'
#' @rdname species
#'

reactomeSpecies <- function(main = TRUE, silent = FALSE){
  if(is.null(main))
    stop("argument is lack")
  if(main)
    species = "main"
  else
    species = "all"
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
rtSpecies <- function(main = TRUE){
  return(reactomeSpecies(main))
}
