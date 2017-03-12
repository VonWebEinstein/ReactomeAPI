#' PhysicalEntity Queries
#'
#' Retrieves the subunits, structures or other forms of a given entity
#'
#'

#' @param id a character string. identifier of the complex, entity or the PhysicalEntity
#' that you want to query
#' @param queryType a character string that specify the query type. can be \code{subunit},
#' \code{structure}, or \code{form}.
#' @details
#' queryType = "subunits". Retrieves the list of subunits that constitute any given complex.
#'
#' queryType = "componentOf". Retrieves the list of structures (Complexes and Sets) that include
#' the given entity as their component.
#'
#' queryType = "otherForms". Retrieves a list containing all other forms of the given PhysicalEntity.
#' @return a dataframe
#' @include error.R
#' @export
#' @examples
#' reactomeEntity("R-HSA-5674003", "subunits")
#' reactomeEntity("R-HSA-199420", "componentOf")

#'
#' @import httr
#' @import jsonlite
#' @rdname entity

reactomeEntity = function(id,          # entity id, complex id , dbid or StId of a PhysicalEntity
                        queryType){    # query type about Entity

  if(is.null(id) || is.null(queryType)){
    stop("Both of id and queryType cannot be null")
  }

  url = 'http://www.reactome.org/ContentService/data/'
  tmp = switch(queryType,
               subunits = 'complex',
               componentOf = 'entity',
               otherForms = 'entity')
  url = str_c(url, tmp, '/', id, '/', queryType)

  res = try(fromJSON(url), silent = TRUE)
  if(inherits(res, "try-error")){
    tmp = content(GET(url, content_type_json()))
    return(errorMessage(tmp))
  }
  else{
    return(res)
  }
}


#' @export
#' @rdname entity
#' @examples
#' entity = rtEntity("R-HSA-199420", "otherForms")
#'
rtEntity <- function(id , queryType){
  return(reactomeEntity(id = id, queryType = queryType))
}
  # if(queryType == 'subunits')
  #   tmp = 'complex'
  # else
  #   tmp = 'entity'

  # if(queryType == "subunit")
  # {
  #   url = paste("http://www.reactome.org/ContentService/data/complex/", id, "/subunits",
  #               sep = "")
  #
  # }
  # if(queryType == "structure"){
  #
  #   url = paste("http://www.reactome.org/ContentService/data/entity/", id, "/componentOf",
  #               sep = "")
  # }
  # if(queryType == "form"){
  #
  #   url = paste("http://www.reactome.org/ContentService/data/entity/", id, "/otherForms",
  #               sep = "")
  # }
  # response = GET(url)
  # if(status_code(response) == 404){
  #
  #   print("Identifier does not match with any in current data\n")
  #   return(404)
  # }
  # if(status_code(response) == 406){
  #
  #   print("Not acceptable according to the accept headers sent in the request\n")
  #   return(406)
  # }
  # if(status_code(response) == 500){
  #
  #   print("Internal Server Error\n")
  #   return(500)
  # }
  # if(status_code(response) == 200){
  #
  #   dtframe = fromJSON(url)
  #   return(dtframe)
  # }


