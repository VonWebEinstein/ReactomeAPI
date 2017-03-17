#' PhysicalEntity Queries
#'
#' Retrieves the subunits, structures or other forms of a given entity
#'
#'

#' @param id a character vector of identifiers of the complex, entity or the PhysicalEntity
#' that you want to query
#' @param queryType a character vector that specify the query type. can be \code{subunits},
#' \code{componentOf}, or \code{otherForms}.
#' @param simplify attempt to reduce the result to a data frame when length of \code{id} is 1.
#' @details
#'
#' queryType = \code{subunits}. Retrieves the list of subunits that constitute any given complex.
#' In case the complex comprises other complexes, this method recursively breaks all of them into
#' their subunits.
#'
#' When queryType = \code{componentOf}, this function will retrieves the list of structures
#' (Complexes and Sets) that include the given entity as their component.It should be mentioned
#' that the list includes only simplified entries (type, names, ids) and not full information about each item.
#'
#' queryType = \code{otherForms}. Retrieves a list containing all other forms of the given PhysicalEntity.
#' These other forms are PhysicalEntities that share the same ReferenceEntity identifier,
#' e.g. PTEN H93R[R-HSA-2318524] and PTEN C124R[R-HSA-2317439] are two forms of PTEN.
#' @return a list of tha same length of \code{id}, with ecah element corresponding to the query result of
#' one of id.
#' @include error.R
#' @export
#' @examples
#' rtEntity("R-HSA-5674003", "subunits")
#' rtEntity("R-HSA-199420", "componentOf")
#' rtEntity(id = c("R-HSA-5674003", "R-HSA-199420"),
#'          queryType= c("subunits", "componentOf"))

#'
#' @import httr
#' @import jsonlite
#' @import stringr
#' @rdname entity

rtEntity = function(id, queryType, simplify = TRUE){

  res = mapply(reactomeEntity, id = id, queryType = queryType,
               SIMPLIFY = FALSE)
  if(length(res) == 1 && simplify){

    res = res[[1]]
  }
  return(res)
}


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


