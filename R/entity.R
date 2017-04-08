#' PhysicalEntity Queries
#'
#'  \code{rtEntity()} retrieves the subunits, structures or other forms of a given entity
#'
#' @param id string or a character vector of identifiers of the complex,
#' entity or the PhysicalEntity that you want to query
#' @param queryType string. Specify the query type. can be \code{subunits},
#' \code{componentOf}, or \code{otherForms}.
#' @param silent logical;if true, run quietly. Default = false.
#' @details
#' \code{queryType} =
#' \itemize{
#'   \item \code{subunits}: retrieves the list of subunits that constitute any given complex.
#' In case the complex comprises other complexes, this method recursively breaks all of them into
#' their subunits.
#'   \item \code{componentOf}: this method will retrieves the list of structures
#' (Complexes and Sets) that include the given entity as their component.It should be mentioned
#' that the list includes only simplified entries (type, names, ids) and not full information about each item.
#'   \item \code{otherForms}. retrieves a list containing all other forms of the given PhysicalEntity.
#' These other forms are PhysicalEntities that share the same ReferenceEntity identifier,
#' e.g. PTEN H93R[R-HSA-2318524] and PTEN C124R[R-HSA-2317439] are two forms of PTEN.
#' }
#' @return a data frame.
#' @include url2dataframe.R
#' @include list2dataframe.R
#' @export
#' @examples
#' ## subunits query of R-HSA-5674003
#' entity.1 = rtEntity("R-HSA-5674003", "subunits")
#' ## structures that include R-HSA-199420 as their component
#' entity.2 = rtEntity("R-HSA-199420", "componentOf")
#' ## tha same type query for multiple ids
#' entity.3 = rtEntity(id = c("R-HSA-5674003", "R-HSA-199426"), queryType = "subunits")
#' @rdname entity

rtEntity = function(id, queryType, silent = FALSE){


  if(is.null(id) || is.null(queryType)){
    stop("Both of id and queryType cannot be null")
  }

  url = 'http://www.reactome.org/ContentService/data/'
  tmp = switch(queryType,
               subunits = 'complex',
               componentOf = 'entity',
               otherForms = 'entity')
  dt.ls = lapply(id,
                 function(x) url2dataframe(list(url, tmp, x, queryType),
                 silent = silent, rework = function(s) {cbind(inputId = x, s)}))
  return(list2dataframe(dt.ls, NULL, NULL, NULL))


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


