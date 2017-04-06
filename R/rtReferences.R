#' ReferenceEntity queries
#'
#' Retrieves a dataframe containing all the reference entities for a given identifier.
#'
#' @param id string. Identifier for a given entity
#'
#' @return A dataframe.
#'
#' @examples refrenceEntity = reactomeReferences("15377").
#' @export
#'
#' @rdname entity
#'
# 帮助指定到entity
reactomeReferences <- function(id){
  if(is.null(id))
    stop("id can't be null")
  url = str_c("http://www.reactome.org/ContentService/references/mapping/",id)
  res = try(fromJSON(url), silent = TRUE)
  if(inherits(res, "try-error")){
    tmp = content(GET(url, content_type_json()))
    return(errorMessage(tmp, silent = silent))
  }
  else
    return(res)
}

#' @rdname entity
#' @export

rtReferences <- function(id){
  return(reactomeReferences(id))
}

