#' Queries for one or multiple  identifiers
#'
#' Analyse the post identifiers over the different species or projects the result
#' to Homo Sapiens
#'
#' @param id character vector. one or multiple identifiers to analyse followed by
#' their expresssion(when applied)
#' @param MakeProjection logical, whether projects the result to Homo Sapiens or not.
#' @param IncludeInteractors logical. whether include interactors.
#' @param pageSize integer. pathways per page.
#' @param page integer. page number
#' @param sortBy string. how to sort the result.
#' @param order string. specifies the order.
#' @param resource string. the resource to sort.
#' @param silent logical, to run quietly.
#'
#' @details
#' The projection is calculated by the orthologous slot in the Reactome database.
#' Use page and pageSize to reduce the amount of data retrieved. Use sortBy and
#' order to sort the result by your preferred option. The resource field will
#' filter the results to show only those corresponding to the preferred molecule
#' type (TOTAL includes all the different molecules type).
#'
#' \code{sortBy} can be one of \code{NAME,TOTAL_ENTITIES,TOTAL_INTERACTORS,
#' TOTAL_REACTIONS,FOUND_ENTITIES,FOUND_INTERACTORS,FOUND_REACTIONS,ENTITIES_RATIO,
#' ENTITIES_PVALUE,ENTITIES_FDR,REACTIONS_RATIO}.
#'
#' \code{order} can be \code{ASC} or \code{DESC}.
#'
#' \code{resource} can be \code{TOTAL,UNIPROT,ENSEMBL,CHEBI,MIRBASE,NCBI_PROTEIN,
#' EMBL,COMPOUND}.
#' @return a data frame including pathway information; if no pathway found,
#' a summary data frame will be returned.
#' @export
#' @rdname identifier
#' @include includes.R
#' @examples
#' # analyse multiple ids at the same time
#' pathwayInf.1 = rtAnalyseIdentifier(id = c('Q13501', 'P41743', 'Q9H492', '10652'))
#' # projects the result to Homo Sapiens
#' pathwayInf.2 = rtAnalyseIdentifier(id = c('Q13501', 'P41743', 'Q9H492', '10652'),
#' MakeProjection = TRUE)

rtAnalyseIdentifier = function(id,
                             MakeProjection= FALSE,
                             IncludeInteractors = FALSE,
                             pageSize = -1,
                             page = -1,
                             sortBy = "ENTITIES_PVALUE",
                             order = "ASC",
                             resource = "TOTAL",
                             silent = FALSE){

  projection = switch (MakeProjection, "projection", NULL)
  tmp = str_c(c("?interactors", "pageSize", "page", "sortBy",
                "order", "resource"),
              c(str_to_lower(IncludeInteractors), pageSize,
                page, sortBy, order, resource),
              sep = "=", collapse = "&")
  url = str_c(getURL(), projection, tmp, collapse = "")

  checkempty_func = function(x) x$pathwaysFound == 0 & length(x$summary) == 0

  rework_func = function(x){

    if(x$pathwaysFound){

      return(x$pathways)
    }else{

    return(x$summary)
    }
  }

  dt = POST_Method(url =url,
                   body = str_c(id, collapse = ","),
                   rework_func = rework_func,
                   checkempty_func = checkempty_func,
                   silent = silent)
  return(dt)

}

getURL = function(){

  "http://www.reactome.org/AnalysisService/identifiers/"
}



