#' Molecule interactors
#'
#' Retrieve clustered interaction, details or summary, sorted by score,
#' of single or multiple given accession(s) by resource i.e. PSICQUIC method.
#' Retrieve a detailed(summary) interaction information of given accession(s)
#' i.e. static method.
#' @usage
#' rtInteractor(acc, resource = 'Reactome', type = 'psicquic',
#'                    detail = TRUE, silent = FALSE)
#' rtInteractor(acc, type = 'static', detail = TRUE, page = -1,
#'                    pageSize = -1, silent = FALSE)
#' rtInteractor(acc, type = 'static', detail = FALSE, silent = FALSE)
#' @param acc string. Interactor accession(s) (or identifier(s)).
#' @param resource string. PSICQUIC Resource.
#' @param type string. \code{psicquic}: PSICQUIC method;
#' \code{static}: static method.
#' @param detail logical. Return details or summary.
#' @param page interger. For paginating the results.
#' @param pageSize interge. Number of results to be retrieved.
#' @param silent logical. Run quitely.
#'
#' @return
#' \itemize{
#'   \item \code{logical FALSE}: When an error happens.
#'   \item \code{list}: When query details of multiple entries.
#'   \item \code{dataframe}: Others.
#' }
#'
#' @examples
#' # List PSICQUIC proviers:
#' (provs = PSICQUIC_Provier())
#' \dontrun{
#' ins.p = rtInteractor(acc = c('Q13501','Q16543'), type = 'psiquic',
#'                 resource = 'MINT')}
#' ins.s = rtInteractor(acc = c('Q13501','Q16543'), type = 'static',
#'                 resource = 'MINT')
#' @rdname interactor
#' @export
#' @include POST_Method.R

rtInteractor <- function(acc,
                         type = 'static',
                         resource = 'Reactome',
                         detail = TRUE,
                         page = -1,
                         pageSize = -1,
                         silent = FALSE){
  # check type
  type = str_to_lower(type)
  if(length(type) > 1 || !any(type %in% c('static', 'psicquic')))
    stop("type can only be 'static' or 'psicquic'")

  # url components
  url0 = 'http://www.reactome.org/ContentService/interactors'
  url = list(url0, type, 'molecules')
  ds = c('summary', 'details')
  ds = ds[detail + 1]
  if(type == 'psicquic'){
    url = c(url, resource, ds)
  }
  else{
    if(detail)
      url = c(url, str_c(ds, '?page=', page, '&pageSize=', pageSize))
    else
      url = c(url, ds)
  }

  # define vadidate empty dataframe func
  checkempty_func = function(x) length(x$entities) == 0
  # define rework_func func
  # rework_func = function(x) x$entities
  multipleAcc = length(acc) > 1 || (length(acc) == 1 && str_detect(acc, ','))
  # detail
  if(detail){
    rework_func = function(x) x$entities$interactors
    if(!multipleAcc)
      rework_func = function(x) x$entities$interactors[[1]]
  }
  # summary
  else{
    rework_func = function(x) x$entities
  }

  dt = POST_Method(url = url,
                   body = str_c(acc, collapse = ','),
                   rework_func = rework_func,
                   checkempty_func = checkempty_func,
                   silent = silent)
}

#' @rdname interactor
#' @export
#' @details \code{PSICQUIC_Provier} returns the list of PSICQUIC Proviers.
#'
PSICQUIC_Provier = function(silent = FALSE){
  url = 'http://www.reactome.org/ContentService/interactors/psicquic/resources'
  dt = url2dataframe(url)
}
