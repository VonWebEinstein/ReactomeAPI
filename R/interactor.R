#' Molecule interactors
#'
#' Retrieve clustered interaction, details or summary, sorted by score,
#' of single or multiple given accession(s) by resource i.e. PSICQUIC method.
#' Retrieve a detailed(summary) interaction information of given accession(s)
#' i.e. static method.
#' @usage
#' reactomeInteractor(acc, resource = 'Reactome', type = 'psicquic',
#'                    detail = TRUE, silent = FALSE)
#' reactomeInteractor(acc, type = 'static', detail = TRUE, page = -1,
#'                    pageSize = -1, silent = FALSE)
#' reactomeInteractor(acc, type = 'static', detail = FALSE, silent = FALSE)
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
#' @rdname interactor
#' @export
#' @include POST_Method.R

reactomeInteractor <- function(acc,
                               resource = 'Reactome',
                               type = 'static',
                               detail = TRUE,
                               page = -1,
                               pageSize = -1,
                               silent = FALSE){

}
