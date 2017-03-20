#' Disease related queries
#'
#' It retrieves the list of disease or disease DOIDs annotated in Reactome
#'
#' @description retrieves the list of disease DOIDs annotated in Reactome
#'
#' @usage
#'
#' @param dataType a chacter string that specify the data type. can be \code{diseasw}
#' or \code {disease/doid}
#'
#' @details 'dataType = disease ' It retrieves the list of diseases annotated in Reactome
#' 'dataType = disease/doid',It retrieves the list of disease DOIDs annotated in Reactome
#' Its Response Class (Status 200) is a string,else if Response Messages HTTP Status
#' \Code{406},the reason is Not acceptable according to the accept headers sent in the request
#' \code{500},Internal Server Error
#'
#' @return a dataframe
#' @export
#' @examples
#' reactomeDiseases("disease")
#' reactomeDiseases("disease/doid")
#'
#' @import stringer
#' @import httr
#' @import jsonlite
#' @rdname disease
#'
#'
#'
reactomeDiseases <- function(dataType) {
  if(is.null(dataType))
    stop("dataType cannot be null")
  url = str_c('http://www.reactome.org/ContentService/data/',dataType)
  if(dataType == "diseases")
    res = fromJSON(url)
  else if(dataType == "diseases/doid"){
    tmp = strsplit(content(GET(url)),"\n")
    tmp1 = list(1:length(tmp[[1]]))
    tmp2 = list(1:length(tmp[[1]]))
    for(i in 1:length(tmp[[1]])){
      tmp1[[1]][i] = strsplit(tmp[[1]][i],"\t")[[1]][1]
      tmp2[[1]][i] = strsplit(tmp[[1]][i],"\t")[[1]][2]
    }
    res = data.frame(tmp1[[1]],tmp2[[1]],stringsAsFactors = F)
    return(res)
  }
}
