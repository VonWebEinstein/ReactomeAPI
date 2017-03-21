#' Disease related queries
#'
#' It retrieves the dataframe of disease or disease DOIDs annotated in Reactome
#'
#' @description retrieves the dataframe of disease DOIDs annotated in Reactome
#'
#' @usage reactomeDiseases("diseases")
#' or rectomeDiseases("diseases/doid")
#'
#' @param dataType a chacter string that specify the data type. can be \code{diseasw}
#' or \code {disease/doid}
#'
#' @details 'dataType = disease ' It retrieves the dataframe of diseases annotated in Reactome
#' 'dataType = disease/doid',It retrieves the dataframe of disease DOIDs annotated in Reactome
#' Its Response Class (Status 200) is a string,else if Response Messages HTTP Status
#' \Code{406},the reason is Not acceptable according to the accept headers sent in the request
#' \code{500},Internal Server Error
#'
#' @return a dataframe
#' @export
#' @examples
#' reactomeDiseases("diseases")
#' reactomeDiseases("diseases/doid")
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
    tmp = strsplit(as.character(content(GET(url))),"\n")
    l = length(tmp[[1]])
    tmp1 = list(1:l)
    tmp2 = list(1:l)
    for(i in 1:l){
      tmp1[[1]][i] = strsplit(as.character(tmp[[1]][i]),"\t")[[1]][1]
      tmp2[[1]][i] = strsplit(as.character(tmp[[1]][i]),"\t")[[1]][2]
    }
    tmp3 = list(1:l)
    for(i in 1:l){
      tmp3[[1]][i] = strsplit(as.character(tmp2[[1]][i]),":")[[1]][2]
    }
    res = data.frame(tmp1[[1]],tmp3[[1]],stringsAsFactors = F)
    colnames(res) = c("tmp","DOID")
    return(res)
  }
}
