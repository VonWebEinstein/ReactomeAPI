#' Disease related queries
#'
#' It retrieves the dataframe of disease or disease DOIDs annotated in Reactome
#'
#'
#' @param DOID_only logical. \code{FALSE}: return DOIDs of diseases only.
#' \code{TRUE}: return detailed infomation.
#'
#' @return a dataframe
#' @export
#' @examples
#' diseases = reactomeDiseases()
#' diseases_doid = reactomeDiseases(TRUE)
#'
#' @import stringr
#' @import httr
#' @import jsonlite
#' @rdname disease
#'
#'
#'
reactomeDiseases <- function(DOID_only = FALSE){
  url = 'http://www.reactome.org/ContentService/data/diseases'
  if(DOID_only){
    url = str_c(url, '/doid')
    str = rawToChar(GET(url)$content)
    str = str_split(str, '[\\n|\\t]')
    dt = as.data.frame(t(matrix(str[[1]], nrow = 2)), stringsAsFactors = FALSE)
    colnames(dt) = c('ID', 'DOID')

    return(dt)
  }
  else{
    return(fromJSON(url))
  }
}
# reactomeDiseases <- function(DOID_only = FALSE) {
#   url = 'http://www.reactome.org/ContentService/data/diseases'
#   if(!DOID_only)
#     res = fromJSON(url)
#   else{
#     tmp = strsplit(as.character(content(GET(str_c(url,"/doid")))),"\n")
#     tmp = strsplit(tmp[[1]], '\t')
#     l = length(tmp)
#     tmp1 = list(1:l)
#     tmp2 = list(1:l)
#     # for(i in 1:l){
#     #     tmp1[[1]][i] = strsplit(as.character(tmp[[1]][i]),"\t")[[1]][1]
#     #     tmp2[[1]][i] = strsplit(as.character(tmp[[1]][i]),"\t")[[1]][2]
#     #   }
#     # tmp3 = list(1:l)
#     # for(i in 1:l){
#     #     tmp3[[1]][i] = strsplit(as.character(tmp2[[1]][i]),":")[[1]][2]
#     #   }
#     for(i in 1:l){
#       tmp1[[1]][i] = tmp[[i]][1]
#       tmp2[[1]][i] = tmp[[i]][2]
#     }
#     res = data.frame(tmp1[[1]],tmp2[[1]],stringsAsFactors = F)
#     return(res)
#   }
# }
