#' Database Info Quenries
#'
#' Return the name and the version of current database
#'
#' @return A dataframe.
#'
#'
#'
#' @export
#' @import stringr
#' @import httr
#' @rdname database
#'
#'
reactomeDatabase<-function(){
  url = "http://www.reactome.org/ContentService/data/database/"
  data_nv = c("name","version")
  tmp = lapply(data_nv,function(str) content(GET(str_c(url,str))))
  name_version = data.frame(tmp[[1]],tmp[[2]],stringsAsFactors = FALSE)
  colnames(name_version) = data_nv
  return(name_version)
}





