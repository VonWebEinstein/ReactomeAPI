#' Database Info Quenries
#'
#' Return the name and the version of current database
#'
#' @describeIn Return the name and the version of current database
#'
#' @usage reactomeDatabase()
#'
#' @details Its Response Class (Status 200) is a string,else if Response Messages HTTP Status
#' \Code{406},the reason is Not acceptable according to the accept headers sent in the request
#' \code{500},Internal Server Error
#'
#' @return the name and the version of current database
#'
#' @example x = reactomeDatabase()
#'
#' @import stringer
#' @import httr
#' @rdname disease
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





