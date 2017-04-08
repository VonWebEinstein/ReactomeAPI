#' Database info queries
#'
#' Return the name and version number of current database.
#' @return a dataframe.
#' @seealso \href{http://www.reactome.org}{\code{Reactome}}
#'
#' @export
#' @rdname database

reactomeDatabase <- function(){
  url = 'http://www.reactome.org/ContentService/data/database/'
  tmp = lapply(c('name', 'version'),
              function(str) content(GET(paste(url, str, sep = ''))))
  dt = data.frame(name = tmp[[1]], version = tmp[[2]], stringsAsFactors = F)
  cat(str_c(dt$name, '\n', dt$version))
}

#' @export
#' @rdname database
rtDatabase <- function(){
  return(reactomeDatabase())
}
