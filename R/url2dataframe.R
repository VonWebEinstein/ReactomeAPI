
#' @include error.R
#' @import jsonlite
#' @import httr
#' @import stringr
#'
url2dataframe <- function(urlComponent,
                          bindToSingleDataframe = TRUE,
                          silent = FALSE,
                          rework = function(s) s){

  url = str_c(urlComponent, collapse = '/')
  res = try(fromJSON(url), silent = TRUE)
  if(inherits(res, "try-error")){
    tmp = content(GET(url, content_type_json()))
    return(errorMessage(tmp, silent = silent))
  }
  else{
    # bind dataframes to single one
    if(bindToSingleDataframe && !is.data.frame(res)){
      res = do.call(rbind, res)
    }
    return(rework(res))
  }

}
