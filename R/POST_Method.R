
#' @include error.R
POST_Method <- function(url = NULL,    # url of the page to retrieve
                        body = NULL,   # POST body
                        # a funcion handle the return value from fromJSON
                        rework_func = function(s) s,
                        # chenck the result of fromJSON is empty of not
                        checkempty_func = function(x) FALSE,
                        silent = FALSE # run quitely

){
  # paste url, do nothing when url is scalar
  url = str_c(url, collapse = '/')
  postObj = POST(url = url,
                 body = body,
                 content_type("text/plain"),
                 accept_json())
  # return error
  if(!postObj$status_code == 200)
    return(errorMessage(content(postObj), silent = silent))
  dt = fromJSON(rawToChar(postObj$content))
  if(checkempty_func(dt)){
    if(!silent) cat('Empty result')
    return(FALSE)
  }
  return(rework_func(dt))

}
