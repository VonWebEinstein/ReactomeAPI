
POST_Method <- function(url = NULL,    # url of the page to retrieve
                        body = NULL,   # POST body
                        # a funcion handle the return value from fromJSON
                        rework = function(s) s,
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
    return(errorMessage(url, silent = silent))
  dt = fromJSON(rawToChar(postObj$content))

  return(rework(dt))

}
