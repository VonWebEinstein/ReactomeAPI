#' List the children of something in a dataframe tree
#'
#' In a tree(e.g. a hierarchical structure for some species) represtented
#' by a nested dataframe. \code{children} lists the children of \code{whose}.
#'
#' @export
#' @import stringr
#' @include list2dataframe.R
#' @rdname eventHierarchy
#' @param whose string. an idendifier indicating \code{key}
#' @param where list. the list tree including \code{whose}
#' @param key string. name we search depends on
children <- function(whose, where, key = 'stId'){
  s = sapply(where, '[', key)

  # where is the key
  nth = (1:length(s))[s == whose]
  if(length(nth) > 0){
      tmp = childrenof(where[[nth]])
      if(is.logical(tmp)){
        cat(whose, "has NO CHILDREN")
        return(tmp)
      }
      return(tmp)
  }
  else{
    # search the children of children of where
    for(child in where){
      # leaf node
      if(is.null(child$children))
        next
      # return when got a dataframe or TRUE
      dt = children(whose, child$children, key = key)
      if(!is.logical(dt))
        return(dt)
      else{
        if(dt)
          return(dt)
      }

    }

  }
  # cat(whose, "NOT FOUND")
  return(FALSE)

}


childrenof <- function(obj){
  if(is.null(obj$children)){

    return(TRUE)
  }
  else{
    # browser()
    return(list2dataframe(obj$children))
  }
}


