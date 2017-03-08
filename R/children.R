#' List the children of something in a list tree
#'
#' In a tree(e.g. a hierarchical structure for some species) represtented
#' by a nested list. \code{children} lists the children of \code{whose} as
#' a dataframe.
#'
#' @export
#' @import stringr
#' @rdname children
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
      return(children(whose, child$children, key = key))
      #return(lapply(where[key], function(x) children(whose, x)))
    }

  }
  cat(whose, "NOT FOUND")
  return(FALSE)

}

#' @export
#' @rdname children
#' @param obj list. the father node
childrenof <- function(obj){
  if(is.null(obj$children)){

    return(TRUE)
  }
  else{
    # browser()
    return(list2dataframe(obj$children))
  }
}

list2dataframe <- function(LST,
                           tomerge = 'name',         # which keys to merge to one
                           todelete = NULL,          # which keys to delete
                           toquantity = 'children'   # which keys to substutute with its quantity
                           ){

  # merge the list to a string
  if(!is.null(tomerge)){
    for(n in 1:length(LST)){
      LST[[n]][[tomerge]] = str_c(LST[[n]][[tomerge]], collapse = "; ")
    }
  }

  # substitute list to its quantity(length), rename later
  if(!is.null(toquantity) & !is.null(LST[[1]][[toquantity]])){
    for(n in 1:length(LST))
      LST[[n]][[toquantity]] = length(LST[[n]][[toquantity]])
  }

  # convert to dataframe
  dt = do.call(rbind, lapply(LST, as.data.frame, stringsAsFactors = FALSE))

  # to delete
  if(!is.null(todelete)){
    dt[todelete] = NULL
  }

  # rename the column toquantity
  colnames(dt) = str_replace(colnames(dt),
                             str_c("^", toquantity, "$"),
                             str_c(toquantity, "NO."))

  # rename rows the abbreviatory NAME, for convenience to index
  if(!is.null(LST[[1]]$name) & any(str_detect(dt[['name']], "; "))){
    abbrName = str_match(dt[['name']], "([^;]+)(?:; |$)")[,2]
    rownames(dt) = abbrName
  }
  return(dt)
}
