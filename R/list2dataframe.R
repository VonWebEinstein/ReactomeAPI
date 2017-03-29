list2dataframe <- function(LST,
                           tomerge = 'name',         # which keys to merge to one
                           todelete = NULL,          # which keys to delete
                           toquantity = 'children'   # which keys to substutute with its quantity
){

  # merge the list to a string
  if(!is.null(tomerge)){
    for(n in 1:length(LST)){
      for(mer in tomerge)
        LST[[n]][[mer]] = list(as.character(LST[[n]][[mer]]))
          # str_c(LST[[n]][[mer]], collapse = "; ")
    }
  }

  # substitute list to its quantity(length), rename later
  if(!is.null(toquantity) && !is.null(LST[[1]][[toquantity]])){
    for(n in 1:length(LST))
      LST[[n]][[toquantity]] = length(LST[[n]][[toquantity]])
  }

  # to delete
  if(!is.null(todelete)){
    for(n in 1:length(LST))
      LST[[n]][[todelete]] = NULL
  }


  # check keys in every node
  KEYS = unique(unlist(sapply(LST, names)))
  if(!all(sapply(LST, length) == length(KEYS))){
    for(n in 1:length(LST)){
      missingNames = KEYS[!(KEYS %in% names(LST[[n]]))]
      LST[[n]][missingNames] = NA
    }
  }
  # convert to dataframe
  dt = do.call(rbind, lapply(LST, singleLine2dataframe))



  # rename the column toquantity
  if(!is.null(toquantity))
    colnames(dt) = str_replace(colnames(dt),
                             str_c("^", toquantity, "$"),
                             str_c(toquantity, "NO."))

  # rename rows the abbreviatory NAME, for convenience to index
  # if(!is.null(LST[[1]]$name) & any(str_detect(dt[['name']], "; "))){
  #   abbrName = str_match(dt[['name']], "([^;]+)(?:; |$)")[,2]
  #   # set row names if abbrNames are unique
  #   if(length(unique(abbrName)) == nrow(dt))
  #     rownames(dt) = abbrName
  # }
  return(dt)
}

# convert a list to dataframe with single obs.
singleLine2dataframe <- function(LST){
  return(Line2dt(LST)[[1]])
}

Line2dt <- function(LST){
  if(0 == length(LST))
    return(NA)
  if(is.data.frame(LST))
    return(list(LST))

  dt = data.frame(t(1:length(LST)))
  for(n in 1:length(LST)){
    UNLIST = untiList(LST[[n]])
    if(length(UNLIST) == 0){
      dt[[n]] = NA
      next
    }
    if(is.list(UNLIST) && !is.null(names(UNLIST)))
      dt[[n]] = Line2dt(UNLIST)
    else{
      if(length(LST[[n]]) > 1)
        dt[[n]] = list(LST[[n]])
      else
        dt[[n]] = LST[[n]]
    }
  }
  colnames(dt) = names(LST)
  return(list(dt))
}

untiList <- function(LST){
  if(is.list(LST) && length(LST) == 1 && is.list(LST[[1]]))
    return(LST[[1]])
  return(LST)
}
