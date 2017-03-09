list2dataframe <- function(LST,
                           tomerge = 'name',         # which keys to merge to one
                           todelete = NULL,          # which keys to delete
                           toquantity = 'children'   # which keys to substutute with its quantity
){

  # merge the list to a string
  if(!is.null(tomerge)){
    for(n in 1:length(LST)){
      for(mer in tomerge)
        LST[[n]][[mer]] = str_c(LST[[n]][[mer]], collapse = "; ")
    }
  }

  # substitute list to its quantity(length), rename later
  if(!is.null(toquantity) & !is.null(LST[[1]][[toquantity]])){
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
  dt = do.call(rbind, lapply(LST, as.data.frame, stringsAsFactors = FALSE))



  # rename the column toquantity
  colnames(dt) = str_replace(colnames(dt),
                             str_c("^", toquantity, "$"),
                             str_c(toquantity, "NO."))

  # rename rows the abbreviatory NAME, for convenience to index
  if(!is.null(LST[[1]]$name) & any(str_detect(dt[['name']], "; "))){
    abbrName = str_match(dt[['name']], "([^;]+)(?:; |$)")[,2]
    # set row names if abbrNames are unique
    if(length(unique(abbrName)) == nrow(dt))
      rownames(dt) = abbrName
  }
  return(dt)
}
