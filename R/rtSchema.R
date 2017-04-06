#' Schema class queries
#'
#' This method retrieves the dataframe of entries in Reactome
#' that belong to the specified schema class. Or retrieves
#' counts the total number of entries in Reactome that belong to
#' the specified schema class. Or retrieves the dataframe of simplified
#' entries in Reactome that belong to the specified schema class.
#' Or retrieves the dataframe of simplified reference objects that belong
#' to the specified schema class.
#'
#' @param schemaclass string. Schema class name.
#' @param species string. Allowed species filter: SpeciesName
#' (eg: Homo sapiens) SpeciesTaxId (eg: 9606).
#' @param specified string. "detail", "simplified", "count" or "reference".
#' @param Page integer. Page to be returned.
#' @param offset integer. Number of rows returned.
#'
#' @return dataframe or number.
#'
#' @examples schema_class = rtSchema(schemaclass = "Pathway",species = "9606",specified = "detail",page = 1,offset = 25,silent = FALSE)
#'
#' @include url2dataframe.R
#' @include error.R
#' @export
#' @rdname schema
#'
# specified -> type
# 增大offset上限，增加返回所有结果选项
# 或者改为请求结果的范围 c(1,100)
# (1,-1)返回所有结果，告诉用户返回结果z总项数，更新过cheng中返回更新位置
reactomeSchema <- function(schemaclass = "Pathway",
                           species = "9606",
                           type = "detail", # detail summary count or reference
                           intervalOFpathway = c(1,10),
                           silent = FALSE){
  number = dataframeResPage(type = 'count')
  #let offset = 25 , calculate page under interval of pathway

  # # let user choose tocontinue or not
  if((intervalOFpathway[1] == 1)&(intervalOFpathway[2] == -1)){
    return(schema(interval = c(1, number)))

  #   tmp = str_c("The count of ",
  #               schemaclass,
  #               " that you ask is ",
  #               as.character(number),
  #               ", continue : input 1, end : input 0?")
  #   choiceOFuser = edit(tmp)
  #   if(choiceOFuser == 0)
  #     return('results too much, choose to break!')
  #   else{
  #     res = schema(interval = c(1,number))
  #     return(res)
  #   }
  }

  return(schema(interval = intervalOFpathway))
}

# return the interval dataframe that user ask
schema <- function(interval){
  page = lapply(interval, function(x) (floor((x-1) / 25) + 1))
  dataframeRes = lapply(page[[1]]:page[[2]],
                        function(x) dataframeResPage(page = x, total_pages = page[[2]]))
  KEYS = unique(unlist(sapply(dataframeRes, names)))
  if(!all(sapply(dataframeRes, length) == length(dataframeRes))){
    for(n in 1:length(dataframeRes)){
      missingNames = KEYS[!(KEYS %in% names(dataframeRes[[n]]))]
      dataframeRes[[n]][missingNames] = NA
    }
  }
  res = do.call(rbind, dataframeRes)
  l = interval[1]
  r = interval[2]
  return(res[l:r,])
}



#  the dataframe response page = x
dataframeResPage <- function(schemaclass = 'Pathway',
                             species = "9606",
                             type = 'detail', # detail summary count or reference
                             page = 1,
                             offset = 25,
                             total_pages = 0,
                             silent = FALSE){
  tmp_url = 'http://www.reactome.org/ContentService/data/schema/'
  tmp_url = str_c(tmp_url,schemaclass)
  # 再组织一下
  detail_url = str_c("?species=", species, "&page=", page, "&offset=", offset)
  summary_url = str_c("/min?species=", species, "&page=", page, "&offset=", offset)
  count_url = str_c("/count?species=", species)
  reference_url = str_c("/reference?page=", page, "&offset=", offset)

  # url的组织 不简洁
  if(type == 'count'){
    count_url = str_c(tmp_url,count_url)
    res = fromJSON((count_url))
    return(res)
  }
  else{
    url = switch(type,
                 detail = str_c(tmp_url,detail_url),
                 simplified = str_c(tmp_url,summary_url),
                 reference = str_c(tmp_url,reference_url))
    # 最好显式返回
    res = url2dataframe(urlComponent = url)
    cat(printRateOfProcess(page, total_pages))
    return(res)
  }
}

printRateOfProcess <- function(page, total_pages){
  l = nchar(printed_info((page-1)/total_pages))
  str_sub(printed_info(page/total_pages), l+1, -1)
}

# printRateOfProcess <- function(page, total_pages){
#   rate = sapply(1:total_pages, function(x) x/total_pages)
#   add_printed = printed_info(rate[page])
#   cat(add_printed)
# }


printed_info <- function(ratio){
  r1 = floor(100*ratio/20)
  r2 = floor((100*ratio-20*r1)/2)
  str2 = str_c(replicate(r2, '.'),collapse = '')
  if(r1 > 0){
    str1 = str_c(lapply(1:r1, function(n) sprintf('.......... %d%% complete\n', 20*n)),collapse = '')
    return(str_c(str1, str2))
  }
  else{
    return(str2)
  }

}


#' @export
#' @rdname schema
# @usage rtSchema(...)
rtSchema <- function(schemaclass = "Pathway",
                     species = "9606",
                     specified = "detail", # detail simplified count or reference
                     page = 1,
                     offset = 25,
                     silent = FALSE){
  return(reactomeSchema(schemaclass = schemaclass,
                        species = specified,
                        specified = specified,
                        page = page,
                        offset = offset,
                        silent = silent))
}


