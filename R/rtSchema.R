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
#' @param classname string. Schema class name
#' @param species string. Allowed species filter: SpeciesName
#' (eg: Homo sapiens) SpeciesTaxId (eg: 9606)
#' @param Page integer. Page to be returned.
#' @param offset integer. Number of rows returned.
#'
#' @return dataframe or number.
#'
#' @examples schema_class = rtSchema(className = "pathway",species = "9606",page = 1,offset = 25,schema = FALSE)
#'
#' @include error.R
#' @import jsonlite
#' @import stringr
#' @import httr
#' @export
#' @rdname schema
#'

reactomeSchema <- function(className = "Pathway",
                           species = "9606",
                           page = 1,
                           offset = 25,
                           schema = FALSE,
                           class = "count",
                           silent = FALSE){
  url = 'http://www.reactome.org/ContentService/data/schema/'
  if(!schema){
    tmp_url = switch(class,
                     count = str_c(url,className,"/",class,"?","species=",species),
                     min = str_c(url,className,"/",class,"?","species=",species,"&","page=",page,"&","offset=",offset),
                     reference = str_c(url,className,"/",class,"?","page=",page,"&","offset=",offset))
  }
  else
    tmp_url = str_c(url,className,"?","species=",species,"&","page=",page,"&","offset=",offset)
  res = try(fromJSON(tmp_url), silent = TRUE)
  if(inherits(res, "try-error")){
    tmp = content(GET(tmp_url, content_type_json()))
    return(errorMessage(tmp, silent = silent))
  }
  else{
      return(res)
    }

}

#' @export
#' @rdname schema

rtSchema <- function(className = "Pathway",
                     species = "9606",
                     page = 1,
                     offset = 25,
                     schema = FALSE,
                     class = "count",
                     silent = FALSE){
  return(reactomeSchema(className,
                        species,
                        page,
                        offset,
                        schema,
                        class,
                        silent))
}


