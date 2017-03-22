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
#' @examples schema_class = rtSchema(schemaclass = "pathway",species = "9606",specified = "detail",page = 1,offset = 25,silent = FALSE)
#'
#' @include error.R
#' @import jsonlite
#' @import stringr
#' @import httr
#' @export
#' @rdname schema
#'

reactomeSchema <- function(schemaclass = "Pathway",
                           species = "9606",
                           specified = "detail", # detail simplified count or reference
                           page = 1,
                           offset = 25,
                           silent = FALSE){
  tmp_url = 'http://www.reactome.org/ContentService/data/schema/'
  url = switch(specified,
                detail = str_c(tmp_url,schemaclass,"?","species=",species,"&","page=",page,"&","offset=",offset),
                simplified = str_c(tmp_url,schemaclass,"/","min","?","species=",species,"&","page=",page,"&","offset=",offset),
                count = str_c(tmp_url,className,"/","count","?","species=",species),
                reference = str_c(tmp_url,className,"/","reference","?","page=",page,"&","offset=",offset))
  res = try(fromJSON(url), silent = TRUE)
  if(inherits(res, "try-error")){
    tmp = content(GET(url, content_type_json()))
    return(errorMessage(tmp, silent = silent))
  }
  else{
      return(res)
    }

}

#' @export
#' @rdname schema

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


