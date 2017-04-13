#' Search Reactome Database
#'
#' Search some keys, retrieves faceting information, or get search suggests.
#'
#' @usage
#' # query
#' rtSearch(query, faceting = FALSE, species = 'Homo sapiens', types = 'pathway',
#'   compartments_to_filter = NULL, reaction_types_to_filter = NULL, silent = FALSE)
#' # faceting
#' rtSearch(query = NULL, faceting = TURE, species = 'Homo sapiens', types = 'pathway',
#'   compartments_to_filter = NULL, reaction_types_to_filter = NULL,
#'   cluster = TRUE, Start_row = NULL, number_of_rows_to_include = NULL, silent = FALSE)
#' @param query string. Search term.
#' @param faceting logical. Retrieve faceting information.
#' @param species string. Species names.
#' @param types string. Types to filter.
#' @param compartments_to_filter string. Compartments to filter.
#' @param reaction_types_to_filter string. Reaction types to filter.
#' @param cluster logical. Cluster results.
#' @param start_row integer. Start row.
#' @param number_of_rows_to_include integer. Number of rows to include.
#' @param silent logical. Run quitely.
#'
#' @details
#' \code{facet}: retrieves faceting information on the specific queries, or
#'  the whole Reactome search data(\code{query = NULL}).
#' \code{query}: performs a Solr query on the Reactome knowledgebase.
#'  Results can be provided in a paginated format.
#' @return A datafram or list.
#'
#' @examples
#' # Search facet information
#' facet.2 = rtSearch('apoptosis', species = c('Homo sapiens', 'Gallus gallus'),
#'             types = c('Pathway', 'Reaction'), faceting = T)
#' # facets in the whole database
#' facet.all = rtSearch(faceting = T)
#'
#' # Search keys of Human
#' s.1 = rtSearch(query = 'apoptosis', types = c('Pathway', 'Reaction'))
#' @include includes.R
#' @export
#' @rdname search
#'
#'
rtSearch <- function(query = NULL,
                     species = 'Homo sapiens',
                     types = 'Pathway',
                     faceting = FALSE,
                     compartments_to_filter = NULL,
                     reaction_types_to_filter = NULL,
                     cluster = TRUE,
                     start_row = NULL,
                     number_of_rows_to_include = NULL,
                     silent = FALSE){

  # check the parameters
  if(!is.logical(faceting) || (is.null(query) && !faceting))
    error('Error in parameters.')

  params = list(query, species, types, compartments_to_filter, reaction_types_to_filter)
  tmp = lapply(params, pretreat_param)
                     query = tmp[[1]]
                   species = tmp[[2]]
                     types = tmp[[3]]
    compartments_to_filter = tmp[[4]]
  reaction_types_to_filter = tmp[[5]]
  if(faceting){
    # search facet
    return(s_facet(query, species, types,
           compartments_to_filter, reaction_types_to_filter, silent))
  }
  else{
    # search key
    return(s_query(query, species, types,
           compartments_to_filter, reaction_types_to_filter ,
           cluster, start_row, number_of_rows_to_include, silent))
  }

}

pretreat_param <- function(str){
  if(length(str) > 1)
    str = str_c(str, collapse = ', ')
  # substitute backspace to '%20'
  str = str_replace_all(str, '\\s', '%20')
  # ',;' to '%2C'
  str = str_replace_all(str, '[,;]', '%2C')
  return(str)
}

s_facet <- function(query, species, types, compartments_to_filter,
                    reaction_types_to_filter, silent){


  prestr = list('_query?query=', '&species=', '&types=', '&Compartments%20to%20filter=',
             '&Reaction%20types%20to%20filter=')
  params = list(query, species, types, compartments_to_filter, reaction_types_to_filter)

  # all facet, default
  url = str_c(getURL(), '/facet')
  extract_key = 'available'
  if(length(query) > 0){
    prestr_exist = sapply(params, length) > 0
    url = str_c(url, str_c(prestr[prestr_exist], params[prestr_exist], collapse = ''))
    extract_key = 'selected'
  }
  lst = url2dataframe(url, bindToSingleDataframe = F, silent = silent)
  lst.2 = extract_facet(lst[-1], extract_key)
  dt.2 = do.call(rbind, lst.2)
  # add column 'facet'
  dt.1 = data.frame(facet = str_replace(rownames(dt.2), '\\.\\d+', ''),
                    stringsAsFactors = F)
  dt = cbind(dt.1, dt.2)
  rownames(dt) = NULL

  return(dt)
}

extract_facet <- function(lst,
                          key # extract key 'selected' or 'available'
                          ){
  return(lapply(lst, '[[', key))
}

getURL <- function(){
  'http://www.reactome.org/ContentService/search'
}


s_query <- function(query, species, types,
                    compartments_to_filter, reaction_types_to_filter ,
                    cluster, start_row, number_of_rows_to_include, silent){

  prestr = list('/query?query=', '&species=', '&types=', '&Compartments%20to%20filter=',
                '&Reaction%20types%20to%20filter=', '&cluster=', '&Start%20row=',
                '&Number%20of%20rows%20to%20include=')
  params = list(query, species, types, compartments_to_filter, reaction_types_to_filter,
                str_to_lower(cluster), start_row, number_of_rows_to_include)
  prestr_exist = sapply(params, length) > 0
  url = str_c(getURL(), str_c(prestr[prestr_exist], params[prestr_exist], collapse = ''))

  lst = url2dataframe(url, bindToSingleDataframe = F, silent = silent)
  lst = lst$results$entries
  dt = lst[[1]]
  for(dt0 in lst[-1]){
    dt = bindbyrow(dt, dt0)
  }

  # remove HTML tags
  rm = c('name', 'summation')
  dt[rm] = lapply(dt[rm], remove_HTML_tags)

  return(dt)

}

remove_HTML_tags <- function(str){
  str_replace_all(str, '<[^>]*>', '')
}

#' @rdname search
#' @export
#' @details
#' \code{rtSuggest} gives spell-check suggestions and search suggestions
#' for given search terms
#' @examples
#' # search suggestions
#' sug = rtSuggest(c('appoptosis', 'apoptos'))
#'
rtSuggest <- function(query, silent = FALSE){
  # split key
  if(length(query) == 1)
    query = str_split(query, '[,;]')[[1]]

  lst = lapply(query, suggest_single_key, silent = silent)
  dt.1 = data.frame(inputKey =query)
  dt.2 = list2col_dt(lst)
  names(dt.2) = 'suggestion'
  dt = cbind(dt.1, dt.2)
  return(dt)
}

suggest_single_key <- function(key, silent){
  url1 = str_c(getURL(), '/spellcheck?query=', key)
  url2 = str_c(getURL(), '/suggest?query=', key)
  str1 = url2dataframe(url1, bindToSingleDataframe = F, silent = silent)
  str2 = url2dataframe(url2, bindToSingleDataframe = F, silent = silent)

  str = c(str1, str2)
  if(!is.list(str))
    str = list(str)
  return(str)
}
