#' Common data retrieval
#'
#' Query Reactome database objects.
#' @usage rtQuery(id, attributeName, ...)
#' rtQuery(id, type = "summary", ...)
#'
#' @param id string. A comma separated list of DbIds or StIds or character vector.
#' @param type Data of type: \code{summary}, \code{detail}, \code{extended}, \code{more},
#'   or \code{withmapping}.
#' @param attributeName string. Attribute(s) of Reactome
#'   knowledgebase to be filtered.
#' @param directParticipants logical. Include direct participants
#'   (proteins or molecules directly involved in Reactions)
#' @param silent logical. Run quitely.
#'
#' @details
#' \itemize{
#'   \item \code{detail}: Query for an entry in Reactome knowledgebase based on
#'   the given identifier, i.e. stable id or database id. It is worth mentioning
#'   that the retrieved database object has all its properties and direct
#'   relationships (relationships of depth 1) filled.
#'   \item \code{summary}: Only requests containing up to 20 ids are processed.
#'   \item \code{extended}: ContentDetails contains: componentsOf,
#'   other forms of the entry, locationsTree,except DatabaseObject.
#'   \item \code{more}: Query for an entry in Reactome knowledgebase
#'   providing more information. In particular, the retrieved database object
#'   has all its properties and direct relationships (relationships of depth 1)
#'   filled, while it also includes any second level relationships regarding
#'   regulations and catalysts.
#'   \item \code{withmapping}: This method is particularly useful for users
#'   that still rely on the previous version of stable identifiers to query
#'   this API. Please note that those are no longer part of the retrieved objects.
#'   Only requests containing up to 20 ids are processed.
#'   }
#' @return A dataframe.
#' @examples
#' # detailed info
#' dt.detail = rtQuery('R-HSA-1640170', type = 'detail')
#'
#' # summary of multiple ids and attributes
#' dt.mul = rtQuery(id = c('R-DME-1640170,R-HSA-1640170'),
#'                  attributeName = c('displayname', 'figure'))
#' @export
#' @include list2dataframe.R
#' @include url2dataframe.R
#' @include POST_Method.R
#' @include error.R
#'
#' @rdname query
#'
rtQuery <- function(id,
                    type = "summary",
                    attributeName = NULL,
                    directParticipants = FALSE,
                    silent = FALSE){
  # split ids to atomic vector
  if(1 == length(id)){
    id = str_split(id, '[,;\\s]')[[1]]
  }
  # query_attribute
  if(!is.null(attributeName))
    return(query_attribute(id, attributeName, silent))

  dt = switch(type,
    summary     = query_summary(id, silent),
    detail      = query_detail(id, silent),
    extended    = query_extended(id, directParticipants, silent),
    more        = query_more(id, silent),
    withmapping = query_withmapping(id, silent)
  )
  return(dt)
}


query_summary <- function(id, silent){
  url = list(GETURL(), 'ids')
  id = str_c(id, collapse = ',')
  POST_Method(url = url, body = id, silent = silent)
}

query_detail <- function(id, silent){
  GET_multiIDs(id, silent, NULL)
  # LST = lapply(id, function(x) url2dataframe(list(GETURL(), x), FALSE, silent))
  # list2dataframe(LST, NULL,NULL,NULL)
}

query_extended <- function(id, directParticipants, silent){
  url_last = NULL
  if(is.logical(directParticipants)){
    if(directParticipants)
      url_last = 'extended?directParticipants=true'
    else
      url_last = 'extended?directParticipants=false'
  }
  LST = lapply(id, function(x) url2dataframe(list(GETURL(), x, url_last), FALSE, silent)[-1])
  list2dataframe(LST, NULL,NULL,NULL)
}

query_more <- function(id, silent){
  GET_multiIDs(id, silent, "more")
  # LST = lapply(id, function(x) url2dataframe(list(GETURL(), x, "more"), FALSE, silent))
  # list2dataframe(LST, NULL, NULL,NULL)
}

query_withmapping <- function(id, silent){
  url = list(GETURL(), "ids/map")
  id = str_c(id, collapse = ',')
  LST = POST_Method(url = url, body = id, silent = silent)
  list2dataframe(LST, NULL,NULL,NULL)
}

GET_multiIDs <- function(id, silent, url0){
  if(is.null(url0)) url = str_c(GETURL(), id, sep = '/')
  else url = str_c(GETURL(), id, url0, sep = '/')
  LST = lapply(url, function(x) url2dataframe(x, FALSE, silent))
  list2dataframe(LST, NULL,NULL,NULL)
}

GETURL <- function(){
  'http://www.reactome.org/ContentService/data/query'
}

# for multiple ids and attrs
query_attribute <- function(id, attributeName, silent){
  dt_right = data.frame(t(1:length(attributeName)))
  names(dt_right) = attributeName
  dt_tmp = lapply(attributeName, function(x) query_single_attr(id, x, silent))
  dt_right = do.call(cbind, dt_tmp)
  # dt_right[] = lapply(attributeName, function(x) query_single_attr(id, x, silent))
  # supplement 'inputId'
  dt_left = data.frame(inputId = id, stringsAsFactors = F)
  return(cbind(dt_left, dt_right))
}

# for single attr multiple ids
query_single_attr <- function(id, attributeName, silent){

  # attrs with dbid
  dbid_ns = c('created','modified','evidenceType','species',
              'summation','hasEvent','edited','figure','orthologousEvent')
  # attrs with stid
  stid_ns = c('inferredFrom')
  # attrs witch are column vector
  col_ns = c('name')
  # attrs which are dataframe
  dt_ns = c('hasEvent', 'orthologousEvent')
  # default attr names
  ns = c('dbId','displayName','className')

  # which name set attr belongs to
  attr_which_set = sapply(list(dbid_ns, stid_ns, col_ns, dt_ns),
                          function(x) attributeName %in% x)
  names(attr_which_set) = c('dbid_ns', 'stid_ns', 'col_ns', 'dt_ns')

  if(any(attributeName %in% stid_ns))
    ns[1] = 'stId'
  if(!any(attributeName %in% c(dbid_ns, stid_ns)))
    ns = NULL

  dt = lapply(id, function(x)
    query_attr_atom(str_c(GETURL(), x, attributeName, sep = '/'),
    ns, attr_which_set, silent))
  # dataframe is different to bind
  if(!is.null(ns))
    dt = list2col_dt(dt)
  else
    dt = data.frame(unlist(dt), stringsAsFactors = FALSE)
  names(dt) = attributeName
  return(dt)

}

list2col_dt <- function(LST){
  N = length(LST)
  dt = data.frame(1:N)
  for(n in 1:N)
    dt[n,1] = LST[n]
  return(dt)
}

# for single id single attr
query_attr_atom <- function(url, ns, attr_which_set, silent){
  GET_object = GET(url)
  str = content(GET_object)
  if(!GET_object$status_code == 200){
    tmp = errorMessage(str, silent)
    return(NA)
  }

  # error
  if(is.logical(str)) return(NA)
  # no colnames
  if(is.null(ns)){
    # attr: name
    if(attr_which_set['col_ns'])
      return(str_split(str, '\\s'))
    # scalar
    else return(str)
  }
  # with names
  if(!is.null(ns)){
    # vector
    if(!any(attr_which_set['dt_ns'])){
      dt = data.frame(t(str_split(str, '\\t')[[1]]), stringsAsFactors = FALSE)
    }
    # dataframe
    else{
      s1 = str_split(str, '\\n')[[1]]
      s2 = str_split(s1, '\\t', simplify = T)
      dt = data.frame(s2, stringsAsFactors = F)
    }
    names(dt) = ns
    return(list(dt))
  }

}

#' @details \code{rtGetImage} gets images of reactome objects to directory
#' \code{~/ReacromeImage/}.
#' @rdname query
#' @export
#' @include tools.R
#' @examples
#' # download images
#' # NA means no figure.
#' images = rtGetImage(c('R-DMA-1640170','R-HSA-1640170', 'R-HSA-69620'))
#'

rtGetImage <- function(id){
  # creat directory if doesn't exist
  dir = str_c(getwd(), "/ReactomeImage/")
  validateAndCreatDir(dir)

  addr = rtQuery(id, attributeName = 'figure', silent = T)

  addr['url'] = sapply(addr[['figure']], extract_url)
  addr['figureName'] = str_match(addr[['url']], '/([^/]+\\.[^/]+)$')[,2]

  # download
  for(n in 1:nrow(addr)){
    if(is.na(addr[n, 'url']))
      next
    download.file(     url = addr[n,'url'],
                  destfile =  str_c(dir, addr[n,'figureName']),
                      mode = 'wb')
  }

  return(addr[c('inputId', 'figureName')])

}

extract_url = function(dt){
  if(all(is.na(dt))) return(NA)
  url = dt[1,'displayName']
  return(str_c('http://www.reactome.org', url))
}



