#' Reactome Data: Orthology related queries
#'
#' Retrieves the orthologie for any given set of events or entities in the specified species,
#' multiple or single. Multiple response multi-ids, single response single id.
#'
#' @param speciesId integer. The species for which the orthology is requested.
#' @param id strings. The event for which the orthology is requested.
#' @param silent logical. Run quitely.
#'
#' @return
#' \itemize{
# or nothing found
#'   \item \code{logical FALSE}: When an error happens.
#'   \item \code{list}: When query multi-ids.
#'   \item \code{dataframe}: Others.
#'   }
#'
#'
#' @examples
#' rtOrthology = reactomeOthology(speciesId = '49633',id = 'R-HSA-6799198')
#' rtOrthologies = reactomeOthology(speciesId = '49633',
#'                                  id = c('R-HSA-6799198','R-HSA-6799197'))
#'
#' @rdname othology
#' @export
#' @include POST_Method.R

reactomeOthology <- function(speciesId = '49633',
                             id = 'R-HSA-6799198',
                             multi = FALSE,
                             silent = FALSE){
  if(is.null(speciesId)&is.null(id))
    stop('speciesId and id cannot be null')
  speciesId = as.character(speciesId)
  if(1 == length(id))
    id = str_split(id, '[,;\\s]')[[1]]
  url = "http://www.reactome.org/ContentService/data/orthologies/ids/species/"
  url = str_c(url, speciesId)
  dt = POST(url = url,
            body = id,
            content_type("text/plain"),
            accept_json())
  lst = fromJSON(rawToChar(dt$content))
  # dt = lst(1:lenth(lst))
  # dt1 = data.frame(lst[[1]],stringsAsFactors = F)
  # for(i in 1:length(lst)){
  #   dt[[i]] = data.frame(lst[[i]],stringsAsFactors = F)
  #   dt1 = rbind(dt1,dt[[i]])
  # }

  # ns = names(lst)
  dt = list2dataframe(lst)
  # dt = cbind(data.frame(inputID = ns), dt)
  return(dt)
}

#' @rdname othology
#' @export

rtOthology <- function(speciesId,id, multi, silent = FALSE){
  return(reactomeOthology(speciesId = speciesId, id = id, multi = multi, silent = silent))
}


