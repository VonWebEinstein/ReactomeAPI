#' Pathway related queries
#'
#' This method recursively retrieves all the events contained in any given event.
#' @param id string. The event for which the contained events are requested.
#' @param attributeNames string or character vector. if NULL, the default, all
#' properties of the contained evnets will be returned;otherwise, just retrieve
#' those attributes in \code{attributeNames} list.
#' @param silent logical; if TRUE, error message is not printed. default = false
#' @details
#' Events are the building blocks used in Reactome to represent all biological
#' processes, and they include pathways and reactions. Typically, an event can
#' contain other events. For example, a pathway can contain smaller pathways and
#' reactions.
#' @return  a data frame.
#' @export
#' @include includes.R
#' @rdname ContainedEvents
#' @examples
#' # retrieve some attributes of the contained events
#' multi.Attr.event = rtContainedEvents(id = "R-HSA-5673001",
#'                    attributeNames = c("stId", "name"), silent = FALSE)
#' # get all the properties
#' all.Attr.event = rtContainedEvents(id = "R-HSA-5673001",
#'                  attributeNames = NULL, silent = FALSE)

rtContainedEvents = function(id, attributeNames = NULL, silent = FALSE){


  if(!is.null(attributeNames)){

    return(multi_attr_event(id, attributeNames, silent))
  }

  dt = url2dataframe(list(GETURL(), id, "containedEvents"), silent = silent)
  return(dt)


}

GETURL = function(){
  "http://www.reactome.org/ContentService/data/pathway"
}


single_attr_event = function(id, singleAttribute, silent){

  url0 = GETURL()
  url = str_c(url0, id, "containedEvents", singleAttribute, sep = "/")
  GET_obj = GET(url)
  cont = content(GET_obj)

  if(!GET_obj$status_code == 200){
    errorMessage(cont, silent = silent)
    return(NA)
  }

  if(singleAttribute == "name"){

    vec.tmp = str_replace_all(str_split(cont, "\\n,")[[1]],
                        "[\\]\\[\\n]", "")
  }
  else{

  vec.tmp = str_replace_all(str_split(cont, ",")[[1]], "[\\]\\[]", "")
  }
  vec = str_trim(vec.tmp)

  dt = data.frame(vec, stringsAsFactors = FALSE)
  names(dt) = singleAttribute

  return(dt)

}

multi_attr_event = function(id, attributeNames, silent){

  list.attr.event = lapply(attributeNames, function(x) single_attr_event(id, x, silent))
  #names(dt) = attributeNames

  dt = do.call(cbind, list.attr.event)

  return(dt)


}
