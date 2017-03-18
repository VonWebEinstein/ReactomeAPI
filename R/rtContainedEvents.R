#' Pathway related queries
#'
#' This method recursively retrieves all the events contained in any given event.
#' @param id string. The event for which the contained events are requested.
#' @param ATTR string. Retrieve all attributes or only a single attribute for each of the
#' events contained in the given event. default = \code{all}
#' @param silent logical; if TRUE, error message is not printed.
#' @details
#' Events are the building blocks used in Reactome to represent all biological processes,
#' and they include pathways and reactions. Typically, an event can contain other events.
#' For example, a pathway can contain smaller pathways and reactions.
#'
#' This function will get all infromations about the containes events when ATTR = \code{"all},
#' otherwise, only the attribute sepcified by \code{ATTR} will be retrieved.
#' @return  a data frame.
#' @export
#' @include error.R
#' @include url2dataframe.R
#' @example
#' res = rtContainedEvents(id = "R-HSA-5673001", ATTR = "all")
#'
#' res = rtContainedEvents(id = "R-HSA-5673001", ATTR = "stId")

rtContainedEvents = function(id, ATTR = "all", silent = TRUE){

  url0 = "http://www.reactome.org/ContentService/data/pathway"
  if(ATTR == "all"){

   url = list(url0, id, tmp0 = "containdEvents")
  }
  else{
    url = list(url0, id, tmp0 ="containedEvents", tmp1 = ATTR)
  }

  dt = url2dataframe(urlComponent = urlComponent, silent = silent)
  return(dt)


}
