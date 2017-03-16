#' Reactome Data: Queries related to events
#'
#' The \code{reactomeEvent} function can query data to given event, including \code{ancestors},
#' \code{participants}, \code{participatingPhysicalEntities}, \code{participatingReferenceEntities},
#' given as the argument \code{type}.
#'
#' @note
#' \code{ancestors}: The Reactome definition of events includes pathways and reactions.
#' Although events are organised in a hierarchical structure, a single event can be in more than
#' one location, i.e. a reaction can take part in different pathways while, in the same way,
#' a sub-pathway can take part in many pathways. Therefore, this method retrieves a list of all
#' possible paths from the requested event to the top level pathway(s).
#'
#' \code{participants}: Participants contains a PhysicalEntity (dbId, displayName) and a
#' collection of ReferenceEntities (dbId, name, identifier, url)
#'
#' \code{participatingPhysicalEntities}: This method retrieves all the PhysicalEntities
#' that take part in a given event. It is worth mentioning that because a pathway can
#' contain smaller pathways (subpathways), the method also recursively retrieves the
#' PhysicalEntities from every constituent pathway.
#'
#' \code{participatingReferenceEntities}: PhysicalEntity instances that represent,
#' e.g., the same chemical in different compartments, or different post-translationally modified
#' forms of a single protein, share numerous invariant features such as names, molecular structure
#' and links to external databases like UniProt or ChEBI.
#'
#' To enable storage of this shared information in a single place, and to create an explicit
#' link among all the variant forms of what can also be seen as a single chemical entity,
#' Reactome creates instances of the separate ReferenceEntity class. A ReferenceEntity instance
#' captures the invariant features of a molecule.
#'
#' This method retrieves the ReferenceEntities of all PhysicalEntities that take part in a given
#' event. It is worth mentioning that because a pathway can contain smaller pathways (subpathways),
#' this method also recursively retrieves the ReferenceEntities for all PhysicalEntities in every
#' constituent pathway.
#' @param id string. The event for which the ancestors etc. are requested.
#' @param type string. Can be \code{ancestors}, \code{participants},
#' \code{participatingPhysicalEntities}, or \code{participatingReferenceEntities}
#' @param silent logical. Whether print error messages on the screen.
#'
#' @return A data frame.
#' @export
#' @include url2dataframe.R
#' @examples
#' # The ancestors of a given event
#' res = reactomeEvent('R-HSA-5673001', 'ancestors')
#'
#' # A list of participants for a given event
#' res = reactomeEvent('5205685', 'participants')
#'
#' # A list of participating PhysicalEntities for a given event
#' res = reactomeEvent('R-HSA-5205685', 'participatingPhysicalEntities')
#'
#' # A list of participating ReferenceEntities for a given event
#' res = reactomeEvent('5205685', 'participatingReferenceEntities')
#'
#' @seealso \code{\link{reactomeEventHierarchy}}
#' @rdname event
reactomeEvent <- function(id, type, silent = FALSE){
  if(exis)
  url0 = 'http://www.reactome.org/ContentService/data/event'
  urlComponent = list(url0, id, type)
  dt = url2dataframe(urlComponent = urlComponent)

  return(dt)
}

#' @rdname event
#' @export

rtEvent <- function(id, type, silent = FALSE){
  return(reactomeEvent(id = id, type = type, silent = silent))
}
