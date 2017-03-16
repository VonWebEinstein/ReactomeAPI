#' The full event hierarchy for a given species
#'
#' Events (pathways and reactions) in Reactome are organised in a hierarchical structure
#' for every species. By following all 'hasEvent' relationships, this method retrieves
#' the full event hierarchy for any given species. The result is a list of tree structures,
#' one for each TopLevelPathway. Every event in these trees is represented by a
#' PathwayBrowserNode. The latter contains the stable identifier, the name, the species,
#' the url, the type, and the diagram of the particular event.
#'
#' @param species string. Allowed species filter: SpeciesName (eg: Homo sapiens)
#' SpeciesTaxId (eg: 9606)
#' @param silent logical. Whether print error messages on the screen.
#'
#' @return A data frame.
#' @seealso \code{\link{reactomeEvent}}
#'
#' @include url2dataframe.R
#' @export
#' @rdname eventHierarchy
reactomeEventHierarchy <- function(species = '9606', silent = FALSE){
  url0 = 'http://www.reactome.org/ContentService/data/eventsHierarchy'
  urlComponent = list(url0, species)
  dt = url2dataframe(urlComponent = urlComponent)
  return(dt)
}

#' @export
#' @rdname eventHierarchy
rtEventHierarchy <- function(species = '9606', silent = FALSE){
  return(reactomeEventHierarchy(species = species, silent = silent))
}
