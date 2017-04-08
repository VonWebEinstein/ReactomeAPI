#' Pathway related queries
#'
#' The \code{rtLowerPathway} function traverses the event hierarchy and retrieves the list of
#' all lower level pathways that have a diagram(optional) and contain any form
#' (optional) of the given PhysicalEntity or Event.
#' The \code{rtTopPathway} function retrieves the list of top level pathways for the given species.
#'
#' @param id string. The entity (in any of its forms) or a identifier that has to be present
#' in the pathways.
#' @param species string.The species for which the pathways are requested
#' (SpeciesName or SpeciesTaxId)
#' @param allForms logical;if TRUE, any form of the given PhysicalEntity or
#'  Event is consideried. default = TRUE
#' @param withDiagram logical;if TRUE, only those pathways with a diagram will be listed.
#' default = FALSE.
#' @param silent logical; if TRUE, error message will not be printed. default = fasle
#' @return a data frame
#' @export
#' @include error.R
#' @import stringr
#' @import httr
#' @import jsonlite
#' @rdname  path
#' @examples
#  # find  lower level pathways with a diagram, considering all forms of R-HSA-199420
#' res.1 = rtLowerPathway(id = "R-HSA-199420", species = "48887",
#'                   allForms = TRUE, withDiagram = TRUE)
#' # all those pathways with or without a diagram
#' res.2 =  rtLowerPathway(id = "R-HSA-199420", species = "48887",
#'                   allForms = TRUE, withDiagram = FALSE)
#' # search with a known  identifier
#' res.3 =  rtLowerPathway(id = "PTEN", species = "48887",
#'                   allForms = TRUE, withDiagram = TRUE)

rtLowerPathway = function(id, species = "48887",
                       allForms = TRUE, withDiagram = TRUE, silent = FALSE){

 url0 = "http://www.reactome.org/ContentService/data/pathways/low"
 tmp0 = switch (withDiagram, "diagram", NULL)
 tmp1 = switch (allForms, "allForms", NULL)

 type = switch (str_detect(id, "\\d{5}"), "entity", "identifier")

 url = str_c(str_c(url0, tmp0, type, id, tmp1, sep = "/"),
             str_c("speciesId=", species, sep = ""),
             sep = "?")

 res = try(fromJSON(url), silent = TRUE)

 if(inherits(res, "try-error")){
   tmp3 = content(GET(url, content_type_json()))
   return(errorMessage(tmp, silent = silent))
 }
 else{
   return(res)
 }


}




#' @rdname path
#'
#' @examples
#' # find all top level pathways for Homo sapiens
#' human.1 = rtTopPathway(species = "9606")
#' # SpeciesName is also permitted for rtTopPath
#' human.2 = rtTopPathway(species = "Homo sapiens")


rtTopPathway = function(species, silent = FALSE){

  url0 = "http://www.reactome.org/ContentService/data/pathways/top"
  if( !str_detect(species, "\\d{4}") ){

    species = parse_species(species)
  }
  urlComponent = list(url0, species)

  dt = url2dataframe(urlComponent = urlComponent, silent = FALSE)

  return(dt)
}

# transform when SpeciesName is given
parse_species = function(species){

    species = str_replace_all(species, "\\s", "%20")

    return(species)
}
