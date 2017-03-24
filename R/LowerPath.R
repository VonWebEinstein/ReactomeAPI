#' Pathway related queries
#'
#' This method traverses the event hierarchy and retrieves the list of
#' all lower level pathways that have a diagram(optional) and contain any form
#' (optional) of the given PhysicalEntity or Event.
#' @param id string. The entity (in any of its forms) that has to be present
#' in the pathways.
#' @param species string.The species for which the pathways are requested
#' (SpeciesName or SpeciesTaxId)
# 可以指定id 或identifier
#' @param type string. Whether the given \code{id} is a entity or identifier.
#' @param allForms logical;if TRUE, any form of the given PhysicalEntity or
#'  Event is consideried. default = TRUE
# withDiagram default = FALSE
#' @param diagram logical;if TRUE, only those pathways with a diagram will be listed.
#' default = TRUE.
#' @param silent logical; if TRUE, error message will not be printed. default = TRUE
#' @return a data frame
#' @export
#' @include error.R
#' @import stringr
#' @import httr
#' @import jsonlite
#' @examples
# examples 中加注释
# 起个更符合场景的变量名
#' res = rtLowerPath(id = "R-HSA-199420", species = "48887", type = "entity",
#'                   allForms = TRUE, diagram = TRUE)
#'
#' res =  rtLowerPath(id = "R-HSA-199420", species = "48887", type = "entity",
#'                   allForms = TRUE, diagram = FALSE)
#'
#' res =  rtLowerPath(id = "PTEN", species = "48887", type = "identifier",
#'                   allForms = TRUE, diagram = TRUE)

# rtLowerPathway
rtLowerPath = function(id, species = "48887", type = "entity",
                       allForms = TRUE, diagram = TRUE, silent = TRUE){

 url0 = "http://www.reactome.org/ContentService/data/pathways/low"
 tmp0 = switch (diagram, "diagram", NULL)
 tmp1 = switch (allForms, "allForms", NULL)

 url = str_c(str_c(url0, tmp0, type, id, tmp1, sep = "/"),
             str_c("speciesId=", species, sep = ""),
             sep = "?")
# silent = T
 res = try(fromJSON(url), silent = silent)

 if(inherits(res, "try-error")){
   tmp3 = content(GET(url, content_type_json()))
   return(errorMessage(tmp, silent = silent))
 }
 else{
   return(res)
 }


}
