# 写到pathway 一起， 帮助也融合到一起
#' Pathway related queries
#'
#'This method retrieves  the list of top level pathways for the given species.
#'@param species string. Specifies the species by SpeciesName (eg: Homo sapiens)
#'or SpeciesTaxId (eg: 9606).
#'@return a data frame
#'@export
#'@include url2dataframe.R
#'@examples
#'res = rtTopPath(species = "9606")


rtTopPath = function(species){

  url0 = "http://www.reactome.org/ContentService/data/pathways/top"
  urlComponent = list(url0, species)
  dt = url2dataframe(urlComponent = urlComponent)

  return(dt)
}
