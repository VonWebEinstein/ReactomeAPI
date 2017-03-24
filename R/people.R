# 空格
#'Person queries
#'
#'Retrieves a specific person's property by his/her OrcidId or DbId or a list of pathways or publications
#'authored by a given person. OrcidId, DbId or Email can be used to specify the person.
#'@param id string.Person identifier: Can be OrcidId, DbId. Email can also be used for
#'type = \code{authoredPathways} or \code{publications}
# can be null 废话，解释清楚null时什么意思
#'@param type string. Can be NULL, a attribute to be filtered, \code{authoredPathways} and
#'\code{publications}.
#'@details
# 解释并列多个type , 用\item, 行内用 \code{type1}:  ; \code{type2}:   ;... .
#'When type = NULL, all attributes about the given person in Reactome will be retrieved unless a single
#'attribute name is specified by the "type" parameter.
#'
#'A list of pathways or publications authored by the given person will be returned with
#'type = \code{authoredPathways} or \code{publications}.
#'@return a data frame.
#'@export
#'@include url2dataframe.R
#'@examples
#'
#'# Retrieves a person in Reactome whose orcidId is 0000-0001-5807-0069
#'res = people(id = "0000-0001-5807-0069", type = NULL)
#'
#'
#'##
#'res = people(id = "0000-0001-5807-0069", type ="authoredPathways" )
#'
#'##
#'res = people(id = "0000-0001-5807-0069", type = "publications")

# rtPerson(id = , name = , exactlyMatch = TRUE, attributeName = NULL) or
# rtPerson(str, ...), 当未指定第一个参数明名时，检测str是id还是name
#
# authoredPathway(id), authoredPublication(id), 写到一起
people = function(id, type = NULL){


  url0 = "http://www.reactome.org/ContentService/data/person"
  if(is.null(type)){

    urlComponent = list(url0, id)
  }
  else{

    urlComponent = list(url0, id, type)
  }
  dt = url2dataframe(urlComponent = urlComponent,
                     bindToSingleDataframe = FALSE)

  return(dt)

}
