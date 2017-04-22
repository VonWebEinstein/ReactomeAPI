#' Person queries
#'
#' \code{rtperson()} retrieves a specific person's information in Ractome by
#' his/her OrcidId or DbId. \code{authoredPathways()} or \code{authoredPublication()}
#' can be used to get a list of pathways or publications authored by a given person.
#' @usage rtperson(id, attributeNames, ...)
#' rtperson(name, exactlyMatch, ...)
#'
#' @param id string or charactor vector.Person identifier: Can be OrcidId, DbId. Email can also be used for
#' the methods \code{authoredPathway()} or \code{authoredPublication()}
#' @param name string or character vector.first  or last name of people you want to search in Reactome.
#' @param attributeNames string. Parameter for id query method. Attribute names to be filtered.
#' If NULL, the default, no filtration wil be done.
#' @param  exactlyMatch logical.If TRUE, the default, a list of people in Reactome with either their first
#' or last name matching exactly the given string(\code{name}) will be returned for name query method.
#' @param silent logical. Run quitely.
#'
#' @details
#' When exactlyMatch = false, \code{name} should be a string or character vector of length 1.
#' Only one of \code{id} and \code{name} needs to be specified, the other remains null.
#'
#' @return a data frame.
#' @rdname person
#' @export
#' @include includes.R
#' @examples
#‘ # search by DbId or OrCiId and retrieve some specfic properties
#' id.query = rtperson(id = c("0000-0001-5807-0069","5251225"),
#' attributeNames = c("displayName", "firstname"), silent = FALSE)
#' # query for all properties of "0000-0001-5807-0069" and "5251225"
#' (id.query = rtperson(id = c("0000-0001-5807-0069","5251225"),
#' attributeNames = NULL, silent = TRUE) )
#' # search by name
#' name.query = rtperson(name = c("Steve Jupe", "Tam Angela"), exactlyMatch = TRUE, silent = FALSE)
#'
#' # get a list of pathways or publications authored by given people
#' dt.1 = authoredPublication(id = "0000-0001-5807-0069",silent = FALSE)
#' dt.2 = authoredPathway(id = "0000-0001-5807-0069",silent = FALSE)


rtperson = function(id = NULL, name = NULL, exactlyMatch = TRUE, attributeNames = NULL, silent = FALSE){

  if(!is.null(id) && !is.null(name))
    stop("one of id and name must be null")
  if(!is.null(id)){

    return(id_query(id, attributeNames, silent))
  }

  return(name_query(name, exactlyMatch, silent))
}

getURL = function(){

  "http://www.reactome.org/ContentService/data"
}


name_query = function(name, exactlyMatch = TRUE, silent){

  if(exactlyMatch){

    name.ls = str_replace(name, "\\s", "%20")
    url.ls = str_c(getURL(), "people/name", name.ls, "exact", sep = "/")

    res.ls = lapply(url.ls, function(x) url2dataframe(x, silent))

    #dt = bindToSingledataframe(res.ls)
    dt = list2dataframe(res.ls, NULL, NULL, NULL)

  }
  else{

    name.ls = str_replace_all(name, "\\s", "%20")
    dt = url2dataframe(list(getURL(), "people/name", name.ls),silent)
  }

  return(dt)
}

id_query = function(id, attributeNames, silent){

  if(is.null(attributeNames)){

    url.ls = str_c(getURL(), "person", id, sep = "/")
    dt.ls = lapply(url.ls, function(x) url2dataframe(x, silent))
    dt = do.call(rbind, dt.ls)
  }
  else{

    dt_right = do.call(rbind,
                       lapply(id, function(x) multi_attr_person(x, attributeNames, silent)))

    dt_left = data.frame(id, stringsAsFactors = FALSE)
    dt = cbind(dt_left, dt_right, stringsAsFactors = FALSE)
    names(dt) = c("Id", attributeNames)
  }
  return(dt)
}

#retrieve multiple attributes for single id

multi_attr_person = function(id, attributeNames, silent){

  res.ls = lapply(attributeNames, function(x) single_attr_person(id, x, silent))
  dt = do.call(cbind, res.ls)

  return(dt)
}

# retrieve single attribute for single id
single_attr_person = function(id, singleAttribute, silent){

  url = str_c(getURL(), "person", id, singleAttribute, sep = "/")
  GET_obj = GET(url)
  str = content(GET_obj)

  if(!GET_obj$status_code == 200){
    errorMessage(cont, silent)
    return(NA)
  }

  return(str)

}

#' @include includes.R
#' @export
#' @rdname person
authoredPathway = function(id, silent = FALSE){

  dt.ls = lapply(id, function(x) url2dataframe(list(getURL(), "person", x, "authoredPathways"),
                                   silent))
  return(list2dataframe(dt.ls, NULL, NULL, NULL))
}


#' @include includes.R
#' @export
#' @rdname person
authoredPublication = function(id, silent = FALSE){

  dt.ls = lapply(id, function(x) url2dataframe(list(getURL(), "person", x, "publications"),
                                               silent))
  return(list2dataframe(dt.ls, NULL, NULL, NULL))
}
