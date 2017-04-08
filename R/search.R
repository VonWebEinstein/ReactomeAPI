#' Search Reactome Database
#'
#' Search some keys, retrieves faceting information, or get search suggests.
#'
#' @usage
#' rtSearch(query, faceting = FALSE, species = 'Homo sapiens', types = 'pathway',...)
#' rtSearch(query, faceting = TURE, )
#' @param query string. Search term.
#' @param faceting logical. Retrieve faceting information.
#' @param species string. Species names.
#' @param types string. Types to filter.
#' @param compartments_to_filter string. Compartments to filter.
#' @param reaction_types_to_filter string. Reaction types to filter.
#' @param cluster logical. Cluster results.
#' @param start_row integer. Start row.
#' @param Number_of_rows_to_include integer. Number of rows to include.
#' @param silent logical. Run quitely.
#'
#' @details

rtSerach <- function(query = NULL,
                     faceting = FALSE,
                     species = 'Homo sapiens',
                     types = 'pathway',
                     Compartments_to_filter = NULL,
                     Reaction_types_to_filter = NULL,
                     cluster = TRUE,
                     Start_row = NULL,
                     Number_of_rows_to_include = NULL,
                     silent = FALSE){



}
