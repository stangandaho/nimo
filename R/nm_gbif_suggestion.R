
#' @title
#' Get suggestions from GBIF
#' @description
#' A quick and simple autocomplete service that returns up to 20 name usages by doing
#' prefix matching against the scientific name. Results are ordered by relevance.
#'
#' @param query Simple full text search parameter. The value for this parameter can be a simple word or a phrase. Wildcards are not supported
#' @param time_out Set time (in second) limit for a request. An error will be thrown
#' if the request does not complete in the time limit.
#'
#' @return
#' Dataframe
#'
#' @export
#'
#' @examples
#' nm_gbif_suggestion("Gyps")

nm_gbif_suggestion <- function(query, time_out = 60) {

  url <- paste0("https://api.gbif.org/v1/species/suggest?")

  response <- httr2::request(base_url = url) %>%
    httr2::req_url_query(q = query)%>%
    httr2::req_timeout(time_out) %>%
    httr2::req_perform()

  species_list <- response %>%
    httr2::resp_body_json(simplifyDataFrame = T)

  if (!is.null(species_list)) {
    return(species_list)
  }

}
