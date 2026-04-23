
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
#'
#' @examples
#' nm_gbif_suggestion("Gyps")
#'
#' @importFrom httr2 request req_url_query req_timeout req_perform
#' resp_body_json
#'
#' @export

nm_gbif_suggestion <- function(query, time_out = 60) {

  url <- paste0("https://api.gbif.org/v1/species/suggest?")

  response <- request(base_url = url) %>%
    req_url_query(q = query)%>%
    req_timeout(time_out) %>%
    req_perform()

  species_list <- response %>%
    resp_body_json(simplifyDataFrame = T)

  if (!is.null(species_list)) {
    return(species_list)
  }

}
