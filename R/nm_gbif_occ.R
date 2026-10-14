#' @title
#' Query occurrence records from GBIF
#' @description
#' Download georeferenced occurrence records from the GBIF occurrence search API,
#' page by page, together with the citation of every dataset involved. This is
#' the function used by the nimo app; it lets scripts reproduce a GBIF query made
#' in the app.
#'
#' GBIF data change over time. To reproduce an analysis exactly, save the
#' downloaded records (e.g. with `write.csv()`) and cite the datasets.
#'
#' @param query named list of GBIF occurrence search parameters, e.g.
#' `list(scientificName = "Gyps africanus", country = "ZA", year = "2000,2020")`.
#' `hasCoordinate = "true"` and `hasGeospatialIssue = "false"` are added unless set.
#' See <https://techdocs.gbif.org/en/openapi/v1/occurrence#/Searching%20occurrences>.
#' @param limit integer. Records per request (max 300).
#' @param max_records integer. Maximum number of records to download. Default `Inf`.
#' @param time_out numeric. Time limit (in seconds) for each request.
#' @param sleep numeric. Pause (in seconds) between requests, to be gentle with the API.
#' @param fields character. Columns kept in the output. Missing columns are filled with `NA`.
#'
#' @return A list with:
#' \itemize{
#' \item occurrences: data.frame with one row per record.
#' \item citation: character vector with the citation of each dataset.
#' }
#'
#' @examples
#' \dontrun{
#' res <- nm_gbif_occ(list(scientificName = "Gyps africanus", country = "BJ"))
#' head(res$occurrences)
#' res$citation
#' }
#'
#' @importFrom httr2 request req_url_query req_timeout req_perform resp_body_json
#' @export
nm_gbif_occ <- function(query,
                        limit = 200,
                        max_records = Inf,
                        time_out = 300,
                        sleep = 0.5,
                        fields = c("basisOfRecord", "decimalLatitude", "decimalLongitude",
                                   "country", "occurrenceStatus", "scientificName",
                                   "taxonomicStatus", "sex", "continent", "stateProvince",
                                   "locality", "year", "month", "day", "recordedBy",
                                   "institutionCode", "samplingProtocol", "habitat", "license")) {
  base_url <- "https://api.gbif.org/v1/occurrence/search"
  query <- lapply(query, function(x) paste(as.character(x), collapse = ","))
  defaults <- list(hasCoordinate = "true", hasGeospatialIssue = "false")
  query <- c(query, defaults[setdiff(names(defaults), names(query))])
  query$limit <- min(limit, 300)

  occ <- list()
  dataset_keys <- character(0)
  offset <- 0
  n <- 0
  repeat {
    query$offset <- offset
    resp <- request(base_url = base_url) %>%
      req_timeout(time_out) %>%
      req_url_query(!!!query) %>%
      req_perform()
    res <- resp_body_json(resp, simplifyDataFrame = TRUE)
    page <- res$results
    if (is.data.frame(page) && nrow(page) > 0) {
      dataset_keys <- union(dataset_keys, unique(page$datasetKey))
      for (f in setdiff(fields, names(page))) page[[f]] <- NA
      occ[[length(occ) + 1]] <- page[, fields, drop = FALSE]
      n <- n + nrow(page)
    }
    if (isTRUE(res$endOfRecords) || n >= max_records || !is.data.frame(page) || nrow(page) == 0) {
      break
    }
    offset <- offset + query$limit
    Sys.sleep(sleep)
  }

  occurrences <- if (length(occ) > 0) {
    do.call(rbind, occ)
  } else {
    as.data.frame(stats::setNames(replicate(length(fields), logical(0), simplify = FALSE), fields))
  }
  if (is.finite(max_records) && nrow(occurrences) > max_records) {
    occurrences <- occurrences[seq_len(max_records), , drop = FALSE]
  }
  rownames(occurrences) <- NULL

  citation <- vapply(dataset_keys, function(key) {
    meta <- tryCatch(
      request(paste0("https://api.gbif.org/v1/dataset/", key)) %>%
        req_timeout(time_out) %>%
        req_perform() %>%
        resp_body_json(),
      error = function(e) NULL
    )
    txt <- meta$citation$text
    if (is.null(txt)) NA_character_ else sub("accessed via GBIF.org", "accessed via GBIF.org using nimo", txt)
  }, character(1), USE.NAMES = FALSE)

  list(occurrences = occurrences, citation = citation[!is.na(citation)])
}
