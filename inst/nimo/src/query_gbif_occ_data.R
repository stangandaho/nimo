## Field to query
gbif_q <- c(
  "Scientific name" = "scientificName", "Phylum" = "phylum", "Order" = "order",
  "Class" = "class", "Genus" = "genus", "Family" = "family", "Status" = "status",
  "Rank" = "rank", "Parent" = "parent", "Higher classification map" = "higherClassificationMap",
  "Synonym" = "synonym", "Family key" = "familyKey", "Canonical name" = "canonicalName",
  "Key" = "key", "Name key" = "nameKey", "kingdom" = "kingdom",
  "Nub key" = "nubKey", "Phylum key" = "phylumKey", "Parent key" = "parentKey",
  "Genus key" = "genusKey", "Order key" = "orderKey", "Kingdom key" = "kingdomKey",
  "Taxon key" = "taxonKey"
)


# Prepare the query parameters
query_params <- function() {
    query_params <- list(
      "limit" = 200,
      "offset" = 0,
      "hasCoordinate" = "true",
      "hasGeospatialIssue" = "false",
      "hl" = "true"
    )
    # Add optional parameters if provided
    if (!is.null(input$country_filter) & input$country_filter != "") {
      query_params[["country"]] <- input$country_filter
    }

    query_params[[input$search_by]] <- input$species_suggestions

    if (!is.null(input$date_filter_from)) {
      if(length(input$date_filter_from) > 1){
        x_ <- as.Date(input$date_filter_from)
        query_params[["year"]] <- paste(substr(as.character(min(x_)), 1,4),
                                        substr(as.character(max(x_)), 1,4),
                                        sep = ",")
      } else{
        query_params[["year"]] <- paste(substr(input$date_filter_from, 1, 4), collapse = ",")
      }
    }
    return(query_params)
}

## Query
## Code of the last GBIF query
gbif_query_code <- reactiveVal(NULL)

query_occ <- function(query_params) {
  query <- query_params[!names(query_params) %in% c("limit", "offset")]
  args <- list(query = query, limit = query_params$limit,
               time_out = input$sys_timeout * 60, sleep = input$sleep_time)
  res <- do.call(nm_gbif_occ, args)
  gbif_query_code(c(
    paste0("# Query run on ", format(Sys.Date()), " (GBIF data change over time)"),
    nm_code_call("nimo::nm_gbif_occ", args, assign = "gbif"),
    "occ_raw <- gbif$occurrences",
    "gbif$citation"
  ))
  return(list(res$occurrences, res$citation))
}
