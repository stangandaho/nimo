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
query_occ <- function(query_params) {
    base_url <- "https://api.gbif.org/v1/occurrence/search" # GBIF API base URL
    # Initialize variables
    occ <- data.frame()
    offset <- 0
    more_results <- TRUE
    while (more_results) {
      # Update the offset parameter
      query_params$offset <- offset

      response <- httr2::request(base_url = base_url) %>%
        httr2::req_timeout((input$sys_timeout)*60) %>%
        httr2::req_url_query(!!!query_params) %>%
        httr2::req_perform()

      # Check if the request was successful
      if (response$status_code == 200) {
        resultats <- response %>%
          httr2::resp_body_json(simplifyDataFrame = T) # Extract the occurrence data
        occurrences <- resultats$results
        more_results <- resultats$endOfRecords == FALSE # Check if there are more results

        offset <- offset + query_params$limit # Update the offset for the next request

        occdt_col <- colnames(occurrences)
        basic_col <- c("basisOfRecord", "decimalLatitude", "decimalLongitude",
                       "country", "occurrenceStatus", "scientificName", "taxonomicStatus", "sex",
                       "continent", "stateProvince", "locality", "year", "month", "day",
                       "recordedBy", "institutionCode", "samplingProtocol", "habitat", "license")

        absent_col <- basic_col[!basic_col %in% occdt_col[which(occdt_col %in% basic_col)]]

        if (length(absent_col) > 0) {
          df <- data.frame(matrix(NA, nrow = nrow(occurrences), ncol = length(absent_col)))
          colnames(df) <- absent_col
          occurrences <- occurrences %>% dplyr::bind_cols(df)
        }
        #col <- basic_col[which(basic_col %in% occdt_col)]

        occurrences <- occurrences %>%
          dplyr::select(dplyr::all_of(basic_col))
        occ <- dplyr::bind_rows(occ, occurrences)
        # Get dataset keys for metadata retrieval
        dataset_keys <- unique(resultats$results[, "datasetKey"])#unique(occurrences$datasetKey)
        # Fetch metadata for each dataset
        metadata_list <- list()
        for (dataset_key in dataset_keys) {
          metadata_url <- paste0("https://api.gbif.org/v1/dataset/", dataset_key)
          metadata_response <- httr2::request(base_url = metadata_url) %>%
            httr2::req_perform()

          # Check if the request for dataset metadata was successful
          if (metadata_response$status_code == 200) {
            metadata <- metadata_response %>%
              httr2::resp_body_json()
            metadata_list[[dataset_key]] <- metadata$citation$text
          }

        }
          markdown_text <- sub("accessed via GBIF.org", "accessed via GBIF.org using nimo", paste(metadata_list))
      }
      Sys.sleep(input$sleep_time) # Sleep

    }

    return(list(occ, markdown_text))
}
