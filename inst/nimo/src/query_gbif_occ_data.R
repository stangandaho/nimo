## Field to query
gbif_q <- c(
  "Scientific name" = "scientificName", "Phylum" = "phylum", "Order" = "order",
  "Class" = "class", "Genus" = "genus", "Family" = "family", "Status" = "status",
  "Rank" = "rank", "Parent" = "parent", "Higher classification map" = "higherClassificationMap",
  "Synonym" = "synonym", "Family key" = "familyKey", "Canonical name" = "canonicalName",
  "Key" = "key", "Name key" = "nameKey", "kingdom" = "kingdom",
  "Nub key" = "nubKey", "Phylum key" = "phylumKey", "Parent key" = "parentKey",
  "Genus key" = "genusKey", "Order key" = "orderKey", "Kingdom key" = "kingdomKey"
)

## Set country iso code
cc <- paste0(system.file("", package = "nimo"), "/data")
ccode <- read.csv(paste0(cc, "/country_code.txt")) %>%
  dplyr::select(country, iso) %>%
  dplyr::mutate(iso = dplyr::case_when(is.na(iso) ~ "NA",
                                       TRUE ~ iso))
ccode <- rbind(data.frame("country" = "Any country","iso" = ""), ccode)

countries <- ccode$iso
names(countries) <- ccode$country

# Prepare the query parameters
query_params <- function() {
    query_params <- list(
      "limit" = 300,
      "offset" = 0,
      "q" = input$species_suggestions,
      "hasCoordinate" = "true",
      "hasGeospatialIssue" = "false"
    )
    # Add optional parameters if provided
    if (!is.null(input$country_filter) & input$country_filter != "") {
      query_params[["country"]] <- input$country_filter
    }
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
    all_occurrences <- list()
    offset <- 0
    more_results <- TRUE
    while (more_results) {
      # Update the offset parameter
      query_params$offset <- offset
      response <- GET(base_url, query = query_params) # Make the API request
      # Check if the request was successful
      if (response$status_code == 200) {
        json_data <- fromJSON(content(response, "text", encoding = "UTF-8")) # Parse the JSON response
        occurrences <- json_data$results # Extract the occurrence data
        more_results <- json_data$endOfRecords == FALSE # Check if there are more results
        offset <- offset + query_params$limit # Update the offset for the next request
        all_occurrences <- append(all_occurrences, list(occurrences))

        # Get dataset keys for metadata retrieval
        dataset_keys <- unique(occurrences$datasetKey)
        # Fetch metadata for each dataset
        metadata_list <- list()
        for (dataset_key in dataset_keys) {
          metadata_url <- paste0("https://api.gbif.org/v1/dataset/", dataset_key)
          metadata_response <- GET(metadata_url)
          # Check if the request for dataset metadata was successful
          if (metadata_response$status_code == 200) {
            metadata <- content(metadata_response, "parsed")
            metadata_list[[dataset_key]] <- metadata$citation$text
          }

        }
          markdown_text <- sub("accessed via GBIF.org", "accessed via nimo 10.15468/22lope", paste(metadata_list))
      }
    }
    # Combine all_occurrences list into a single data frame
    all_occurrences_df <- dplyr::bind_rows(all_occurrences)
    occdt_col <- colnames(all_occurrences_df)
    basic_col <- c("basisOfRecord", "decimalLatitude", "decimalLongitude",
                   "country", "occurrenceStatus", "scientificName", "taxonomicStatus", "sex",
                   "continent", "stateProvince", "locality", "year", "month", "day",
                   "recordedBy", "institutionCode", "samplingProtocol", "habitat", "license")
    col <- basic_col[which(basic_col %in% occdt_col)]
    occ <- all_occurrences_df %>% dplyr::select(dplyr::all_of(col))
    return(list(occ, markdown_text))
}




