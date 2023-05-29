## simplify table view for standard table output
render_dt <- function(...) {
  DT::renderDT(...,
               options = list(scrollX = TRUE, paging = FALSE,
                              ordering = FALSE, searching = FALSE))
}

## error modal showing
error <- function(e){
  showModal(error_modal())
  output$error_modal <- renderText(paste(e))
}

## concatenate models selected for prediction
concat_model <- function(list, index = c()){
  empty_list <- list()
  for (i in index) {
    empty_list[[i]] <- list[[i]]
  }
  return(empty_list[i][[1]])
}

## bind in list prediction output (raster)
predict_rasters <- function(prediction){
  predic_raster_list <- list()
  for (m in 1:length(prediction)) {
    rst <- prediction[[m]]
    if (input$model_category == "Standard models") {
      model_name <- names(which(algorithm == names(rst)[1]))
    } else if(input$model_category == "Ensemble"){
      model_name <- names(which(ensemble == names(rst)[1]))
    } else if(input$esm == TRUE){
      model_name <- paste0(names(which(algorithm == substr(names(rst)[1], 5, 7))), "ESM")
    }
    if (terra::nlyr(rst) > 1 && input$esm == FALSE) {
      names(rst) <- c(model_name, paste(names(rst)[1], "-", input$predict_thr))
    } else if (terra::nlyr(rst) <= 1 && input$esm == FALSE) {
      names(rst)[1] <- model_name
    }

    for (r in 1:terra::nlyr(rst)) {
      predic_raster_list[[ names(rst[[r]]) ]] <- rst[[r]]
    }
  }
  return(predic_raster_list)
}

