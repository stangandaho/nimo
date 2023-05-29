nimo <- function(browser = FALSE) {
  options(warn = -1, message = FALSE)# suppress warning
  source("./R/ui.R", local = T); source("./R/server.R", local = T)

  if (browser == TRUE) {
    opt_list <- list("launch.browser")
  } else {
    opt_list <- list()
  }
  #shiny::shinyApp(ui = ui, server = server, options = opt_list)
}

nimo(browser = TRUE)

bibliometrix::biblioshiny()

getAnywhere(biblioshiny)
