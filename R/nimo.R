#' App for Species Distribution Modeling
#'
#' Entry function to launch nimo app to query occurrence data
#' from the Global Biodiversity Information Facility \href{https://www.gbif.org/occurrence/search}{(GBIF)}
#' and incorporate it into ecological niche modelling workflow offered by
#' \href{https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13874}{[flexdsm]}
#'
#' @examples
#' # Install remotes if it is not already installed
#' if (!require("remotes", character.only = TRUE)) {
#'   install.packages("remotes")
#'   }
#'
#' # Install nimo R package
#' remotes::install_github("stangandaho/nimo")
#'
#' # Launch the app
#' nimo::nimo()
#'
#' @export

nimo <- function() {
  rt_path <- system.file("nimo", package = "nimo")
  shiny::addResourcePath("nimo", paste0(rt_path, "/www"))
  shiny::shinyAppDir(appDir = rt_path)
}



