#' App for Species Distribution Modeling
#'
#' Entry function to launch nimo app to query occurrence data
#' from the Global Biodiversity Information Facility (GBIF)
#' and incorporate it into ecological niche modelling workflow offered by flexsdm
#' \doi{10.1111/2041-210X.13874}
#'
#' @importFrom shiny addResourcePath shinyAppDir
#'
#' @export

nimo <- function() {
  rt_path <- system.file("nimo", package = "nimo")
  shiny::addResourcePath("nimo", paste0(rt_path, "/www"))
  shiny::shinyAppDir(appDir = rt_path)
}



