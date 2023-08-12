#' App for Species Distribution Modeling
#'
#' \code{nimo} allows to focus on ecological niche modeling tasks without worrying about complex programming or technical details
#'
#' @examples
#'
#' nimo()
#'
#' @export

nimo <- function() {
  rt_path <- system.file("nimo", package = "nimo")
  shiny::addResourcePath("nimo", paste0(rt_path, "/www"))
  shiny::shinyAppDir(appDir = rt_path)
}



