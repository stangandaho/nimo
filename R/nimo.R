#' Shiny UI for nimo package
#'
#' \code{nimo} allows to focus on ecological niche modeling tasks without worrying about complex programming or technical details
#'
#' @param port is the TCP port that the application should listen on. If the port is not specified,
#' and the shiny.port option is set (with options(shiny.port = XX)), then that port will be used.
#' Otherwise, use a random port.
#'
#' @param in_browser If TRUE, the system's default web browser will be launched automatically
#' after the app is started. Defaults to FALSE in interactive sessions only. This value of
#' this parameter can also be a function to call with the application's URL.
#'
#' @param host The IPv4 address that the application should listen on.
#' Defaults to the shiny.host option, if set, or "127.0.0.1" if not.
#'
#' @param maxUploadSize is a integer. The max upload file size argument. Default value is 200 (megabyte)
#'
#' @examples
#'
#' nimo()
#'
#' @export

nimo <- function(maxUploadSize=200) {
  rt_path <- system.file("nimo", package = "nimo")
  source(paste0(rt_path, "/ui.R")); source(paste0(rt_path, "/server.R"))
  shinyOptions(maxUploadSize = maxUploadSize)

  shinyApp(ui = ui, server = server)

}

