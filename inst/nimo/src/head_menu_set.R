
box_dropdown_item <- function (..., id = NULL, href = NULL, icon = NULL)
{
  shiny::tags$li(shiny::tags$a(id = id, class = if (!is.null(id))
    "action-button", href = if (!is.null(href))
      href
    else "#", target = if (!is.null(href))
      "", icon, ...))
}
