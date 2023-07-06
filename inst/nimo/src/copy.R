## ALL OBJECT IMPORTED FROM shinyCopy2clipboard: https://github.com/deepanshu88/shinyCopy2clipboard/tree/main

usecopy <- function ()
{
  tagList(shiny::singleton(shiny::tags$head(shiny::tags$script(src = "nimo/clipboard.min.js"),
                                            shiny::tags$script(src = "nimo/copy2clipboard.js"))),
          shiny::tags$body(shiny::tags$script(src = "nimo/tooltip.js")))
}

###
copy_button <- function (id, label, text = "No Text", icon = NULL, width = NULL,
                         class = NULL, modal = FALSE, ...)
{
  shiny::actionButton(inputId = id, label = label, icon = icon,
                      width = width, class = class, `data-clipboard-text` = text,
                      onclick = if (!modal) {
                        paste0("new ClipboardJS(\".btn\", document.getElementById(\"",
                               id, "\"));")
                      }
                      else {
                        paste0("new ClipboardJS(\".btn\", { container: document.getElementById(\"",
                               id, "\") });")
                      }, ...)
}

###

copy_button_update <- function (session, id = "copybtn", label = "Copy to Clipboard",
          icon = NULL, text = "Sample Text", modal = FALSE)
{
  if (!is.null(icon))
    icon <- as.character(icon)
  session$sendCustomMessage("copybtnUpdate", list(id = id,
                                                  label = label, icon = icon, `data-clipboard-text` = text,
                                                  onclick = if (!modal) {
                                                    paste0("new ClipboardJS(\".btn\", document.getElementById(\"",
                                                           id, "\"));")
                                                  } else {
                                                    paste0("new ClipboardJS(\".btn\", { container: document.getElementById(\"",
                                                           id, "\") });")
                                                  }))
}
