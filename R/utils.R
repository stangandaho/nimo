# nocov start

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @description This operator allows for chaining commands in a more readable way.
#' @keywords internal
#' @importFrom magrittr %>%
#' @return The left-hand side value is passed to the right-hand side function.
#' @usage lhs \%>\% rhs
#' @export
magrittr::`%>%`

#' Pipe operator
#'
#' @name %<>%
#' @rdname pipe
#' @description This operator allows for chaining commands in a more readable way, while also updating the left-hand side value.
#' @keywords internal
#' @importFrom magrittr %<>%
#' @return The left-hand side value is modified by the right-hand side function and reassigned to the left-hand side.
#' @usage lhs \%<>\% rhs
#' @export
magrittr::`%<>%`


#' Pipe operator
#' use left if not NULL, else right
#' @name %||%
#' @noRd
`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

# nocov end
