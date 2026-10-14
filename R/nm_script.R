# Reproducible script: code generation helpers

#' Value with an explicit code representation
#'
#' Wraps an object that is passed to a function in the app (e.g. a data.frame)
#' together with the R code that recreates it in the generated script
#' (e.g. the variable name `"sdm_data"`).
#'
#' @param value object used in the app.
#' @param code character. R code used in the generated script.
#' @noRd
nm_cv <- function(value, code) {
  structure(list(value = value, code = code), class = "nm_cv")
}

#' Replace nm_cv elements of an argument list by their values
#' @noRd
nm_cv_values <- function(args) {
  lapply(args, function(a) if (inherits(a, "nm_cv")) a$value else a)
}

#' Literal R code for a value
#' @noRd
nm_lit <- function(x) {
  if (inherits(x, "nm_cv")) {
    return(x$code)
  }
  if (is.null(x)) {
    return("NULL")
  }
  if (inherits(x, "formula")) {
    return(paste(deparse(x, width.cutoff = 500L), collapse = " "))
  }
  if (is.character(x)) {
    x <- gsub("\\\\", "/", x) # Windows paths
  }
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x)
    items <- vapply(seq_along(x), function(i) {
      v <- nm_lit(x[[i]])
      if (!is.null(nms) && nzchar(nms[i])) paste0(nm_name(nms[i]), " = ", v) else v
    }, character(1))
    return(paste0("list(", paste(items, collapse = ", "), ")"))
  }
  if (is.atomic(x)) {
    nms <- names(x)
    x <- as.vector(if (is.factor(x)) as.character(x) else unclass(x))
    names(x) <- nms
  }
  paste(deparse(x, width.cutoff = 500L), collapse = "")
}

#' Syntactic argument name
#' @noRd
nm_name <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", x, "`"))
}

#' Code for a function call, one argument per line
#'
#' @param fun character. Function name as written in the script (e.g. "flexsdm::fit_glm").
#' @param args named list of arguments (values or nm_cv objects).
#' @param assign character. Name of the object receiving the result, or NULL.
#' @noRd
nm_code_call <- function(fun, args, assign = NULL) {
  lhs <- if (is.null(assign)) "" else paste0(assign, " <- ")
  if (length(args) == 0) {
    return(paste0(lhs, fun, "()"))
  }
  nms <- names(args)
  if (is.null(nms)) nms <- rep("", length(args))
  arg_code <- vapply(seq_along(args), function(i) {
    v <- nm_lit(args[[i]])
    if (nzchar(nms[i])) paste0(nm_name(nms[i]), " = ", v) else v
  }, character(1))
  paste0(lhs, fun, "(\n  ", paste(arg_code, collapse = ",\n  "), "\n)")
}

#' Sections of the modelling workflow, in script order
#' @noRd
nm_script_sections <- function() {
  c(
    setup = "Project directory",
    occurrences = "Occurrence data",
    layer = "Study area",
    predictors = "Predictors",
    outliers = "Environmental outliers",
    filtering = "Occurrence filtering",
    colinearity = "Collinearity",
    pca = "Principal components as predictors",
    constraints = "Spatial constraint (a priori)",
    calibration = "Calibration area",
    partition_preview = "Data partition (preview)",
    background = "Background / pseudo-absence points",
    partition = "Data partition",
    extraction = "Environmental data extraction",
    modeling = "Model fitting",
    varimp = "Variable importance",
    response = "Response curves",
    ensemble = "Ensemble",
    prediction = "Spatial predictions",
    extrapolation = "Extrapolation",
    uncertainty = "Uncertainty (bootstrap)",
    posteriori = "Overprediction correction"
  )
}

#' Direct upstream sections of each section (what each step uses)
#' @noRd
nm_script_deps <- function() {
  list(
    setup = character(0),
    occurrences = character(0),
    layer = character(0),
    predictors = character(0),
    outliers = c("occurrences", "predictors"),
    filtering = c("occurrences", "outliers", "predictors"),
    colinearity = "predictors",
    pca = "colinearity",
    constraints = c("occurrences", "outliers", "filtering", "predictors", "pca"),
    calibration = c("occurrences", "outliers", "filtering", "layer"),
    partition_preview = c("occurrences", "outliers", "filtering", "predictors", "constraints"),
    background = c("occurrences", "outliers", "filtering", "predictors", "constraints", "calibration", "partition_preview"),
    partition = c("occurrences", "outliers", "filtering", "predictors", "constraints", "background"),
    extraction = c("partition", "predictors", "constraints"),
    modeling = "extraction",
    varimp = c("modeling", "extraction"),
    response = c("modeling", "extraction"),
    ensemble = "modeling",
    prediction = c("modeling", "ensemble", "predictors", "constraints"),
    extrapolation = c("extraction", "predictors", "constraints", "colinearity", "prediction"),
    uncertainty = c("modeling", "extraction", "predictors", "constraints"),
    posteriori = c("prediction", "extraction")
  )
}

#' All sections a section depends on, directly or not
#' @noRd
nm_script_upstream <- function(section) {
  deps <- nm_script_deps()
  up <- character(0)
  todo <- deps[[section]]
  while (length(todo) > 0) {
    up <- union(up, todo)
    todo <- setdiff(unlist(deps[todo]), up)
  }
  up
}

#' Assemble recorded steps into a standalone R script
#'
#' @param steps named list of steps. Each step is a list with `section`, `title`,
#' `code` and `seq` (order in which the step was last run).
#' @param seed numeric. Seed used in the app.
#' @param session_info logical. Append package versions as comments.
#' @return character(1). The script.
#' @noRd
nm_script_build <- function(steps, seed = 123, session_info = TRUE) {
  sections <- nm_script_sections()
  pkgs <- c("nimo", "flexsdm", "terra", "sf", "dplyr")
  version <- function(p) {
    tryCatch(as.character(utils::packageVersion(p)), error = function(e) "not installed")
  }

  header <- c(
    "# Species distribution modelling workflow",
    paste0("# Generated by nimo ", version("nimo"), " on ", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "# File paths are those selected in the app.",
    "",
    "# Packages ----",
    paste0("library(", pkgs, ")"),
    "",
    "# Settings ----",
    paste0("seed <- ", nm_lit(seed)),
    ""
  )

  if (length(steps) == 0) {
    body <- c("# No step has been run in the app yet.", "")
  } else {
    rank <- match(vapply(steps, `[[`, "", "section"), names(sections))
    seqs <- vapply(steps, `[[`, 0, "seq")
    ord <- order(rank, seqs)
    steps <- steps[ord]
    rank <- rank[ord]
    seqs <- seqs[ord]

    body <- character(0)
    n <- 0
    current <- NA
    for (i in seq_along(steps)) {
      st <- steps[[i]]
      if (!identical(current, st$section)) {
        n <- n + 1
        current <- st$section
        body <- c(body, paste0("# ", n, ". ", sections[[st$section]], " ----"))
      }
      if (!is.null(st$title) && nzchar(st$title)) {
        body <- c(body, paste0("## ", st$title))
      }
      sections_run <- vapply(steps, `[[`, "", "section")
      if (any(sections_run %in% nm_script_upstream(st$section) & seqs > seqs[i])) {
        body <- c(body, "# Note: an earlier step was re-run in the app after this one.")
      }
      body <- c(body, st$code, "")
    }
  }

  footer <- character(0)
  if (session_info) {
    footer <- c(
      "# Session information ----",
      paste0("# ", R.version.string),
      paste0("# ", pkgs, " ", vapply(pkgs, version, ""), collapse = NULL),
      ""
    )
  }
  paste(c(header, body, footer), collapse = "\n")
}
