test_that("nm_lit writes valid R literals", {
  expect_equal(nimo:::nm_lit(NULL), "NULL")
  expect_equal(nimo:::nm_lit("a"), "\"a\"")
  expect_equal(nimo:::nm_lit(c(method = "kfold", folds = "10")), "c(method = \"kfold\", folds = \"10\")")
  expect_equal(nimo:::nm_lit(TRUE), "TRUE")
  expect_equal(nimo:::nm_lit("C:\\data\\occ.csv"), "\"C:/data/occ.csv\"")
  expect_equal(nimo:::nm_lit(pr_ab ~ s(x) + y), "pr_ab ~ s(x) + y")
  expect_equal(nimo:::nm_lit(noquote("POLYGON((0 0))")), "\"POLYGON((0 0))\"")
  expect_equal(nimo:::nm_lit(factor("b", levels = c("a", "b"))), "\"b\"")
  expect_equal(nimo:::nm_lit(list(a = 1, `b c` = "x")), "list(a = 1, `b c` = \"x\")")
  expect_equal(nimo:::nm_lit(nimo:::nm_cv(mtcars, "sdm_data")), "sdm_data")
})

test_that("nm_code_call produces code equivalent to the app call", {
  args <- list(data = nimo:::nm_cv(mtcars, "cars"), n = 3L)
  code <- nimo:::nm_code_call("head", args, assign = "out")
  expect_equal(code, "out <- head(\n  data = cars,\n  n = 3L\n)")
  env <- new.env()
  env$cars <- mtcars
  eval(parse(text = sub("data = ", "x = ", code)), env)
  expect_identical(env$out, do.call(head, list(x = mtcars, n = 3L)))
})

test_that("nm_script_build orders steps and flags stale steps", {
  steps <- list(
    model = list(section = "modeling", title = "GLM", code = "m_glm <- 1", seq = 2),
    occ = list(section = "occurrences", title = "", code = "occ <- 1", seq = 3),
    env = list(section = "predictors", title = "", code = "env <- 1", seq = 1)
  )
  script <- nimo:::nm_script_build(steps, seed = 42, session_info = FALSE)
  expect_true(grepl("seed <- 42", script, fixed = TRUE))
  pos <- vapply(c("occ <- 1", "env <- 1", "m_glm <- 1"),
                function(x) regexpr(x, script, fixed = TRUE), integer(1))
  expect_true(all(diff(pos) > 0))
  # occurrences (seq 3) re-run after the model (2), which depends on it;
  # predictors (1) do not depend on occurrences
  expect_equal(lengths(regmatches(script, gregexpr("Note: an earlier step", script))), 1)
  expect_true(regexpr("Note:", script) > regexpr("env <- 1", script))
  expect_silent(parse(text = script))
  expect_true(grepl("No step", nimo:::nm_script_build(list(), session_info = FALSE)))
})

test_that("script section dependencies are consistent", {
  sections <- names(nimo:::nm_script_sections())
  deps <- nimo:::nm_script_deps()
  expect_setequal(names(deps), sections)
  for (s in names(deps)) {
    expect_true(all(deps[[s]] %in% sections), info = s)
    expect_false(s %in% deps[[s]], info = s)
    # dependencies come earlier in the script
    expect_true(all(match(deps[[s]], sections) < match(s, sections)), info = s)
  }
})
