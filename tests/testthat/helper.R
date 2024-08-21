test_path <- function(...) {
  # testthat::test_path() returns incorrect value
  # when in the context of devtools::test_coverage_active_file()
  path <- file.path(...)
  if (file.exists(path)) {
    path
  } else {
    file.path("tests/testthat/", path)
  }
}
