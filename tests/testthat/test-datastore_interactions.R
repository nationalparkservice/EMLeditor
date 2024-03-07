BICY_metadata <- DPchecker::load_metadata(testthat::test_path("good", "BICY"))

test_that("set_datastore_doi generates a draft reference on datastore", {
  mockr::local_mock()
})
