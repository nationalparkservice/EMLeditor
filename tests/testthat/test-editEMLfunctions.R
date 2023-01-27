good_dir <- here::here("tests", "testthat", "good")
bad_dir <- here::here("tests", "testhat", "bad")

#load datasets etc to to test functions with
BICY_meta <- EML::read_eml(here::here("tests", "testthat", "good", "BICY",
                                      "BICY_metadata.xml"),
                           from ="xml")
BICY_EMLed_meta <- EML::read_eml(here::here("tests",
                                            "testthat", "good", "BICY",
                                            "BICY_EMLeditor_metadata.xml"),
                                 from="xml")
BUIS_meta <- EML::read_eml(here::here("tests", "testthat", "good", "BUIS",
                                      "BUIS_metadata.xml"),
                           from="xml")
BUIS_EMLed_meta <- EML::read_eml(here::here("tests", "testthat", "good", "BUIS",
                                            "BUIS_EMLeditor_metadata.xml"),
                                 from="xml")

# ---- set_title ----
test_that("set_title works on valid eml", {
  title <- "Test Title"
  new_meta <- set_title(BICY_meta, title, force=TRUE)
  expect_equal(new_meta$dataset$title, title)
}
)

# ---- set_doi ----
test_that("set_doi works on valid eml", {
  # No existing DOI
  doi <- "fakedoi"
  new_meta_doi <- set_doi(BICY_meta, doi, force=TRUE)
  expect_match(new_meta_doi$dataset$alternateIdentifier, doi)

  # Replace previously existing DOI
  another_doi <- "anotherfakedoi"
  new_meta_replace_doi <- set_doi(new_meta_doi, doi, force=TRUE)
  expect_match(new_meta_replace_doi$dataset$alternateIdentifier, another_doi)
}
)

# ---- set_content_units ----
test_that("set_content_units works on valid eml", {
  new_units <- c("JOTR", "DEVA")
  new_meta <- set_content_units(BICY_meta, new_units, force=TRUE)
  geo_desc <- arcticdatautils::eml_get_simple(new_meta, "geographicDescription")
  expect_true(all(c("NPS Content Unit Link: JOTR", "NPS Content Unit Link: DEVA") %in% geo_desc))

  # Try it with a single unit that is not a capital-P park
  new_units <- c("PARA")
  new_meta <- set_content_units(new_meta, new_units, force=TRUE)
  geo_desc <- arcticdatautils::eml_get_simple(new_meta, "geographicDescription")
  expect_true("NPS Content Unit Link: PARA" %in% geo_desc)
}
)
