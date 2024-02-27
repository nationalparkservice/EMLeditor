good_dir <- here::here("tests", "testthat", "good")
bad_dir <- here::here("tests", "testhat", "bad")





#load datasets etc to to test functions with
BICY_EMLed_meta <- EML::read_eml(here::here("tests",
                                            "testthat",
                                            "good",
                                            "BICY",
                                            "BICY_EMLeditor_metadata.xml"),
                                 from="xml")

BUIS_EMLed_meta <- EML::read_eml(here::here("tests",
                                            "testthat",
                                            "good",
                                            "BUIS_Herps_test",
                                            "BUIS_EMLeditor_metadata.xml"),
                                 from="xml")

# ---- set_title ----
test_that("set_title works on valid eml", {
  title <- "Test Title"
  new_meta <- set_title(BICY_EMLed_meta, title, force=TRUE)
  expect_equal(new_meta$dataset$title, title)
})


test_that("set_title does not change title if user says not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)

    title <- "Test Title"
    new_meta <- set_title(BICY_EMLed_meta, title, force = FALSE, NPS = FALSE)

    expect_equal(new_meta$dataset$title, BICY_EMLed_meta$dataset$title)
  })
})

test_that("set_title changes if users says to change", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)

    title <- "Test Title"
    new_meta <- set_title(BICY_EMLed_meta, title, force = FALSE, NPS = FALSE)

    expect_equal(new_meta$dataset$title, title)
      })
})

test_that("set_title produces valid EML when title is changed", {
  title <- "Test Title"
  new_meta <- set_title(BICY_EMLed_meta, title, force=TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})



# ---- set_doi ----
test_that("set_doi adds doi on valid eml", {
  # No existing DOI
  doi <- "1234567"
  new_meta_doi <- set_doi(BICY_EMLed_meta, doi, force=TRUE)
  expect_match(new_meta_doi$dataset$alternateIdentifier, doi)
}
)

test_that("set_doi returns valid eml, interactive mode on", {
  doi <- "1234567"
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta_doi <- set_doi(BICY_EMLed_meta, doi, force=TRUE)
    validation <- EML::eml_validate(new_meta_doi)
    expect_equal(validation[1], TRUE)
    })
})

test_that("set_doi works on eml with a valid doi", {
  doi <- "1234567"
  new_meta_doi <- set_doi(BICY_EMLed_meta, doi, force=TRUE) #add a doi
  # Replace previously existing DOI
  another_doi <- "7654321"
  new_meta_replace_doi <- set_doi(new_meta_doi, another_doi, force=TRUE)
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

# ----- test set_missing_data ----

test_that("set_missing_data retruns valid EML, interactive1", {
return_val_1 <- function() {1}
local({mockr::local_mock(.get_user_input = return_val_1)

  x <- set_missing_data(eml_object = BICY_EMLed_meta,
                   file = "Mini_BICY_Veg_Transect_Cleaned.csv",
                   columns = "scientificName",
                   codes = "NA",
                   definitions = "unidentifiable",
                   force = FALSE,
                   NPS = TRUE)
  validation <- EML::eml_validate(x)
  expect_equal(validation[1], TRUE)
  })

})

test_that("set_missing_data retruns valid EML, interactive2", {
  return_val_1 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_1)

    x <- set_missing_data(eml_object = BICY_EMLed_meta,
                          file = "Mini_BICY_Veg_Transect_Cleaned.csv",
                          columns = "scientificName",
                          codes = "NA",
                          definitions = "unidentifiable",
                          force = FALSE,
                          NPS = TRUE)
    validation <- EML::eml_validate(x)
    expect_equal(validation[1], TRUE)
  })

})

test_that("set_missing_data retruns valid EML, scripting method", {
  x <- set_missing_data(eml_object = BICY_EMLed_meta,
                        file = "Mini_BICY_Veg_Transect_Cleaned.csv",
                        columns = "scientificName",
                        codes = "NA",
                        definitions = "unidentifiable",
                        force = TRUE,
                        NPS = TRUE)
  validation <- EML::eml_validate(x)
  expect_equal(validation[1], TRUE)
})

test_that("set_missing_data handles vectorized input", {
  x <- set_missing_data(eml_object = BICY_EMLed_meta,
                        file = c("Mini_BICY_Veg_Transect_Cleaned.csv",
                                 "Mini_BICY_Veg_Transect_Cleaned.csv",
                                 "Mini_BICY_Veg_Geography.csv"),
                        columns = c("scientificName",
                                    "vernacularName",
                                    "parkID"),
                        codes = c("NA", "NA", "empty"),
                        definitions = c("Not recorded",
                                        "Not recorded",
                                        "Intentionally blank - database error"),
                        force = TRUE,
                        NPS = TRUE)
  validation <- EML::eml_validate(x)
  expect_equal(validation[1], TRUE)
})
