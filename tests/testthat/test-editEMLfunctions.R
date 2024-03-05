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
test_that("set_doi adds doi on valid eml, scripting route", {
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
    new_meta_doi <- set_doi(BICY_EMLed_meta, doi, force=FALSE)
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
})

test_that("set_doi does not replace doi when user says not to", {
  doi <- "1234567"
  new_meta_doi <- set_doi(BICY_EMLed_meta, doi, force=TRUE) #add a doi
  # do not Replace previously existing DOI
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta_doi2 <- set_doi(new_meta_doi, doi, force=FALSE)
    expect_equal(new_meta_doi2$dataset$alternateIdentifier,
                 new_meta_doi$dataset$alternateIdentifier)
  })
})

# ---- set_content_units ----
test_that("set_content_units works on valid eml", {
  new_units <- c("JOTR", "DEVA")
  new_meta <- set_content_units(BICY_EMLed_meta, new_units, force=TRUE)
  geo_desc <- arcticdatautils::eml_get_simple(new_meta, "geographicDescription")
  expect_true(all(c("NPS Content Unit Link: JOTR", "NPS Content Unit Link: DEVA") %in% geo_desc))
})

test_that("set_content_units works with a single input", {
  # Try it with a single unit that is not a capital-P park
  new_units <- c("PARA")
  new_meta <- set_content_units(BICY_EMLed_meta, new_units, force=TRUE)
  geo_desc <- arcticdatautils::eml_get_simple(new_meta, "geographicDescription")
  expect_true("NPS Content Unit Link: PARA" %in% geo_desc)
})

test_that("set_content_units retains input when told to", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input3 = return_val_1)
    new_units <- c("PARA")
    new_meta <- set_content_units(BICY_EMLed_meta,
                                  new_units,
                                  force = TRUE)
    new_meta2 <- set_content_units(new_meta,
                                   "YELL",
                                   force = FALSE)
    geo_desc <- arcticdatautils::eml_get_simple(new_meta2, "geographicDescription")
    expect_false("NPS Content Unit Link: YELL" %in% geo_desc)
  })
})

test_that("set_content_units adds input when told to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input3 = return_val_1)
    new_units <- c("PARA")
    new_meta <- set_content_units(BICY_EMLed_meta,
                                  new_units,
                                  force = TRUE)
    new_meta2 <- set_content_units(new_meta,
                                   "YELL",
                                   force = FALSE)
    geo_desc <- arcticdatautils::eml_get_simple(new_meta2, "geographicDescription")
    expect_true("NPS Content Unit Link: YELL" %in% geo_desc)
  })
})

test_that("set_content_units replaces input when told to", {
  return_val_3 <- function() {3}
  local({mockr::local_mock(.get_user_input3 = return_val_3)
    new_units <- c("PARA")
    new_meta <- set_content_units(BICY_EMLed_meta,
                                  new_units,
                                  force = TRUE)
    new_meta2 <- set_content_units(new_meta,
                                   "YELL",
                                   force = FALSE)
    geo_desc <- arcticdatautils::eml_get_simple(new_meta2, "geographicDescription")
    expect_true("NPS Content Unit Link: YELL" %in% geo_desc)
  })
})

test_that("set_content_units works on valid eml", {
  new_units <- c("JOTR", "DEVA")
  new_meta <- set_content_units(BICY_EMLed_meta, new_units, force=TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_content_units produces valid EML with a single input", {
  # Try it with a single unit that is not a capital-P park
  new_units <- c("PARA")
  new_meta <- set_content_units(BICY_EMLed_meta, new_units, force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_content_units returns null for invalid park unit", {
  new_units <- c("TRBX")
  x <- set_content_units(BICY_EMLed_meta, new_units, force = TRUE)
  expect_equal(is.null(x), TRUE)
})

# ----- test set_cui_code ----

test_that("set_cui_code returns valid EML", {
  cui <- "NOCON"
  new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_cui_code updates CUI code in EML", {
  cui <- "NOCON"
  new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = TRUE)
  x <- get_cui(new_meta)
  expect_equal(x, "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
})

test_that("set_cui_code updates when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
     cui <- "NOCON"
     new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = FALSE)
     x <- get_cui(new_meta)
     expect_equal(x,
                  "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
   })
})

test_that("set_cui_code does not update when requested not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    cui <- "NOCON"
    new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = FALSE)
    x <- get_cui(new_meta)
    expect_equal(x,
                 "Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI."
)
  })
})

# ----- test set_cui ------ DEPRECATED
test_that("set_cui is deprecated", {
  rlang::local_options(lifecycle_verbosity = "error")
  cui <- "NOCON"
  expect_error(set_cui(BICY_EMLed_meta,
                       cui_code = cui,
                       force = TRUE),
               class = "defunctError")
})

test_that("set_cui returns valid EML", {
  cui <- "NOCON"
  new_meta <- suppressWarnings(set_cui(BICY_EMLed_meta,
                                       cui_code = cui,
                                       force = TRUE))
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_cui updates CUI code in EML", {
  cui <- "NOCON"
  new_meta <- suppressWarnings(set_cui_code(BICY_EMLed_meta,
                                            cui_code = cui,
                                            force = TRUE))
  x <- get_cui(new_meta)
  expect_equal(x, "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
})

test_that("set_cui updates when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    cui <- "NOCON"
    new_meta <- suppressWarnings(set_cui_code(BICY_EMLed_meta,
                                              cui_code = cui,
                                              force = FALSE))
    x <- get_cui(new_meta)
    expect_equal(x,
                 "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
  })
})

test_that("set_cui does not update when requested not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    cui <- "NOCON"
    new_meta <- suppressWarnings(set_cui_code(BICY_EMLed_meta,
                                              cui_code = cui,
                                              force = FALSE))
    x <- get_cui(new_meta)
    expect_equal(x,
                 "Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI."
    )
  })
})

# ----- test set_cui_marking ----

test_that("set_cui_marking returns valid EML", {
  marking <- "PUBLIC"
  code <- "PUBLIC"
  new_meta <- set_cui_code(BICY_EMLed_meta,
                           cui_code = code,
                           force = TRUE)
  new_meta2 <- set_cui_marking(new_meta,
                               cui_marking = marking,
                               force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_cui_marking adds CUI marking to EML", {
  code <- "PUBLIC"
  marking <- "PUBLIC"
  new_meta <- (set_cui_code(BICY_EMLed_meta,
                            cui_code = code,
                            force = TRUE))
  new_meta2 <- set_cui_marking(new_meta,
                               cui_marking = marking,
                               force = TRUE)
  new_marking <- get_cui_marking(new_meta2)
  expect_equal(new_marking, "Your CUI marking is set to PUBLIC. This means the data do not contain CUI.")
})

test_that("set_cui_marking updates CUI marking interactively", {
  code <- "PUBLIC"
  code2 <- "NOCON"
  marking <- "PUBLIC"
  marking2 <- "SP-HISTP"
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
  new_meta <- (set_cui_code(BICY_EMLed_meta,
                            cui_code = code,
                            force = TRUE))
  new_meta2 <- set_cui_marking(new_meta,
                               cui_marking = marking,
                               force = FALSE)
  new_meta3 <- (set_cui_code(BICY_EMLed_meta,
                            cui_code = code2,
                            force = TRUE))
  new_meta4 <- set_cui_marking(new_meta3,
                               cui_marking = marking2,
                               force = FALSE)
  x <- get_cui_marking(new_meta2)
  expect_equal(x, "Your CUI marking is set to PUBLIC. This means the data do not contain CUI.")
  })
})

test_that("set_cui_marking does not update CUI marking when asked not to",
          {
  code <- "PUBLIC"
  code2 <- "NOCON"
  marking <- "PUBLIC"
  marking2 <- "SP-HISTP"
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- (set_cui_code(BICY_EMLed_meta,
                              cui_code = code,
                              force = TRUE))
    new_meta2 <- set_cui_marking(new_meta,
                                 cui_marking = marking,
                                 force = FALSE)
    new_meta3 <- (set_cui_code(new_meta3,
                               cui_code = code2,
                               force = TRUE))
    new_meta4 <- set_cui_marking(new_meta3,
                                 cui_marking = marking2,
                                 force = FALSE)
    x <- get_cui_marking(new_meta4)
    expect_equal(x, "Your CUI marking is set to SP-HISTP. This means the CUI in the data is related to the location character, or ownership of historic property.")
  })
})

test_that("set_cui_marking doesn't update with identical informatin", {
  code <- "PUBLIC"
  marking <- "PUBLIC"
  new_meta <- (set_cui_code(BICY_EMLed_meta,
                            cui_code = code,
                            force = TRUE))
  new_meta2 <- set_cui_marking(new_meta,
                               cui_marking = marking,
                               force = TRUE)
  new_meta2 <- set_cui_marking(new_meta,
                               cui_marking = marking,
                               force = FALSE)
  x <- get_cui_marking(new_meta2)
  expect_equal(x, "Your CUI marking is set to PUBLIC. This means the data do not contain CUI.")
})

# ----- test set_drr -----

test_that("set_drr returns valid metadata", {
  new_meta <- set_drr(BICY_EMLed_meta,
                      drr_ref_id = 12324567,
                      drr_title = "Test DRR Title",
                      force = TRUE)
  expect_true(EML::eml_validate(new_meta)[1])
})

test_that("set_drr adds the DRR title to metadata", {
  title <- "Test DRR Title"
  new_meta <- set_drr(BICY_EMLed_meta,
                      drr_ref_id = 12324567,
                      drr_title = "Test DRR Title",
                      force = TRUE)
  expect_equal(title, get_drr_title(new_meta))
})

test_that("set_drr adds the DRR DOI to metadata", {
  doi <- 1234567
  new_meta <- set_drr(BICY_EMLed_meta,
                      drr_ref_id = doi,
                      drr_title = "Test DRR Title",
                      force = TRUE)
  expect_match(get_drr_doi(new_meta), as.character(doi))
})

test_that("set_drr adds DRR DOI when requested by user", {
  title <- "Test DRR Title"
  doi <- 1234567
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_drr(BICY_EMLed_meta,
                        drr_ref_id = doi,
                        drr_title = "Test DRR Title",
                        force = FALSE)
    expect_match(get_drr_doi(new_meta), as.character(doi))
  })
})

test_that("set_drr adds DRR title when requested by user", {
  title <- "Test DRR Title"
  doi <- 1234567
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_drr(BICY_EMLed_meta,
                        drr_ref_id = doi,
                        drr_title = "Test DRR Title",
                        force = FALSE)
    expect_equal(get_drr_title(new_meta), title)
  })
})

test_that("set_drr does not change title when requested not to", {
  title <- "Test DRR Title"
  doi <- 1234567
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_drr(BICY_EMLed_meta,
                        drr_ref_id = doi,
                        drr_title = "Test DRR Title",
                        force = FALSE)
    expect_equal(get_drr_title(BICY_EMLed_meta),
                 get_drr_title(new_meta))
    })
})

test_that("set_drr does not change DRR DOI when requested not to", {
  title <- "Test DRR Title"
  doi <- 1234567
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_drr(BICY_EMLed_meta,
                        drr_ref_id = doi,
                        drr_title = "Test DRR Title",
                        force = FALSE)
    expect_equal(get_drr_doi(BICY_EMLed_meta),
                 get_drr_doi(new_meta))
  })
})

# ---- test set_absract ----

test_that("set_abstract returns valid EML", {
  new_abstract <- "This is a short test abstract"
  new_meta <- set_abstract(BICY_EMLed_meta,
                           abstract = new_abstract,
                           force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_abstract updates the abstract", {
  new_abstract <- "This is a short test abstract"
  new_meta <- set_abstract(BICY_EMLed_meta,
                           abstract = new_abstract,
                           force = TRUE)
  expect_equal(get_abstract(new_meta), new_abstract)
})

test_that("set abstract updates abstract on request", {
  new_abstract <- "This is a short test abstract"
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_abstract(BICY_EMLed_meta,
                             abstract = new_abstract,
                             force = FALSE)
    expect_equal(get_abstract(new_meta), new_abstract)
  })
})

test_that("set_abstract does not update abstract when requested not to", {
  new_abstract <- "This is a short test abstract"
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_abstract(BICY_EMLed_meta,
                             abstract = new_abstract,
                             force = FALSE)
    expect_equal(get_abstract(new_meta),
                 get_abstract(BICY_EMLed_meta))
  })
})

# ---- test set_additional_info ----



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
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)

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
