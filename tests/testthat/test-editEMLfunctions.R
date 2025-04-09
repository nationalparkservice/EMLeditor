#load metadata to to test functions with.
BICY_EMLed_meta <- EML::read_eml(testthat::test_path("good",
                                            "BICY",
                                            "BICY_EMLeditor_metadata.XML"),
                                 from="xml")

# a couple of house-keeping functions for mockr interactivity:
return_val_2 <- function() {2}
return_val_1 <- function() {1}

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
  local({mockr::local_mock(.get_user_input3 = return_val_2)
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
  x <- get_cui_code(new_meta)
  expect_equal(x, "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
})

test_that("set_cui_code updates when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
     cui <- "NOCON"
     new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = FALSE)
     x <- get_cui_code(new_meta)
     expect_equal(x,
                  "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
   })
})

test_that("set_cui_code does not update when requested not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    cui <- "NOCON"
    new_meta <- set_cui_code(BICY_EMLed_meta, cui_code = cui, force = FALSE)
    x <- get_cui_code(new_meta)
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
  x <- get_cui_code(new_meta)
  expect_equal(x, "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.")
})

test_that("set_cui updates when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    cui <- "NOCON"
    new_meta <- suppressWarnings(set_cui_code(BICY_EMLed_meta,
                                              cui_code = cui,
                                              force = FALSE))
    x <- get_cui_code(new_meta)
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
    x <- get_cui_code(new_meta)
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
  new_meta3 <- (set_cui_code(new_meta2,
                            cui_code = code2,
                            force = TRUE))
  new_meta4 <- set_cui_marking(new_meta3,
                               cui_marking = marking2,
                               force = FALSE)
  x <- get_cui_marking(new_meta4)
  expect_equal(x, "Your CUI marking is set to SP-HISTP. This means the CUI in the data is related to the location character, or ownership of historic property.")
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
    new_meta3 <- (set_cui_code(new_meta2,
                               cui_code = code2,
                               force = TRUE))
    new_meta4 <- set_cui_marking(new_meta3,
                                 cui_marking = marking2,
                                 force = FALSE)
    x <- get_cui_marking(new_meta4)
    expect_equal(x, "Your CUI marking is set to PUBLIC. This means the data do not contain CUI.")
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

test_that("set_additional_info returns valid EML", {
  add_info <- "Here is some text for the Notes section on DataStore."
  new_meta <- set_additional_info(BICY_EMLed_meta,
                                  additional_info = add_info,
                                  force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_additinal_info updates additional info", {
  add_info <- "Here is some text for the Notes section on DataStore."
  new_meta <- set_additional_info(BICY_EMLed_meta,
                                  additional_info = add_info,
                                  force = TRUE)
  expect_equal(get_additional_info(new_meta), add_info)
})

test_that("set_additional_info updates additional info when none existed", {
  add_info <- "Here is some text for the Notes section on DataStore."
  new_meta <- set_additional_info(BICY_EMLed_meta,
                                    additional_info = add_info,
                                    force = FALSE)
  expect_equal(get_additional_info(new_meta), add_info)
})

test_that("set_additional_info updates additional info when requested", {
  add_info <- "Here is some text for the Notes section on DataStore."
  add_info2 <- "alternate additional info text"
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_additional_info(BICY_EMLed_meta,
                                    additional_info = add_info,
                                    force = TRUE)
    new_meta2 <- set_additional_info(new_meta,
                                     additional_info = add_info2,
                                     force = FALSE)
    expect_equal(get_additional_info(new_meta2),
                                     add_info2)
    })
})

test_that("set_additional_info does not update additional info on request", {
  add_info <- "Here is some text for the Notes section on DataStore."
  add_info2 <- "alternate additional info text"
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_additional_info(BICY_EMLed_meta,
                                    additional_info = add_info,
                                    force = TRUE)
    new_meta2 <- set_additional_info(new_meta,
                                     additional_info = add_info2,
                                     force = FALSE)
    expect_equal(get_additional_info(new_meta),
                 get_additional_info(new_meta2))
  })
})

# ----- test set_methods ----

test_that("set_methods returns valid EML", {
  new_methods <- "Here are some methods we performed."
  new_meta <- set_methods(BICY_EMLed_meta,
                          method = new_methods,
                          force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_methods updates methods", {
  new_methods <- "Here are some methods we performed."
  new_meta <- set_methods(BICY_EMLed_meta,
                          method = new_methods,
                          force = TRUE)
  expect_match(new_methods, get_methods(new_meta)[[1]][[1]][[1]])
})

test_that("set_methods updates methods upon user request", {
  new_methods <- "Here are some methods we performed."
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_methods(BICY_EMLed_meta,
                            method = new_methods,
                            force = FALSE)
    expect_match(new_methods, get_methods(new_meta)[[1]][[1]][[1]])
    })
})

test_that("set_methods does not update methods when user requests not to", {
  new_methods <- "Here are some methods we performed."
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_methods(BICY_EMLed_meta,
                            method = new_methods,
                            force = FALSE)
    expect_equal(get_methods(BICY_EMLed_meta),
                 get_methods(new_meta))
  })
})

# ----- test set_lit -----

#### This function is not working yet!

# ----- test set_producing_units ----

test_that("set_producing_units returns valid EML", {
  prod_units <- c("ABCD", "EFGH")
  new_meta <- set_producing_units(eml_object = BICY_EMLed_meta,
                                  prod_units = prod_units,
                                  force = TRUE,
                                  NPS = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_producing_units updates producing units", {
  prod_units <- c("ABCD", "EFGH")
  new_meta <- set_producing_units(eml_object = BICY_EMLed_meta,
                                  prod_units = prod_units,
                                  force = TRUE,
                                  NPS = TRUE)
  expect_equal(sum(get_producing_units(new_meta) == prod_units), 2)
})

test_that("set_producing_units updates producing units when requested", {
  prod_units <- c("ABCD", "EFGH")
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_producing_units(eml_object = BICY_EMLed_meta,
                                    prod_units = prod_units,
                                    force = FALSE,
                                    NPS = TRUE)
    expect_equal(sum(get_producing_units(new_meta) == prod_units), 2)
    })
})

test_that("set_producing_units doesn't update producing units when requested", {
  prod_units <- c("ABCD", "EFGH")
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_producing_units(eml_object = BICY_EMLed_meta,
                                    prod_units = prod_units,
                                    force = FALSE,
                                    NPS = TRUE)
    expect_equal(get_producing_units(new_meta),
                 get_producing_units(BICY_EMLed_meta))
  })
})

# ----- test set_language ----

test_that("set_language returns valid EML", {
  language <- "english"
  new_meta <- set_language(BICY_EMLed_meta,
                           lang = language,
                           force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_language adds the language code to metadata", {
  language <- "english"
  new_meta <- set_language(BICY_EMLed_meta,
                           lang = language,
                           force = TRUE)
  expect_equal(new_meta[["dataset"]][["language"]], "eng")
})

test_that("set_language updated language when requested by user", {
  language <- "spanish"
  language2 <- "english"
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_language(BICY_EMLed_meta,
                             lang = language,
                             force = TRUE)
    new_meta2 <- set_language(new_meta,
                              lang = language2,
                              force = FALSE)
    expect_equal(new_meta2[["dataset"]][["language"]], "eng")
  })
})

test_that("set_language doesn't update language when requested not by user", {
  language <- "spanish"
  language2 <- "english"
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_language(BICY_EMLed_meta,
                             lang = language,
                             force = TRUE)
    new_meta2 <- set_language(new_meta,
                              lang = language2,
                              force = FALSE)
    expect_equal(new_meta2[["dataset"]][["language"]], "spa")
  })
})

# ---- test set_protocol ----

test_that("set_protocol won't accept a poorly formatted project_reference_id",
          {
            expect_error(set_project(BICY_EMLed_meta, 1234))
          })

## Not sure how to add more tests; probably need snapshots to handle API calls.

# ---- test set_publisher ----

test_that("set_publisher returns valid EML", {
  new_meta <- set_publisher(BICY_EMLed_meta,
                "BroadLeaf",
                "123 First Street",
                "Second City",
                "CO",
                "12345",
                "USA",
                "https://www.organizationswebsite.com",
                "contact@myorganization.com",
                "https://ror.org/xxxxxxxxx",
                for_or_by_NPS = FALSE,
                force = TRUE,
                NPS = FALSE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_publisher correctly updates publisher information", {
  new_meta <- set_publisher(BICY_EMLed_meta,
                            "BroadLeaf",
                            "123 First Street",
                            "Second City",
                            "CO",
                            "12345",
                            "USA",
                            "https://www.organizationswebsite.com",
                            "contact@myorganization.com",
                            "https://ror.org/xxxxxxxxx",
                            for_or_by_NPS = FALSE,
                            force = TRUE,
                            NPS = FALSE)
  expect_equal(names(get_publisher(new_meta)),
               c("organizationName",
                    "address",
                    "onlineUrl",
                    "electronicMailAddress",
                    "userId"))
})

test_that("set_publisher correctly updates publisher information on request", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_publisher(BICY_EMLed_meta,
                              "BroadLeaf",
                              "123 First Street",
                              "Second City",
                              "CO",
                              "12345",
                              "USA",
                              "https://www.organizationswebsite.com",
                              "contact@myorganization.com",
                              "https://ror.org/xxxxxxxxx",
                              for_or_by_NPS = FALSE,
                              force = TRUE,
                              NPS = FALSE)
    new_meta2 <- set_publisher(new_meta,
                               "Leaf",
                               "3 Street",
                               "S City",
                               "CO",
                               "54321",
                               "SUA",
                               "https://www.organizationswebsite.com",
                               "contact@mycompany.com",
                               "https://ror.org/yyyyyyyy",
                               for_or_by_NPS = FALSE,
                               force = FALSE,
                               NPS = FALSE)
  expect_equal(get_publisher(new_meta2)[[1]], "Leaf")
  })
})

test_that("set_publisher does not update publisher when asked not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_publisher(BICY_EMLed_meta,
                              "BroadLeaf",
                              "123 First Street",
                              "Second City",
                              "CO",
                              "12345",
                              "USA",
                              "https://www.organizationswebsite.com",
                              "contact@myorganization.com",
                              "https://ror.org/xxxxxxxxx",
                              for_or_by_NPS = FALSE,
                              force = TRUE,
                              NPS = FALSE)
    new_meta2 <- set_publisher(new_meta,
                               "Leaf",
                               "3 Street",
                               "S City",
                               "CO",
                               "54321",
                               "SUA",
                               "https://www.organizationswebsite.com",
                               "contact@mycompany.com",
                               "https://ror.org/yyyyyyyy",
                               for_or_by_NPS = FALSE,
                               force = FALSE,
                               NPS = FALSE)
    expect_equal(get_publisher(new_meta2)[[1]], "BroadLeaf")
  })
})

# ----- set_int_rights ----

test_that("set_int_rights returns valid EML", {
  new_meta <- set_cui_code(BICY_EMLed_meta, "PUBLIC", force = TRUE)
  new_meta2 <- set_int_rights(new_meta,
                             license = "public",
                             force = TRUE)
  expect_equal(EML::eml_validate(new_meta2)[1], TRUE)
})

test_that("set_int_rights updates intellectual rights statement", {
  new_meta <- set_cui_code(BICY_EMLed_meta, "PUBLIC", force = TRUE)
  new_meta2 <- set_int_rights(new_meta,
                              license = "public",
                              force = TRUE)
  expect_equal(stringr::str_length(
    new_meta2[["dataset"]][["intellectualRights"]]),
    145)
})

test_that("set_int_rights updates intellectual rights when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_cui_code(BICY_EMLed_meta, "PUBLIC", force = TRUE)
    new_meta2 <- set_int_rights(new_meta,
                                license = "public",
                                force = FALSE)
    expect_equal(stringr::str_length(
      new_meta2[["dataset"]][["intellectualRights"]]),
      145)
  })
})

test_that("set_int_rights does not update when conflict exists:", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_cui_code(BICY_EMLed_meta, "PUBLIC", force = TRUE)
    new_meta2 <- set_int_rights(new_meta,
                                license = "restricted",
                                force = FALSE)
    expect_false(stringr::str_length(
      new_meta2[["dataset"]][["intellectualRights"]]) ==
      145)
  })
})

# ----- test set_data_urls

test_that("set_data_urls returns valid EML", {
  doi <- 1234567
  new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
  new_meta2 <- set_data_urls(new_meta, force = TRUE)
  expect_equal(EML::eml_validate(new_meta2)[1], TRUE)
})

test_that("set_data_urls updates urls", {
  doi <- 1234567
  new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
  new_meta2 <- set_data_urls(new_meta, force = TRUE)
  expect_equal(new_meta2[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[1]],
               "https://irma.nps.gov/DataStore/Reference/Profile/1234567")
})

test_that("set_data_urls adds function = \"information\" tag", {
  doi <- 1234567
  new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
  new_meta2 <- set_data_urls(new_meta, force = TRUE)
  expect_equal(new_meta2[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[2]],
               "information")
})

test_that("set_data_urls updates custom urls", {
  doi <- 1234567
  test_url <- "https://wwww.thisisatest.com"
  new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
  new_meta2 <- set_data_urls(new_meta, url = test_url, force = TRUE)
  expect_equal(new_meta2[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[1]],
               test_url)
})

test_that("set_data_urls updates urls upon user request", {
  doi <- 1234567
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
    new_meta2 <- set_data_urls(new_meta, force = FALSE)
    expect_equal(new_meta2[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[1]],
                 "https://irma.nps.gov/DataStore/Reference/Profile/1234567")
  })
})

test_that("set_data_urls does not update urls upon user request not to", {
  doi <- 1234567
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    new_meta <- set_doi(BICY_EMLed_meta, 1234567, force = TRUE)
    new_meta2 <- set_data_urls(new_meta, force = FALSE)
    expect_false(new_meta2[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[1]] ==
                 BICY_EMLed_meta[["dataset"]][["dataTable"]][[1]][["physical"]][["distribution"]][["online"]][["url"]][[1]])
  })
})

test_that("set_data_urls only accepts information or download for tag element", {
  expect_error(set_data_urls(BICY_EMLed_meta, url = "https://tst.com", tag = "test_tag"), "Error: The tag parameter must be either \"information\" or \"download\"")
})

# ----- test set_creator_orcids ----

test_that("set_creator_orcids returns valid EML", {
  creator_orcids <- c("1234-1234-1234-1234",
                      "4321-4321-4321-4321")
  new_meta <- set_creator_orcids(BICY_EMLed_meta,
                                 orcids = creator_orcids,
                                 force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_creator_orcids adds orcids to metadata", {
  creator_orcids <- c("1234-1234-1234-1234")
  new_meta <- set_creator_orcids(BICY_EMLed_meta,
                                 orcids = creator_orcids,
                                 force = TRUE)
  expect_match(new_meta[["dataset"]][["creator"]][[1]][["userId"]][[1]][["userId"]], creator_orcids)
})

test_that("set_creator_orcids adds orcids when requested", {
  return_val_1 <- function() {1}
  local({mockr::local_mock(.get_user_input = return_val_1)
    creator_orcids <- c("1234-1234-1234-1234")
    new_meta <- set_creator_orcids(BICY_EMLed_meta,
                                   orcids = creator_orcids,
                                   force = FALSE)
    expect_match(new_meta[["dataset"]][["creator"]][[1]][["userId"]][[1]][["userId"]], creator_orcids)
  })
})

test_that("set_creator_orcids does not add orcids when requested not to", {
  return_val_2 <- function() {2}
  local({mockr::local_mock(.get_user_input = return_val_2)
    creator_orcids <- c("1234-1234-1234-1234")
    new_meta <- set_creator_orcids(BICY_EMLed_meta,
                                   orcids = creator_orcids,
                                   force = FALSE)
    expect_equal(
      new_meta[["dataset"]][["creator"]][[1]][["userId"]][[1]][["userId"]],
      BICY_EMLed_meta[["dataset"]][["creator"]][[1]][["userId"]][[1]][["userId"]])
  })
})

# ----- test set_creator_orgs ----

test_that("set_creator_orgs returns valid EML", {
  new_meta <- set_creator_orgs(BICY_EMLed_meta,
                               "National Park Service",
                               RORs="044zqqy65",
                               force = TRUE)
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_creator_orgs adds organization to EML", {
  new_meta <- set_creator_orgs(BICY_EMLed_meta,
                               "National Park Service",
                               RORs="044zqqy65",
                               force = TRUE)
  expect_equal(length(seq_along(new_meta[["dataset"]][["creator"]])),
               2)
})

test_that("set_creator_orgs adds organization when force = FALSE", {
  new_meta <- set_creator_orgs(BICY_EMLed_meta,
                               "National Park Service",
                               RORs="044zqqy65",
                               force = FALSE)
  expect_equal(length(seq_along(new_meta[["dataset"]][["creator"]])),
               2)
})

# ----- test set_new_creator ----

test_that("set_new_creator returns valid EML", {
  new_meta <- set_new_creator(BICY_EMLed_meta,
                                  last_name = c("Doe", "Smith"),
                                  first_name = c("John", "Jane"),
                                  middle_name = c(NA, "S."),
                                  organization_name = c("NPS", "UCLA"),
                                  email_address = c("john_doe@@nps.gov", NA))
  expect_equal(EML::eml_validate(new_meta)[1], TRUE)
})

test_that("set_new_creator adds creators to EML", {
  new_meta <- set_new_creator(BICY_EMLed_meta,
                              last_name = c("Doe", "Smith"),
                              first_name = c("John", "Jane"),
                              middle_name = c(NA, "S."),
                              organization_name = c("NPS", "UCLA"),
                              email_address = c("john_doe@@nps.gov", NA))
  creator <- new_meta[["dataset"]][["creator"]]
  expect_equal(length(seq_along(creator)), 3)
})

# ----- test set_creator_order ----

test_that("set_creator_order returns valid EML", {
  new_meta <- set_creator_orgs(BICY_EMLed_meta,
                               "National Park Service",
                               RORs="044zqqy65",
                               force = TRUE)
  new_meta2 <- set_creator_order(new_meta,
                                 new_order = c(2,1),
                                 force = TRUE)
  expect_equal(EML::eml_validate(new_meta2)[1], TRUE)
})

test_that("set_creator_order updates creator order", {
  new_meta <- set_creator_orgs(BICY_EMLed_meta,
                               "National Park Service",
                               RORs="044zqqy65",
                               force = TRUE)
  new_meta2 <- set_creator_order(new_meta,
                                 new_order = c(2,1),
                                 force = TRUE)
  expect_false(identical(new_meta2[["dataset"]][["creator"]],
                         new_meta[["dataset"]][["creator"]]))
})

test_that("set_creator_order reorders creators upon request", {
  return_val_list_2_1 <- function() {c("2, 1")}
  local({mockr::local_mock(.get_user_input3 = return_val_list_2_1)
    new_meta <- set_creator_orgs(BICY_EMLed_meta,
                                 "National Park Service",
                                 RORs="044zqqy65",
                                 force = TRUE)
    new_meta2 <- set_creator_order(new_meta,
                                   force = FALSE)
    expect_false(identical(new_meta2[["dataset"]][["creator"]],
                           new_meta[["dataset"]][["creator"]]))
  })
})

test_that("set_creator_order retains order if asked to", {
  return_val_list_1_2 <- function() {c("1, 2")}
  local({mockr::local_mock(.get_user_input3 = return_val_list_1_2)
    new_meta <- set_creator_orgs(BICY_EMLed_meta,
                                 "National Park Service",
                                 RORs="044zqqy65",
                                 force = TRUE)
    new_meta2 <- set_creator_order(new_meta,
                                   force = FALSE)
    expect_equal(new_meta2[["dataset"]][["creator"]],
                           new_meta[["dataset"]][["creator"]])
  })
})

test_that("set_creator_order removes a creator when requested", {
  return_val_list_2 <- function() {2}
  local({mockr::local_mock(.get_user_input3 = return_val_list_2)
    new_meta <- set_creator_orgs(BICY_EMLed_meta,
                                 "National Park Service",
                                 RORs="044zqqy65",
                                 force = TRUE)
    new_meta2 <- set_creator_order(new_meta,
                                   force = FALSE)
    expect_equal(length(seq_along(new_meta2[["dataset"]][["creator"]])),
                 1)
  })
})

# ----- test set_missing_data ----

test_that("set_missing_data retruns valid EML, interactive1", {
return_val_1 <- function() {1}
local({mockr::local_mock(.get_user_input = return_val_1)

  x <- set_missing_data(eml_object = BICY_EMLed_meta,
                   files = "Mini_BICY_Veg_Transect_Cleaned.csv",
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
                          files = "Mini_BICY_Veg_Transect_Cleaned.csv",
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
                        files = "Mini_BICY_Veg_Transect_Cleaned.csv",
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
                        files = c("Mini_BICY_Veg_Transect_Cleaned.csv",
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


