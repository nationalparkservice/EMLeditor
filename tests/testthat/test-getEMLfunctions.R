#load metadata to to test functions with.
BICY_EMLed_meta <- EML::read_eml(testthat::test_path("good",
                                            "BICY",
                                            "BICY_EMLeditor_metadata.XML"),
                                 from="xml")

# a couple of house-keeping functions for mockr interactivity:
return_val_2 <- function() {2}
return_val_1 <- function() {1}

# ---- get_begin_date ----

test_that("get_begin_date gets the beginning date of EML document", {
  date <- get_begin_date(BICY_EMLed_meta)
  expect_equal(date, "08 April 2002")
})

# ---- get_end_date ----

test_that("get_end_date gets the end date of EML document", {
  date <- get_end_date(BICY_EMLed_meta)
  expect_equal(date, "24 June 2006")
})

# ---- get_abstract ----

test_that("get_abstract gets the abstract of an EML document", {
  abstract <- get_abstract(BICY_EMLed_meta)
  expect_match(abstract,
               "1094 unique plant taxa, both previously known and unknown, ")
})

# ---- get_methods ----

test_that("get_methods gets the methods of an EML document", {
  methods <- get_methods(BICY_EMLed_meta)
  expect_match(methods[[1]][[1]][[1]],
               "A quantitative plant inventory of the Big Cypress National")
})

# ---- get_additional_info ----

test_that("get_additional_info gets additionalInfo of an EML document", {
  add_info <- "clearly unique additional information"
  new_meta <- set_additional_info(BICY_EMLed_meta, add_info)
  info <- get_additional_info(new_meta)
  expect_match(info, add_info)
})

# ---- get_title ----

test_that("get_title gets the title of an EML document", {
  title <- get_title(BICY_EMLed_meta)
  expect_equal(title,
               "EXAMPLE: A Quantitative Plant Inventory of Big Cypress National Preserve: Processed Data (Publicly Available)")
})

# ---- get_ds_id ----

test_that("get_ds_id gets the DataStore Reference ID from an EML document", {
  ds_id <- get_ds_id(BICY_EMLed_meta)
  expect_equal(ds_id, "2295086")
})

# ---- get_citaiton ----

test_that("get_citation gets the proper elements and constructs a citation", {
  citation <- get_citation(BICY_EMLed_meta)
  expect_equal(citation, "Quevedo, Issac A. 2022. EXAMPLE: A Quantitative Plant Inventory of Big Cypress National Preserve: Processed Data (Publicly Available). National Park Service. Fort Collins, CO. https://doi.org/10.57830/2295086")
})

# ---- get_author_list ----

test_that("get_author_list gets the authors and properly formats them", {
  authors <- get_author_list(BICY_EMLed_meta)
  expect_equal(authors, "Quevedo, Issac A.")
})

# ---- get_doi ----

test_that("get_doi gets the DOI from EML document (and properly formats)", {
  doi <- get_doi(BICY_EMLed_meta)
  expect_equal(doi, " https://doi.org/10.57830/2295086")
})

# ---- get_content_units ----

test_that("get_content_units gets the content unit links from EML", {
  park_units <- "BICY"
  new_meta <- set_content_units(BICY_EMLed_meta,
                                park_units = park_units,
                                force = TRUE)
  units <- get_content_units(new_meta)
  expect_equal(units, "NPS Content Unit Link(s): BICY")
})

# ---- get_cui_code ----

test_that("get_cui_code returns the CUI dissemination code from EML", {
  cui <- get_cui_code(BICY_EMLed_meta)
  expect_match(cui,
               "Does NOT contain CUI. The original data contained CUI, but in")
})

# ---- get_cui ---- (deprecated)

test_that("get_cui is deprecated", {
  cui <- suppressWarnings(get_cui(BICY_EMLed_meta))
  expect_match(cui,
               "Does NOT contain CUI. The original data")
})

# ---- get_cui_marking ----

test_that("get_cui_marking returns CUI markings", {
  new_meta1 <- set_cui_code(BICY_EMLed_meta,
                           "PUBLIC",
                           force = TRUE)
  new_meta2 <- set_cui_marking(new_meta1,
                               "PUBLIC",
                               force = TRUE)
  marking <- get_cui_marking(new_meta2)
  expect_equal(marking, "Your CUI marking is set to PUBLIC. This means the data do not contain CUI.")
})

# ---- get_file_info ----

test_that("get_file_info gets the correct number of files for a data package", {
  files <- get_file_info(BICY_EMLed_meta)
  expect_equal(nrow(files), 3)
  })

test_that("get_file_info gets the correct files sizes a data package", {
  file_size <- data.frame(c("187.5 KB", "168.8 KB", " 30.8 KB"))
  names(file_size) <- "Size"
  files <- get_file_info(BICY_EMLed_meta)
  expect_equal(files[2], file_size)
})

# ---- get_drr_doi ----

test_that("get_drr_doi gets the Data Release Report DOI from EML", {
  doi <- get_drr_doi(BICY_EMLed_meta)
  expect_equal(doi, "DRR: https://doi.org/10.36967/2294558")
})

# --- get_drr_title ----

test_that("get_drr_title gets the Data Release Report title from EML", {
  title <- get_drr_title(BICY_EMLed_meta)
  expect_match(title,
               "DRR: Quantitative Plant Inventory of Big Cypress National")
})

# ---- get_lit ----

# experimental function; does not yet warrant testing as not completely sure
# what the results SHOULD look like.

# ---- get_producing_units ----

test_that("get_producing_units gets the proper producing units from EML", {
  units <- get_producing_units(BICY_EMLed_meta)
  expect_equal(units[[1]], "SFCN")
})

# ---- get_publisher ----

test_that("get_publisher gets the publisher information from EML", {
  pub <- get_publisher(BICY_EMLed_meta)
  pub_names <- c("organizationName",
                 "address",
                 "electronicMailAddress",
                 "onlineUrl",
                 "userId")
  expect_equal(names(pub), pub_names)
})

# ---- get_attribute_tables ----

# test output class when input is an emld object
test_that("object input returns list output", {
  expect_equal(class(get_attribute_tables(BICY_EMLed_meta, write = FALSE)), "list")
})

# test output class when input is a path to an XML file
test_that("path input returns list output", {
  expect_equal(class(get_attribute_tables(testthat::test_path(testthat::test_path("good",
                                                                                  "BICY",
                                                                                  "BICY_EMLeditor_metadata.XML")), write = FALSE)), "list")
})

# test length of nested attribute table, should be equal to the number of csv files in the data package
test_that("output table length equals number of csvs in data package", {
  expect_equal(length(get_attribute_tables(BICY_EMLed_meta, write = FALSE)), length(list.files(testthat::test_path("good",
                                                                                                                   "BICY")), pattern = "csv"))
})

# test that default write input writes as many attributes text files as data tables
test_that("default write parameters writes same number of txt files as catvar tables", {
  temp_path <- withr::local_tempdir()
  get_attribute_tables(BICY_EMLed_meta, path = temp_path)
  expect_equal(length(list.files(temp_path, pattern = "attributes")), length(list.files(testthat::test_path("good",
                                                                                                            "BICY")), pattern = "csv"))
})

# test bad eml object
test_that("bad object input throws error", {
  expect_error(get_attribute_tables(bad_metadata_object, write = FALSE))
})

# test bad path
test_that("bad path input throws error", {
  expect_error(get_attribute_tables("bad_metadata_path.xml", write = FALSE))
})

# ---- get_catvar_tables ----


# test output class when input is an emld object
test_that("object input returns list output", {
  expect_equal(class(get_catvar_tables(BICY_EMLed_meta, write = FALSE)), "list")
})

# test output class when input is a path to an XML file
test_that("path input returns list output", {
  expect_equal(class(get_catvar_tables(testthat::test_path(testthat::test_path("good",
                                                                               "BICY",
                                                                               "BICY_EMLeditor_metadata.XML")), write = FALSE)), "list")
})

# test length of nested attribute table, should be equal to the number of csv files in the data package
test_that("output table length equals number of csvs in data package", {
  expect_equal(length(get_catvar_tables(BICY_EMLed_meta, write = FALSE)), length(list.files(testthat::test_path("good",
                                                                                                                "BICY")), pattern = "csv"))
})

# test that default write input writes as many catvars text files as data tables
test_that("default write parameters writes same number of txt files as catvar tables", {
  temp_path <- withr::local_tempdir()
  get_catvar_tables(BICY_EMLed_meta, path = temp_path)
  expect_equal(length(list.files(temp_path, pattern = "catvars")), length(list.files(testthat::test_path("good",
                                                                                                         "BICY")), pattern = "csv"))
})

# test bad eml object
test_that("bad object input throws error", {
  expect_error(get_catvar_tables(bad_metadata_object, write = FALSE))
})

# test bad path
test_that("bad path input throws error", {
  expect_error(get_catvar_tables("bad_metadata_path.xml", write = FALSE))
})

