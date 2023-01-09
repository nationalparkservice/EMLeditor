good_dir <- here::here("tests", "testthat", "good")
bad_dir <- here::here("tests", "testhat", "bad")


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
#load datasets etc to to test functions with

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# ---- set_title ----
testthat::expect_equal("set_title works on valid eml", {
  title <- "Test Title"
  expect_message(mymetatest<-set_title(BICY_meta, "Test Title", force=TRUE),
                 title2<-mymetatest$dataset$title,
                 title == title2,
                 "test failed")#return nothing if test passes
                              #error description if test fails
}
)


args(EML::read_eml)
??read_eml
getwd()
