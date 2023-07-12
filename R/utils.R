#this gets rid of the "no visible binding for global variable 'x'" error in build checks:
globalVariables(c("UnitCode",
                  "Name",
                  "Alpha_3_B"))


#' inject NPS Publisher info into metadata
#'
#' @description .set_npspublisher injects static NPS-specific publisher info into eml documents. Calls the sub-function set.forOrByNPS, which adds an additionalMetadata element with for or by NPS = TRUE.
#'
#' @details checks to see if the publisher element exists, and if not injects NPS-specific info into EML such as publisher, publication location, and ROR id - the types of things that will be the same for all NPS data or non-data publications and do not require user input. This function will be embedded in all set. and write. class functions (and get. functions?).
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#'
#' @return eml_object
#'
#' @examples
#' \dontrun{
#' .set_npspublisher(eml_object)
#' }
.set_npspublisher <- function(eml_object) {
  # get existing publisher info for the data package:
  publish <- eml_object$dataset$publisher

  # create desired publisher info:
  pubset <- list(
    organizationName =
      "National Park Service",
    address = list(
      deliveryPoint = "1201 Oakridge Drive, Suite 150",
      city = "Fort Collins",
      administrativeArea = "CO",
      postalCode = "80525",
      country = "USA"
    ),
    onlineUrl = "http://www.nps.gov",
    electronicMailAddress = "irma@nps.gov",
    userId = list(directory = "https://ror.org/", userId = "https://ror.org/044zqqy65")
  )

  # if existing and desired publisher don't match, replace existing with desired.
  if (!identical(publish, pubset)) {
    eml_object$dataset$publisher <- pubset
  }

  # since the publisher is NPS, sets an additionalMetadata field for For or By NPS to TRUE.
  eml_object <- .set_for_by_nps(eml_object)

  return(eml_object)
}

#' Add/update EMLeditor version
#'
#' @description .set_version adds the current version of EMLeditor to the EML document.
#'
#' @details .set_version adds the current version of EMLeditor to the metadata, specifically in the "additionalMetadata" element
#'
#' @inheritParams .set_npspublisher
#'
#' @return eml_object
#'
#' @examples
#' \dontrun{
#' .set_version(eml_object)
#' }
.set_version <- function(eml_object) {
  # get current EMLeditor package version:
  current_vers <- as.character(utils::packageVersion("EMLeditor"))

  # set up additionalMetadata elements for EMLeditor:
  eml_ed <- list(
    metadata = list(
      emlEditor =
        list(
          app = "EMLeditor",
          release = current_vers
        )
    ),
    id = "emlEditor"
  )

  # access additionalMetadata elements:
  add_meta <- EML::eml_get(eml_object, "additionalMetadata")

  # if no additionalMetadata, add in EMLeditor and current version:
  if (sum(names(add_meta) != "@context") == 0) {
    eml_object$additionalMetadata <- eml_ed
  }

  # if there are existing additionalMetadata elements:
  if (sum(names(add_meta) != "@context") > 0) {
    my_list <- NULL
    # ditch the '@context' list from the goeCoverage:
    for (i in seq_along(names(add_meta))) {
      if (!names(add_meta)[i] == "@context" && !names(add_meta)[i] == "id") {
        my_list <- append(my_list, add_meta[i])
      }
    }
    x <- length(my_list)

    # does it include EMLeditor?
    app <- NULL
    for (i in seq_along(add_meta)) {
      if (suppressWarnings(stringr::str_detect(add_meta[i], "EMLeditor"))) {
        app <- "EMLeditor"
      }
    }

    # if no info on EMLeditor, add EMLeditor to additionalMetadata
    if (is.null(app)) {
      if (x == 1) {
        eml_object$additionalMetadata <- list(eml_ed, eml_object$additionalMetadata)
      }
      if (x > 1) {
        eml_object$additionalMetadata[[x + 1]] <- eml_ed
      }
    }
  }
  return(eml_object)
}

#' Get Park Unit Polygon
#'
#' @description .get_unit_polygon gets the polygon for a given park unit.
#'
#' @details retrieves a geoJSON string for a polygon of a park unit from NPS Rest services. Note: This is not the official boundary (erm... ok then what is it?!?).
#'
#' @param unit_code a string (typically 4 characters) that is the park unit code.
#'
#' @return a park polygon
#'
#' @examples
#' \dontrun{
#' poly <- .get_unit_polygon("BICY")
#' }
.get_unit_polygon <- function(unit_code) {
  # get geography from NPS Rest Services
  units_url <- paste0("https://irmaservices.nps.gov/v2/rest/unit/", unit_code, "/geography")
  xml <- httr::content(httr::GET(units_url))

  # Create spatial feature from polygon info returned from NPS
  park_polygon <- tryCatch(
    expr = {sf::st_as_sfc(xml[[1]]$Geography, geoJSON = TRUE)},
    error = function(e) {
      message(
        paste0(crayon::bold$red(unit_code),
               " is not a valid park unit. Please supply valid park units"))
    }
    )
  return(park_polygon)
}

#' Set "For or By" NPS
#'
#' @description .set_for_by_nps adds an element to additionalMetadata with For or By NPS set to TRUE and a second element agencyOriginated set to "NPS" with the understanding that all data products created for or by the NPS have NPS as the originating agency.
#'
#' @inheritParams .set_npspublisher
#'
#' @return eml_object
#'
#' @examples
#' \dontrun{
#' .set_for_by_nps(eml_object)
#' }
.set_for_by_nps <- function(eml_object) {
  # set up additionalMetadata elements for EMLeditor:
  for_by <- list(
    metadata = list(
      agencyOriginated = list(
        agency = "NPS",
        byOrForNPS = "TRUE"
      )
    ),
    id = "agencyOriginated"
  )

  # access additionalMetadata elements:
  add_meta <- EML::eml_get(eml_object, "additionalMetadata")
  add_meta <- within(add_meta, rm("@context"))

  # if no additionalMetadata, add in EMLeditor and current version:
  if (length(names(add_meta)) == 0) {
    eml_object$additionalMetadata <- for_by
  }

  # if there are existing additionalMetadata elements:
  if (length(names(add_meta)) > 0) {
    x <- length(add_meta)

    # does it include byOrForNPS?
    For_or_by_nps <- NULL
    for (i in seq_along(add_meta)) {
      if (suppressWarnings(stringr::str_detect(add_meta[i], "byOrForNPS"))) {
        For_or_by_nps <- "TRUE"
      }
    }

    # if no info on ForOrByNPS, add ForOrByNPS to additionalMetadata
    if (is.null(For_or_by_nps)) {
      if (x == 1) {
        eml_object$additionalMetadata <- list(for_by, eml_object$additionalMetadata)
      }
      if (x > 1) {
        eml_object$additionalMetadata[[x + 1]] <- for_by
      }
    }
  }
  return(eml_object)
}
