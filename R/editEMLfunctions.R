#' Edit data package title
#'
#' @details The `set_title()` function checks to see if there is an existing title and then asks the user if they would like to change the title. Some work is still needed on this function as `get_eml()` automatically returns all instances of a given tag. Specifying which title will be important for this function to work well.
#'
#' @param eml_object is an EML-formatted R object, either generated in R or imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#' @param data_package_title is a character string that will become the new title for the data package. It can be specified directly in the function call or it can be a previously defined object that holds a character string.
#' @param force logical. Defaults to false. If set to FALSE, a more interactive version of the function requesting user input and feedback. Setting force = TRUE facilitates scripting.
#' @param NPS Logical. Defaults to TRUE. **Most NPS users should leave this as the default**. Only under specific circumstances should it be set to FALSE: if you are **not** publishing with NPS, if you need to set the publisher location to some place other than the Fort Collins Office (e.g. you are NOT working on a data package) or your product is "for" the NPS but not "by" the NPS and you need to specify a different agency, set NPS = FALSE. When NPS=TRUE, the function will over-write existing publisher info and inject NPS as the publisher along the the Central Office in Fort Collins as the location. Additionally, it sets the "for or by NPS" field to TRUE and specifies the originating agency as NPS.
#'
#' @importFrom mockr local_mock
#' @importFrom rlang local_options
#'
#' @return an EML-formatted R object
#' @export
#'
#' @examples
#' \dontrun{
#' data_package_title <- "New Title. Must match DataStore Reference title."
#' eml_object <- set_title(eml_object, data_package_title)
#' }
set_title <- function(eml_object,
                      data_package_title,
                      force = FALSE,
                      NPS = TRUE) {
  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$title <- data_package_title
  }
  # interactive route:
  if (force == FALSE) {
    doc <-  eml_object$dataset$title
    if (is.null(doc)) {
      eml_object$dataset$title <- data_package_title
      cat("No previous title was detected. Your new title, ",
        crayon::blue$bold(data_package_title),
        " has been added.",
        sep = ""
      )
    } else {
      cat("Your EML already has an title, ",
          crayon::blue$bold(doc),
          ".\n", sep = "")
      cat("Are you sure you want to replace it?\n")
      var1 <- .get_user_input()
      # if User opts to retain DOI, retain it
      if (var1 == 1) {
        # print the existing DOI to the screen:
        eml_object$dataset$title <- data_package_title
        cat("You have replaced your title. The new title is: ",
            crayon::blue$bold(data_package_title), ".", sep = ""
        )
      }
      # if User opts to change DOI, change it:
      if (var1 == 2) {
        print("Your original title was retained.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Check & set a DOI
#'
#' @description `set_doi()` checks to see if there is a DOI in the alternateIdentifier tag. The EMLassemblyline package stores data package DOIs in this tag (although the official EML schema has the DOI in a different location). If there is no DOI in the alternateIdentifier tag, the function adds a DOI & reports the new DOI. If there is a DOI, the function reports the existing DOI, and prompts the user for input to either retain the existing DOI or overwrite it. Reports back the existing or new DOI, depending on the user input.
#'
#' As an alternative, consider using `set_datastore_doi()`, which will automatically initiate a draft reference on DataStore and inject the corresponding DOI into metadata.
#'
#' @details if `set_doi()` is used to change the DOI, it will also update the urls listed in metadata for each data file to reflect the new DOI/DataStore reference. If you didn't have links to your data files, `set_doi()` will add them - but only if you actually update the doi.
#'
#' @inheritParams set_title
#'
#' @param ds_ref is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated.You should NOT include the full URL, DOI prefix, or anything except the 7-digit DataStore Reference Code.
#'
#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' eml_object <- set_doi(eml_object, 1234567)
#' }
set_doi <- function(eml_object, ds_ref, force = FALSE, NPS = TRUE) {
  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$alternateIdentifier <- paste0(
      "doi: https://doi.org/10.57830/", ds_ref)
  }
  # interactive route:
  if (force == FALSE) {
    # Look for an existing data package DOI:
    doi <- eml_object$dataset$alternateIdentifier

    # If there is no existing DOI, add a DOI to the metadata
    if (is.null(doi)) {
      eml_object$dataset$alternateIdentifier <- paste0(
        "doi: https://doi.org/10.57830/",
        ds_ref
      )

      #get new doi:
      doi <- eml_object$dataset$alternateIdentifier
      doi <- sub(".*? ", "", doi)
      # print the new DOI to the screen:
      cat("No DOI detected.")
      cat("Your newly specified DOI is: ",
        crayon::blue$bold(doi),
        sep = ""
      )
    }

    # If there is a DOI, find the correct doi by searching for the text "doi: ".
    else {
      # If a DOI exists, ask the user what to do about it:
      cat("Your EML already has a DOI:\n")
      cat(crayon::blue$bold(doi),
        "\n\n",
        sep = ""
      )
      cat("Are you sure you want to replace your DOI?\n")
      var1 <- .get_user_input()
      # if User opts to retain DOI, retain it
      if (var1 == 2) {
        # print the existing DOI to the screen:
        doi <- sub(".*? ", "", doi)
        cat("Your DOI remains: ", crayon::blue$bold(doi), sep = "")
      }
      # if User opts to change DOI, change it:
      if (var1 == 1) {
        eml_object$dataset$alternateIdentifier <- paste0(
          "doi: https://doi.org/10.57830/", ds_ref)
        # get the new DOI:
        doi <- eml_object$dataset$alternateIdentifier
        doi <- sub(".*? ", "", doi)

        # print the new DOI to the screen:
        cat("Your newly specified DOI is: ", crayon::blue$bold(doi),
            ".\n", sep = "")
      }
    }
  }
  # update data URLs to correspond to new DOI
  # (this should probably be a separate function)
  data_table <- EML::eml_get(eml_object, "dataTable")
  data_table <- within(data_table, rm("@context"))

  data_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                     ds_ref)
  #handle case when there is only one data table:
  if("physical" %in% names(data_table)){
    eml_object$dataset$dataTable$physical$distribution$online$url <-
      data_url
  }
  # handle case when there are multiple data tables:
  else {
    for(i in seq_along(data_table)){
      eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <-
        data_url
    }
  }
  if (force == FALSE) {
    cat("Your data files url also been updated to: ",
        crayon::blue$bold(data_url), ".\n", sep = "")
  }

  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Add Park Unit Connections to metadata
#'
#' @description `set_content_units()` adds all specified park units and their N, E, S, W bounding boxes to <geographicCoverage>. This information will be used to fill in the Content Unit Links field in DataStore. Invalid park unit codes will return an error and the function will terminate. If you don't know a park unit code, see [`get_park_code()`](https://nationalparkservice.github.io/NPSutils/reference/get_park_code.html) from the [NPSutils](https://nationalparkservice.github.io/NPSutils/index.html) package].
#'
#' @details Adds the Content Unit Link(s) to a geographicCoverage. Content Unit Links(s) are the (typically) four-letter codes describing the park unit(s) where data were collected (e.g. ROMO, not ROMN). Each park unit is given a separate geographicCoverage element. For each content unit link, the unit name will be listed under geographicDescription and prefaced with "NPS Content Unit Link:". The geographicCoverage element will be given the attribute "system = content unit link". Required child elements (bounding coordinates) are auto populated to produce a rectangle that encompasses the park unit in question. If the default force=FALSE option is retained, the user will be shown existing content unit links (if any exist) and asked to 1) retain them 2) add to them or 3) replace them. If the force is set to TRUE, the interactive components will be skipped and the existing content unit links will be replaced.
#'
#' @inheritParams set_title
#'
#' @param park_units a list of comma-separated strings where each string is a park unit code.
#'
#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' park_units <- c("ROMO", "YELL")
#' set_content_units(eml_object, park_units)
#' }
set_content_units <- function(eml_object, park_units,
                              force = FALSE,
                              NPS = TRUE){
  # test whether park units are actually park units:
  null_units <- NULL
  for(i in seq_along(park_units)){
    is_unit <- .get_unit_polygon(park_units[i]) #
    null_units <- append(null_units, is_unit)
  }
  if (is.null(null_units)) {
    return()
  }

  # add text to indicate that these are park unit connections.
  units <- paste0("NPS Content Unit Link: ", park_units)
  #generate new geographic coverage for NPS Content Unit Links:
  unit_list <- NULL
  for (i in seq_along(park_units)){
    poly <- .get_unit_polygon(park_units[i])
    poly <- as.data.frame(poly[[1]][1])
    N <- max(poly[, 2])
    S <- min(poly[, 2])
    W <- min(poly[, 1])
    E <- max(poly[, 1])
    geocov <- EML::eml$geographicCoverage(
      geographicDescription =
        paste0("NPS Content Unit Link: ", park_units[i]),
      boundingCoordinates = EML::eml$boundingCoordinates(
        northBoundingCoordinate = N,
        eastBoundingCoordinate = E,
        southBoundingCoordinate = S,
        westBoundingCoordinate = W),
      system = "content unit link")
    unit_list <- append(unit_list, list(geocov))
  }
  # get geographic coverage from eml_object
  doc <- eml_object$dataset$coverage$geographicCoverage

  # Are there content unit links already specified?
  exist_units <- NULL
  for (i in seq_along(doc)) {
    doc2 <- unlist(doc)
    if (suppressWarnings(
      stringr::str_detect(doc2[i],
                          "NPS Content Unit Link")) == TRUE) {
      exist_units <- append(exist_units, doc2[[i]])
    }
  }

  # if there is no content unit links add it directly to eml_object
  if (is.null(exist_units)) {
    if (is.null(doc)) {
      eml_object$dataset$coverage$geographicCoverage <- unit_list
    } else {
      #if there are multiple existing geographic coverages:
      if (length(seq_along(doc[[1]])) > 1) {
        # combine new and old geo coverages (new always at the top!)
        doc <- append(unit_list, doc)
        # write over the existing geographic coverage
        eml_object$dataset$coverage$geographicCoverage <- doc
      }
      # if there is only one geo coverage:
      if (length(seq_along(doc[[1]])) == 1) {
        geocov2 <- EML::eml$geographicCoverage(
        geographicDescription =
          doc$geographicDescription,
        boundingCoordinates =
          doc$boundingCoordinates
        )
        # add park unit connections and existing geo coverage (park units always on top!)
        unit_list<-append(unit_list, list(geocov2))
      #insert into EML:
        eml_object$dataset$coverage$geographicCoverage <- unit_list
        #eml_object$dataset$coverage$geographicCoverage <- list(unit_list, (geocov2))
      }
    }
    if (force == FALSE) {
      cat("No previous Content Unit Links Detected\n")
      cat("Your Content Unit Links have been set to:\n")
      for(i in seq_along(park_units)){
        cat(crayon::blue$bold(park_units[i]), "\n")
        sep = ""
      }
    }
  }
  # if there already content unit links:
  if (!is.null(exist_units)) {
    if (force == FALSE) {
      cat("Your metadata already has the following Content Unit Links Specified:\n")
      for (i in seq_along(exist_units)) {
        cat(crayon::blue$bold(exist_units[i]), "\n")
      }
      cat("Do you want to:\n\n 1: Retain the existing Unit Connections\n 2: Add to the exsiting Unit Connections\n 3: Replace the existing Unit Connections")
      var1 <- .get_user_input3()

      # Do nothing:
      if (var1 == 1) {
        cat("Your existing Unit Connections were retained.")
      }
      # Add to existing content unit links:
      if (var1 == 2) {
        #if there are multiple pre-existing geographic coverages:
        if (length(seq_along(doc[[1]])) > 1) {
          # combine new and old geo coverages (new always at the top!)
          doc <- append(unit_list, doc)
          # write over the existing geographic coverage
          eml_object$dataset$coverage$geographicCoverage <- doc
          }
        # if there is only one geo coverage:
        if (length(seq_along(doc[[1]])) == 1) {
          geocov2 <- EML::eml$geographicCoverage(
            geographicDescription =
              doc$geographicDescription,
            boundingCoordinates =
              doc$boundingCoordinates
          )
          # add park unit connections and existing geo coverage (park units always on top!)
          unit_list<-append(unit_list, list(geocov2))
          #insert into EML:
          eml_object$dataset$coverage$geographicCoverage <- unit_list
        }
        # Report on newly set content units; first get the new content units:
        newgeo <- eml_object$dataset$coverage$geographicCoverage
        exist_units <- NULL
        for (i in seq_along(newgeo)) {
          if (suppressWarnings(stringr::str_detect(
            newgeo[i],
            "NPS Content Unit Link"
          )) == TRUE) {
            exist_units <- append(exist_units,
                                  newgeo[[i]]$geographicDescription)
          }
        }
        # print current/new units:
        cat("Your metadata now has the following Content Unit Links Specified:\n")
        for (i in seq_along(exist_units)) {
          cat(crayon::blue$bold(exist_units[i]), "\n")
        }
      }
      # replace existing content unit links:
      if (var1 == 3) {
        #if there is only one item in geoCov, it is not nested as deeply as when there are multiple. Renest single items so that all geoCov are at the same level of nesting:
        if(!is.null(names(doc))){
          doc <- list(doc)
        }
        #get all geographic coverage that is NOT content unit links:
        no_units <- NULL

        for (i in seq_along(doc)) {
          if (suppressWarnings(
            stringr::str_detect(doc[[i]][1],
                                "NPS Content Unit Link")) == FALSE) {
            no_units <- append(no_units, list(doc[[i]]))
          }
        }
        # if the only geo unit was a previous connection, replace it:
        if (is.null(no_units)) {
          eml_object$dataset$coverage$geographicCoverage <- unit_list
        }
        #if there are geographic units other than content units, add to those:
        if (!is.null(no_units)) {
          #if there is only one non-content unit geographic coverage element:
          unit_list <- append(unit_list, no_units)
          #insert into EML:

          eml_object$dataset$coverage$geographicCoverage <- unit_list
        }
          # get new geo units:
          newgeo <- eml_object$dataset$coverage$geographicCoverage
          exist_units <- NULL
          for (i in seq_along(newgeo)) {
            if (suppressWarnings(stringr::str_detect(
              newgeo[i],
              "NPS Content Unit Link"
            )) == TRUE) {
              exist_units <- append(
                exist_units,
                newgeo[[i]]$geographicDescription
              )
            }
          }
          # return current/new units:
          cat("Your metadata now has the following Content Unit Links Specified:\n")
          for (i in seq_along(exist_units)) {
            cat(crayon::blue$bold(exist_units[i]), "\n")
          }
        }
    }
  }
  # scripting route
  if (force == TRUE) {
      #if there is only one item in geoCov, it is not nested as deeply as when
      #there are multiple. Re-nest single items so that all geoCov are at the
      #same level of nesting:
      if(!is.null(names(doc))){
        doc <- list(doc)
      }
      #get all geographic coverage that is NOT content unit links:
      no_units <- NULL
      for (i in seq_along(doc)) {
        if (suppressWarnings(
          stringr::str_detect(doc[[i]][1],
                              "NPS Content Unit Link")) == FALSE) {
          no_units <- append(no_units, list(doc[[i]]))
        }
      }
      # if the only geo unit was a previous connection, replace it:
      if (is.null(no_units)) {
        eml_object$dataset$coverage$geographicCoverage <- unit_list
      }
      #if there are geographic units other than content units, add to those:
      if (!is.null(no_units)) {
        #if there is only one non-content unit geographic coverage element:
        unit_list <- append(unit_list, no_units)
        #insert into EML:
        eml_object$dataset$coverage$geographicCoverage <- unit_list
      }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Adds CUI dissemination codes to metadata
#'
#' @description
#'  `set_cui_code()` adds Controlled Unclassified Information (CUI) dissemination codes to EML metadata. These codes determine who can or cannot have access to the data. Unless you have a specific mandate to restrict data, all data should be available to the public. if the CUI dissemination code is PUBLIC, the CUI marking should also be PUBLIC (`see set_cui_marking()`) and the license should be set to CC0 or public domain (see `set_int_rights()`). If your data contains CUI and you need to set the CUI dissemination code to anything other than PUBLIC, please be prepared to provide a legal justification in the form of the appropriate CUI marking (see `set_cui_marking()`).
#'
#' @details `set_cui_code()` adds a CUI dissemination code to the tag CUI under additionalMetadata/metadata. The available choices for CUI dissemination codes at NPS are (pay attention to the spaces!):
#'
#' PUBLIC: The data contain no CUI, dissemination is not restricted.
#' FED ONLY: Contains CUI. Only federal employees should have access (similar to the "internal only" setting in DataStore)
#' FEDCON: Contains CUI Only federal employees and federal contractors should have access to the data (again, very similar to the DataStore "internal only" setting)
#' DL ONLY: Contains CUI. Should only be available to a named list of individuals. (where and how to supply that list TBD)
#' NOCON - Contains CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.
#'
#' For a more detailed explanation of the CUI dissemination codes, please see the national archives [CUI Registry: Limited Dissemination Controls](https://www.archives.gov/cui/registry/limited-dissemination) web page.
#'
#' @inheritParams set_title
#' @param cui_code a string consisting of one of 7 potential CUI codes: PUBLIC, FED ONLY, FEDCON, DL ONLY, or NOCON

#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' set_cui_dissem(eml_object, "PUBLIC")
#' }
set_cui_code <- function(eml_object,
                           cui_code = c("PUBLIC",
                                        "NOCON",
                                        "DL ONLY",
                                        "FEDCON",
                                        "FED ONLY"),
                           force = FALSE,
                           NPS = TRUE) {

  cui_code <- toupper(cui_code)
  # verify CUI code entry; stop if does not equal one of six valid codes listed above:
  cui_code <- match.arg(cui_code)

  # Generate new CUI element for additionalMetadata
  my_cui <- list(metadata = list(CUI = cui_code), id = "CUI")

  # get existing additionalMetadata elements:
  doc <- eml_object$additionalMetadata

  #if no additional metadata at all....
  if(is.null(doc)){
    eml_object$additionalMetadata <- list(my_cui)
  }
  if(!is.null(doc)){

    #helps track lists of different lengths/hierarchies
    x <- length(doc)

    # Is CUI code already specified?
    exist_cui <- NULL
    for (i in seq_along(doc)) {
      y <- suppressWarnings(stringr::str_replace_all(doc[i], " ", ""))
      if (suppressWarnings(stringr::str_detect(y, "CUI\\b")) == TRUE) {
        seq <- i
        exist_cui <- doc[[i]]$metadata$CUI
      }
    }

    # scripting route:
    if (force == TRUE) {
      #what is [[seq]]? It works but...
      eml_object$additionalMetadata[[seq]] <- my_cui
    }

    # interactive route:
    if (force == FALSE) {
      # If no existing CUI, add it in:
      if (is.null(exist_cui)) {
        if (x == 1) {
          eml_object$additionalMetadata <- list(my_cui,
                                                eml_object$additionalMetadata)
        }
        if (x > 1) {
          eml_object$additionalMetadata[[x + 1]] <- my_cui
        }
        cat("No previous CUI was detected. Your CUI has been set to ",
            crayon::bold$blue(cui_code), ".", sep = "")
      }
      # If existing CUI, stop.
      if (!is.null(exist_cui)) {
        cat("CUI has previously been specified as ",
            crayon::bold$blue(exist_cui),
            ".\n", sep = "")
        cat("Are you sure you want to reset it?")
        var1 <- .get_user_input() #1 = yes, 2 = no
        if (var1 == 1) {
          eml_object$additionalMetadata[[seq]] <- my_cui
          cat("Your CUI code has been rest to ",
              crayon::blue$bold(cui_code), ".", sep = "")
        }
        if (var1 == 2) {
          cat("Your original CUI code was retained")
        }
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/updated EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}


#' Adds CUI to metadata
#'
#' @description
#' #' `r lifecycle::badge("deprecated")`
#'  set_cui adds CUI dissemination codes to EML metadata
#'
#' @details set_cui adds a CUI code to the tag CUI under additionalMetadata/metadata.
#'
#' @inheritParams set_title
#' @param cui_code a string consisting of one of 5 potential CUI codes. Pay attention to the spaces:
#' FED ONLY - Contains CUI. Only federal employees should have access (similar to "internal only" in DataStore)
#' FEDCON - Contains CUI. Only federal employees and federal contractors should have access (also very much like current "internal only" setting in DataStore)
#' DL ONLY - Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)
#' NOCON - Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.
#' PUBLIC - Does NOT contain CUI.

#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' set_cui(eml_object, "PUBFUL")
#' }
set_cui <- function(eml_object, cui_code = c("PUBLIC", "NOCON", "DL ONLY",
                                             "FEDCON", "FED ONLY"),
                    force = FALSE, NPS = TRUE) {
  #add in deprecation
  lifecycle::deprecate_soft(when = "0.1.5", "set_cui()", "set_cui_code()")

  cui_code <- toupper(cui_code)
  # verify CUI code entry; stop if does not equal one of six valid codes listed above:
  cui_code <- match.arg(cui_code)

  # Generate new CUI element for additionalMetadata
  my_cui <- list(metadata = list(CUI = cui_code), id = "CUI")

  # get existing additionalMetadata elements:
  doc <- eml_object$additionalMetadata

  #if no additional metadata at all....
  if(is.null(doc)){
    eml_object$additionalMetadata <- list(my_cui)
  }
  if(!is.null(doc)){

    #helps track lists of different lengths/hierarchies
    x <- length(doc)

    # Is CUI already specified?
    exist_cui <- NULL
    for (i in seq_along(doc)) {
      if (suppressWarnings(stringr::str_detect(doc[i], "CUI")) == TRUE) {
        seq <- i
        exist_cui <- doc[[i]]$metadata$CUI
      }
    }

    # scripting route:
    if (force == TRUE) {
      #what is [[seq]]?
      eml_object$additionalMetadata[[seq]] <- my_cui
    }

    # interactive route:
    if (force == FALSE) {
      # If no existing CUI, add it in:
      if (is.null(exist_cui)) {
        if (x == 1) {
          eml_object$additionalMetadata <- list(my_cui,
                                                eml_object$additionalMetadata)
        }
        if (x > 1) {
          eml_object$additionalMetadata[[x + 1]] <- my_cui
        }
        cat("No previous CUI code was detected. Your CUI code has been set to ",
            crayon::bold$blue(cui_code), ".", sep = "")
      }
      # If existing CUI, stop.
      if (!is.null(exist_cui)) {
        cat("CUI code has previously been specified as ",
            crayon::bold$blue(exist_cui),
            ".\n", sep = "")
        var1 <- .get_user_input() #1 = yes, 2 = no
        if (var1 == 1) {
          eml_object$additionalMetadata[[seq]] <- my_cui
          cat("Your CUI code has been rest to ",
              crayon::blue$bold(cui_code), ".", sep = "")
        }
        if (var1 == 2) {
          cat("Your original CUI code was retained")
        }
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
  eml_object <- .set_npspublisher(eml_object)
  }

  # add/updated EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' The function sets the CUI marking for the data package
#'
#' @description `r lifecycle::badge("experimental")`
#' The Controlled Unclassified Information (CUI) marking is different from the CUI dissemination code. The CUI dissemination code (set `set_cui_code()`) sets who can have access to the data package. The CUI marking set by `set_cui_marking()` specifies the reason (if any) that the data are being restricted.
#' If the CUI dissemination code is set to PUBLIC, the CUI marking must also be PUBLIC.
#' If the CUI dissemination code is set to anything other than PUBLIC, the CUI marking must be set to SP-NPSR, SP-HISTP or SP-ARCHR.
#'
#' @details CUI markings are the legal justification for why data are being restricted from the public. If data contain no CUI, the CUI marking must be set to PUBLIC (and the CUI dissemination code must be set to PUBLIC and the license must be set to CC0 or Public Domain). If the data contain CUI (i.e. the CUI dissemination code is not PUBLIC), you must use the CUI marking to provide a legal justification for why the data are restricted. Only one CUI marking can be applied. At NPS, the following markings are available:
#'
#' PUBLIC: The data contain no CUI, dissemination is not restricted.
#' SP-NPSR: "National Park System Resources" - This material contains information concerning the nature and specific location of a National Park System resource that is endangered, threatened, rare, or commercially valuable, of mineral or paleontological objects within System units, or of objects of cultural patrimony within System units.
#' SP-HISTP: "Historic Properties" - This material contains information related to the location, character, or ownership of historic property.
#' SP-ARCHR: "Archaeological Resources" - This material contains information related to information about the nature and location of any archaeological resource for which the excavation or removal requires a permit or other permission.
#'
#' For more information on CUI markings, please visit the [CUI Markings](https://www.archives.gov/cui/registry/category-marking-list) list maintained by the National Archives.
#'
#' @inheritParams set_title
#' @param cui_marking String. One of four options, "PUBLIC", "SP-NPSR", "SP-HISTP" or "SP-ARCHR" are available.
#'
#' @return an EML-formatted R object
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_cui_marking(eml_object, "PUBLIC")
#' }
set_cui_marking <- function (eml_object,
                          cui_marking = c("PUBLIC",
                                       "SP-NPSR",
                                       "SP-HISTP",
                                       "SP-ARCHR"),
                          force = FALSE,
                          NPS = TRUE) {

  cui_marking <- toupper(cui_marking)
  # verify CUI code entry; stop if does not equal one of six valid codes listed above:
  cui_marking <- match.arg(cui_marking)

  # Generate new CUI element for additionalMetadata
  my_cui <- list(metadata = list(CUImarking = cui_marking), id = "CUImarking")

  # get existing additionalMetadata elements:
  add_meta <- eml_object$additionalMetadata

  #get the location of CUI dissemination codes in additionalMetadata:
  x <- NULL
  for (i in 1:length(seq_along(add_meta))) {
    if (names(add_meta[[i]][["metadata"]]) == "CUI") {
      x <- i
      break
    }
  }

  #if no CUI dissemination code exit the function; warn if force == FALSE
  if (is.null(x)) {
    if (force == FALSE) {
      cat("Your metadata does not contain a CUI dissemination code.")
      cat("Use ",
          crayon::bold$green("set_cui_code()"),
          " to add a dissemination code to the metadata.",
          sep = "")
    }
    return(invisible(eml_object))
  }

  #get location of CUI marking codes in additionalMetadata:
  y <- NULL
  for (i in 1:length(seq_along(add_meta))) {
    if(names(add_meta[[i]][["metadata"]]) == "CUImarking") {
      y <- i
      break
    }
  }

  #if CUI marking already exists:
  if (!is.null(y)) {
    #get existing CUI marking:
    existing_cui_marking <- add_meta[[y]][["metadata"]][["CUImarking"]]

    #don't replace an existing CUI marking with the same marking
    if (existing_cui_marking == cui_marking) {
      if (force == FALSE) {
        cat("Your metadata already have an existing CUI marking of ",
            crayon::bold$blue(existing_cui_marking),
            ".\n",
            sep = "")
        cat("Your metadata CUI marking was not updated.\n")
      }
      return(invisible(eml_object))
    }

  #if CUI markings already exist, ask if they should be replaced/changed?
    if (force == FALSE) {
      cat("Your metadata already contains the CUI marking: ",
          crayon::blue$bold(existing_cui_marking),
          ".\n",
          sep = "")
      cat("Are you sure you want to change it?\n")
      var1 <- .get_user_input()
      if (var1 == 2) {
        cat("Your original CUI marking has been retained")
        return(invisible(eml_object))
      }
    }
  }
  #extract CUI dissemination code
  cui <- add_meta[[x]][["metadata"]][["CUI"]]

  #test that cui code and cui marking are both public:
  if (cui == "PUBLIC" & cui_marking != "PUBLIC") {
    if (force == FALSE){
      msg <- paste0("to choose a CUI marking that coincides",
                    " with your CUI dissemination code or use ")
      cat("Your CUI dissemination code is set to ", cui, ".\n", sep ="")
      cat("The CUI dissemination code and CUI marking must coincide.\n")
      cat("Use ",
          crayon::green$bold("set_cui_marking() "),
          msg,
          crayon::green$bold("set_cui_code()"),
          " to change your CUI dissemination code.\n", sep = "")
    }
    return(invisible(eml_object))
  }

  #test that if cui_code is not public, cui_marking is not public.
  if (cui != "PUBLIC" & cui_marking == "PUBLIC") {
    if (force == FALSE){
      msg <- paste0("to choose a CUI marking that coincides",
                    " with your CUI dissemination code or use ")
      cat("Your CUI dissemination code is set to ", cui, ".\n", sep = "")
      cat("The CUI dissemination code and CUI marking must coincide.\n")
      cat("Use ",
          crayon::green$bold("set_cui_marking() "),
          msg,
          crayon::green$bold("set_cui_code()"),
          " to change your CUI dissemination code\n.", sep = "")
    }
    return(invisible(eml_object))
  }

  # at this point cui_code and cui_marking coincide
  # add cui_marking and put it back in additional metadata

  # Generate new CUI element for additionalMetadata
  my_cui <- list(metadata = list(CUImarking = cui_marking), id = "CUI marking")

  # if there was no CUImarking, add one:
  if (is.null(y)) {
    x <- length(eml_object$additionalMetadata)
    eml_object$additionalMetadata[[x + 1]] <- my_cui
  } else {
    #otherwise, overwrite the existing CUI marking:
    eml_object[["additionalMetadata"]][[y]] <- my_cui
  }

  if (force == FALSE) {
    cat("Your CUI marking has been set to ", crayon::blue$bold(cui_marking))
  }

  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/updated EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)

}

#' adds DRR connection
#'
#' @description set_drr adds the DOI of an associated DRR
#'
#' @details adds uses the DataStore Reference ID for an associate DRR to the <usageCitation> as a properly formatted DOI (prefaced with "DRR: ") to the <usageCitation> element. Creates and populates required children elements for usageCitation including the DRR title, creator organization name, and report number. Note the organization name (org_name) defaults to NPS.  If you do NOT want the organization name for the DRR to be NPS, set org_name="Your Favorite Organization" *and* set NPS=FALSE. Also sets the id flag for this usageCitation to "associatedDRR".
#'
#' @inheritParams set_title
#' @param drr_ref_id a 7-digit string that is the DataStore Reference ID for the DRR associated with the data package.
#' @param drr_title the title of the DRR as it appears in the DataStore Reference.
#' @param org_name String. Defaults to NPS. If the organization publishing the DRR is *not* NPS, set org_name to your publishing organization's name.
#'
#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' drr_title <- "Data Release Report for Data Package 1234"
#' set_drr(eml_object, "2293234", drr_title)
#' }
set_drr <- function(eml_object,
                    drr_ref_id,
                    drr_title,
                    org_name = "NPS",
                    force = FALSE,
                    NPS = TRUE) {
  doi <- paste0("DRR: https://doi.org/10.36967/", drr_ref_id)

  cite <- EML::eml$usageCitation(
    alternateIdentifier = doi,
    title = drr_title,
    creator = EML::eml$creator(
      organizationName = org_name
    ),
    report = EML::eml$report(reportNumber = drr_ref_id),
    id = "associatedDRR"
  )

  if (force == TRUE) {
    eml_object$dataset$usageCitation <- cite
  }

  if (force == FALSE) {
    doc <- eml_object$dataset$usageCitation

    if (is.null(doc) == TRUE) {
      cat("No previous DRR was detected")
      eml_object$dataset$usageCitation <- cite
      cat("Your DRR, ", crayon::blue$bold(drr_title),
          " has been added.", sep = "")
    } else {
      cat("Your current DRR is: ", crayon::blue$bold(doc$title),
          ".\n", sep = "")
      cat("The current DOI is: ", crayon::blue$bold(doc$alternateIdentifier),
        ".\n",
        sep = ""
      )
      cat("Are you sure you want to change it?\n")
      var1 <- .get_user_input() #1 = Yes; 2 = No
      if (var1 == 1) {
        eml_object$dataset$usageCitation <- cite
        cat("Your new DRR is: ", crayon::blue$bold(doc$title), ".\n", sep = "")
        cat("Your new DOI is: ", crayon::blue$bold(doc$alternateIdentifier),
          ".\n",
          sep = ""
        )
      }
      if (var1 == 2) {
        cat("Your original DRR was retained")
      }
    }
  }

  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' adds an abstract
#'
#' @description `set_abstract()` adds (or replaces) a simple abstract.
#'
#' @details Checks for an abstract. If no abstract is found, it inserts the abstract given in @param abstract. If an existing abstract is found, the user is asked whether they want to replace it or not and the appropriate action is taken. Currently set_abstract does not allow for complex formatting such as bullets, tabs, or multiple spaces. You can add line breaks with "\\n" and a new paragraph (blank line between text) with "\\n\\n". You are strongly encouraged to open your abstract in a text editor such as notepad and make sure there are no stray characters. If you need multiple paragraphs, you will need to do that via EMLassemblyline (for now).
#'
#' @inheritParams set_title
#' @param abstract is a text string that is your abstract. You can generate this directly in R or import a .txt file.
#'
#' @returns an EML-formatted R object
#' @export
#' @examples
#' \dontrun{
#' eml_object <- set_abstract(eml_object, "This is a very short abstract")
#' }
set_abstract <- function(eml_object,
                         abstract,
                         force = FALSE,
                         NPS = TRUE) {
  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$abstract <- abstract
  }

  # interactive route:
  if (force == FALSE) {
    # get existing abstract, if any:
    doc <- eml_object$dataset$abstract

    if (is.null(doc)) {
      eml_object$dataset$abstract <- abstract
      cat("No previous abstract was detected.\n")
      cat("Your new abstract has been added.\n")
      cat("View the current abstract using get_abstract.")
    } else {
      cat("Your EML already has an abstract.\n")
      cat("Are you sure you want to replace it?\n\n")
      var1 <- .get_user_input() #1 = yes, 2 = no
      # if User opts to replace abstract:
      if (var1 == 1) {
        # print the existing DOI to the screen:
        eml_object$dataset$abstract <- abstract
        cat("You have replaced your abstract.\n")
        cat("View the current abstract using get_abstract.")
      }
      # if User opts not to replace abstract:
      if (var1 == 2) {
        cat("Your original abstract was retained.\n")
        cat("View the current abstract using get_abstract.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Set Notes for DataStore landing page
#'
#' @description `set_additional_info()` will add information to the additionalInfo element in EML.
#'
#' @details The contents of the additionalInformation element are used to populate the 'notes' field on the DataStore landing page. Users may want to edit the notes if errors or non-ASCII text characters are discovered because the notes are prominently displayed on DataStore. To avoid non-standard characters, users are highly encouraged to generate Notes using a text editor such as Notepad rather than a word processor such as MS Word.
#'
#' At this time, `set_additional_info()` does not support complex formatting such as, bullets, tabs, or multiple spaces. You can add line breaks with "\\n" and a new paragraph (a blank line between text) with "\\n\\n".
#'
#' @inheritParams set_title
#' @param additional_info String. Will become the "notes" on the DataStore landing page.
#'
#' @return an EML-formated R object
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_additional_info(eml_object,
#'  "Some text for the Notes section on DataStore.")
#' }
set_additional_info <- function(eml_object,
                         additional_info,
                         force = FALSE,
                         NPS = TRUE) {
  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$additionalInfo <- additional_info
  }

  # interactive route:
  if (force == FALSE) {
    # get existing abstract, if any:
    doc <- eml_object$dataset$additionalInfo

    if (is.null(doc)) {
      eml_object$dataset$additionalInfo <- additional_info
      cat("No previous additionalInfo was detected.\n")
      cat("Your new additionalInfo has been added.\n")
      cat("View the current additionalInfo using get_additional_info.")
    } else {
      cat("Your EML already has additionalInfo.\n")
      cat("Are you sure you want to replace it?\n\n")
      var1 <- .get_user_input() #1 = yes, 2 = no
      # if User opts to replace abstract:
      if (var1 == 1) {
        # print the existing DOI to the screen:
        eml_object$dataset$additionalInfo <- additional_info
        cat("You have replaced your additionalInfo.\n")
        cat("View the current additionalInfo using get_additional_info.")
      }
      # if User opts not to replace abstract:
      if (var1 == 2) {
        cat("Your original additionalInfo was retained.\n")
        cat("View the current additionalInfo using get_additional_info.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}


#' Sets the Methods in metadata
#'
#' @description `set_methods()` will check for and add/replace methods in the metadata
#'
#' @details Users may want to edit the methods if errors or non-ASCII text characters are discovered because the methods are prominently displayed on DataStore. To avoid non-standard characters, users are highly encouraged to generate methods using a text editor such as Notepad rather than a word processor such as MS Word.
#'
#' At this time, `set_methods()` does not support complex formatting such as, bullets, tabs, or multiple spaces. All text will be included in a description element (which is itself a child element of a single methodStep element within the methods element). Additional child elments of methods or methodStep such as subStep, software, instrumentation, citation, sampling, etc are not supported at this time. All of this information may be added as text. You can add line breaks with "\\n" and a new paragraph (a blank line between text) with "\\n\\n".
#'
#' @inheritParams set_title
#' @param method String. A string of text describing the study methods.
#'
#' @return An EML-formatted object
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_methods(eml_object, "Here are some methods we performed.")
#' }
set_methods <- function(eml_object,
                                method,
                                force = FALSE,
                                NPS = TRUE) {
  method_list <- list(methodStep =
                        list(description = list(method)))

    # scripting route:
  if (force == TRUE) {
    eml_object$dataset$methods <- method_list
  }

  # interactive route:
  if (force == FALSE) {
    # get existing abstract, if any:
    doc <- eml_object$dataset$methods

    if (is.null(doc)) {
      eml_object$dataset$methods <- method_list
      cat("No previous methods were detected.\n")
      cat("Your new methods section has been added.\n")
      cat("View the current methods using get_methods.")
    } else {
      cat("Your EML already has a Methods section.\n")
      cat("Are you sure you want to replace it?\n\n")
      var1 <- .get_user_input() #1 = yes, 2 = no
      # if User opts to replace abstract:
      if (var1 == 1) {
        # print the existing DOI to the screen:
        eml_object$dataset$methods <- method_list
        cat("You have replaced your Methods.\n")
        cat("View the current Methods using get_methods.")
      }
      # if User opts not to replace abstract:
      if (var1 == 2) {
        cat("Your original methods section was retained.\n")
        cat("View the current methods using get_methods.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Edit literature cited
#'
#' @description `r lifecycle::badge("experimental")`
#' set_lit takes a bibtex file (*.bib) as input and adds it as a bibtex list to EML under citations
#'
#' @details looks for literature cited in the <literatureCited> tag and if it finds none, inserts citations for each entry in the *.bib file. If literature cited exists it asks to either do nothing, replace the existing literature cited with the supplied .bib file, or append additional references from the supplied .bib file. if force=TRUE, the existing literature cited will be replaced with the contents of the .bib file.
#'
#' @inheritParams set_title
#' @param bibtex_file is a text file with one or more bib-formatted references with the extension .bib. Make sure the .bib file is in your working directory, or supply the path to the file.
#' @return an EML object
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- litcited2 <- set_lit(eml_object, "bibfile.bib")
#' }
set_lit <- function(eml_object, bibtex_file, force = FALSE, NPS = TRUE) {

  bibtex_citation <- readr::read_file(bibtex_file)

  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$literatureCited$bibtex <- bibtex_citation
  }
  # interactive route:
  if (force == FALSE) {
    lit <- arcticdatautils::eml_get_simple(eml_object, "literatureCited")
    if (is.null(lit)) {
      eml_object$dataset$literatureCited$bibtex <- bibtex_citation
    } else {
      cat("You have already specified literature cited.\n")
      cat("To view yourcurrent literature, use get_lit\n")
      var1 <- readline(prompt = "Would you like to:\n\n 1: Make no changes\n 2: Replace your literature cited\n 3: add to your literature cited\n\n")
      var1 <- .get_user_input3() #
      if (var1 == 1) {
        print("No changes were made to literature cited.")
      }
      if (var1 == 2) {
        eml_object$dataset$literatureCited$bibtex <- bibtex_citation
        cat("Your literature cited section has been replaced.\n")
        cat("To view your new literature cited use get_lit.")
      }
      if (var1 == 3) {
        bib2 <- paste0(lit, "\n", bibtex_citation, sep = "")
        eml_object$dataset$literatureCited$bibtex <- bib2
        cat("You have added to your literature cited section.\n")
        cat("To view your new literature cited use get.lit")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Sets Producing Units for use in DataStore
#'
#' @description set_producing_units inserts the unit code for the producing unit for the data/metadata into the EML metdata file.
#'
#' @details inserts the unit code into the metadataProvider element. Currently cannot add to existing metadataProvider fields; it will just over-write them. It also currently only handles a single producing unit. See @param NPS for details on sub-functions. Additionally, information about the version of EML editor used will be injected into the metadata.
#'
#' @inheritParams set_title
#' @param prod_units A string that is the producing unit Unit Code or a list of unit codes, for example "ROMO" or c("ROMN", "SODN")
#'
#' @return an EML object
#' @export
#'
#' @examples
#' \dontrun{
#' prod_units <- c("ABCD", "EFGH")
#' set_producing_units(eml_object, prod_units)
#' set_producing_units(eml_object, c("ABCD", "EFGH"))
#' set_producing_units(eml_object, "ABCD", force = TRUE)
#' }
set_producing_units <- function(eml_object,
                                prod_units,
                                force = FALSE,
                                NPS = TRUE) {
  # get existing metadataProvider info, if any:
  doc <- eml_object$dataset$metadataProvider

  # make metadataProvider fields with producing units filled in:
  if (length(prod_units == 1)) {
    plist <- EML::eml$metadataProvider(organizationName = prod_units)
  }
  if (length(prod_units > 1)) {
    plist <- NULL
    for (i in seq_along(prod_units)) {
      punit <- EML::eml$metadataProvider(organizationName = prod_units[i])
      plist <- append(plist, list(punit))
    }
  }

  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$metadataProvider <- plist
  }
  # interactive route:
  if (force == FALSE) {
    # if no existing metadataprovider info:
    if (is.null(doc)) {
      eml_object$dataset$metadataProvider <- plist
      cat("No previous Producing Units were detected.\n")
      cat("Your new Producing Unit(s) have been added:\n")
      cat(crayon::bold$blue(prod_units), "\n")
    }
    # if there *is* existing metadataProvider info, choose whether to overwrite or not:
    if (!is.null(doc)) {
      cat("Your metadata already contains the following Producing Unit(s):\n")
      cat(crayon::blue$bold(get_producing_units(eml_object)), "\n")
      cat("Are you sure you want to replace them?\n\n")
      var1 <- .get_user_input() #1 = yes, 2 = no
      # if User opts to replace metadataProvider, replace it:
      if (var1 == 1) {
        eml_object$dataset$metadataProvider <- plist
        cat("You have replaced your Producing Unit(s).\n")
        cat("Your new producing Unit(s) are:\n")
        cat(crayon::bold$blue(prod_units), "\n")
      }
      # if User opts to retain metadataProvider, retain it:
      if (var1 == 2) {
        cat("Your original Producing Units were retained.\n")
        cat("View the current Producing Unit(s) using get_producing_units.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Set the human language used for metadata
#'
#' @description set_language allows the user to specify the language that the metadata (and data) were constructed in. This field is intended to hold the human language, i.e. English, Spanish, Cherokee.
#'
#' @details The English words for the language the data and metadata were constructed in (e.g. "English") is automatically converted to the the 3-letter codes for languages listed in ISO 639-2 (available at https://www.loc.gov/standards/iso639-2/php/code_list.php) and inserted into the metadata.
#'
#' @inheritParams set_title
#'
#' @param lang is a string consisting of the language the data and metadata were constructed in, for example, "English", "Spanish", "Navajo". Capitalization does not matter, but spelling does! The input provided here will be converted to 3-digit ISO 639-2 codes.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' set_language(eml_object, "english")
#' set_language(eml_object, "Spanish")
#' set_language(eml_object, "nAvAjO")
#' }
set_language <- function(eml_object, lang, force = FALSE, NPS = TRUE) {
  # enforces ISO capitalization formatting:
  lang <- stringr::str_to_title(lang)

  # a few common one-off name translations. Probably could improve the dplyr filter to filter for anything containing, but then you run into issues. There are 5 different languages whose English language name includes the word "English" ("English", "English, Old", "Creole and pigeons, English based", etc)).

  if (lang == "Spanish") {
    lang <- "Spanish; Castilian"
  }
  if (lang == "Iroquois") {
    lang <= "Iroquoian languages"
  }

  # get ISO language codes
  langcodes <- ISOcodes::ISO_639_2

  # get language code in the ISO language codes?
  nlang <- dplyr::filter(langcodes, Name == lang)[[1]]

  # if the language supplied is not part of ISO 639-2 (e.g. spelling error):
  if (nchar(nlang) != 3 || identical(nlang, character(0)) == TRUE) {
    stop(message("Please check that your language is included in the ISO 639-2B language code. The codes are available at https://www.loc.gov/standards/iso639-2/php/code_list.php"))
  }

  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$language <- nlang
  }

  # interactive route:
  if (force == FALSE) {
    # get current language from the metadata provided:
    lng <- eml_object$dataset$language

    # if there is no language specified in the metadata:
    if (is.null(lng)) {
      eml_object$dataset$language <- nlang
      cat(
        "The language has been set to ", crayon::bold$blue(nlang),
        ", the ISO 639-2B code for ", crayon::bold$blue(lang), ".", sep="")
    }

    # if the language is already specified in the metadata:
    else {
      if (nchar(lng) == 3) {
        full_lang <- dplyr::filter(langcodes, Alpha_3_B == lng)[[4]]
        cat("The current language is set to ", crayon::blue$bold(lng),
          ", the ISO 639-2B code for ", full_lang, ".",
          sep = ""
        )
      } else {
        cat("The current language is set to ",
          crayon::blue$bold(lng), ".",
          sep = ""
        )
      }

      # does the user want to change the language?
      cat("Are you sure you want to replace it?\n\n")
      var1 <- .get_user_input() #1 = yes, 2 = no

      # if yes, change the language and report the change:
      if (var1 == 1) {
        eml_object$dataset$language <- nlang
        cat("You have replaced the language with ",
          crayon::blue$bold(nlang),
          ", the 3-letter ISO-639-2B code for ",
          crayon::blue$bold(lang), ".",
          sep = ""
        )
      }

      # if User opts to retain metadataProvider, retain it:
      if (var1 == 2) {
        cat("Your original language was retained.")
      }
    }
  }

  # Set NPS publisher, if it doesn't already exist. Also sets byorForNPS in additionalMetadata to TRUE.
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Adds a connection to the protocol under which the data were collected
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'  This function is currently under development. If you use it, it will break your EML.  You've been warned.
#'
#'  set_protocol adds a metadata link to the protocol under which the data being described were collected. It automatically inserts a link to the DataStore landing page for the protocol as well as the protocol title and creator.
#'
#' @details Because protocols can be published to DataStore using a number of different reference types, set_protocol does not check to make sure you are actually supplying a the reference ID for a protocol (or the correct protocol).
#'
#' @inheritParams set_title
#' @param protocol_id a string. The 7-digit number identifying the DataStore reference number for the Project that describes your inventory or monitoring project.
#' @param dev Logical. Defaults to FALSE, meaning all API calls will be to the production version of DataStore. To test the function, set dev = TRUE to use the development environment for DataStore.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' set_protocol(eml_object, 2222140)
#' }
#'
set_protocol <- function(eml_object,
                         protocol_id,
                         dev = FALSE,
                         force = FALSE,
                         NPS = TRUE) {

  # test for properly formatted protocolID
  if (nchar(protocol_id) != 7) {
    cat("You must supply a valid 7-digit protocol_id")
    stop()
  }

  if (dev == TRUE) {
    get_url <- paste0(.ds_dev_api(),
                      "Profile?q=",
                      protocol_id)
  } else {
    get_url <- paste0(.ds_api(),
                      "Profile?q=",
                      protocol_id)
  }

  req <- httr::GET(get_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type'='application/json'))

  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get project information:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  #does reference exist/can user access it - errors are indistinguishable
  if (length(seq_along(rjson)) == 0) {
    permissions_msg <- paste0("The reference does not exist",
                              " or you do not have permissions to access it.")
    cat(permissions_msg)
    stop()
  }

  # extract protocol title
  protocol_title <- rjson$bibliography$title

  # get creator
  protocol_creator <- rjson$history$createdBy
  protocol_creator <- stringr::str_remove(protocol_creator, "@nps.gov")

  #generate URL (check whether protocol has DOI)
  if (dev == TRUE) {
    get_url <- paste0(.ds_dev_api(),
                      "ReferenceCodeSearch?q=",
                      protocol_id)
  } else {
    get_url <- paste0(.ds_api(),
                      "ReferenceCodeSearch?q=",
                      protocol_id)
  }

  req2 <- httr::GET(get_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'))

  status_code <- httr::stop_for_status(req2$status_code)
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get project information:
  json2 <- httr::content(req2, "text")
  rjson2 <- jsonlite::fromJSON(json2)

  project_url <- NULL
  if (rjson2$isDOI == "True") {
    protocol_url <- paste0("https://doi.org/10.57830/", protocol_id)
  } else {
    protocol_url <- rjson2$referenceUrl
  }

  #build protocol element:
  proto <- list(
    title = protocol_title,
    creator = list(
      individualName = list(
        surName = protocol_creator)),
    distribution = list(
      online = list(
        url = protocol_url)))

  # scripting route:
  if (force == TRUE) {
    eml_object$protocol <- proto
  }
  # interactive route:
  if (force == FALSE) {
    # get existing project (if any)
    exist_proto <- eml_object$dataset$protocol

    # if no previous project listed, add project
    if (is.null(exist_proto)) {
      eml_object$protocol <- proto
      cat("The current protocol is now ", crayon::bold$blue(proto$title),
        ".",
        sep = ""
      )
    }

    # if an there is an existing protocol, ask whether to replace:
    else {
      cat("you already have a protocol(s) with the Title:\n",
        crayon::bold$blue(exist_proto$title), ".",
        sep = ""
      )
      cat("Are you sure you want to replace it?\n\n")
      var1 <- .get_user_input() #1=yes, 2=no
      # if yes, change the project:
      if (var1 == 1) {
        eml_object$protocol <- list(proto)
        cat("The current protocol is now ",
          crayon::bold$blue(proto$title), ".",
          sep = ""
        )
      }

      # if no, retain the existing project:
      if (var1 == 2) {
        cat("Your original protocol was retained.")
      }
    }
  }
  # Set NPS publisher, if it doesn't already exist. Also sets byorForNPS in additionalMetadata to TRUE.
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Adds a reference to a DataStore Project housing the data package
#'
#' @description
#' The function will add a single project title and URL to the metadata corresponding to the DataStore Project reference that the data package should be linked to. Upon EML extraction on DataStore, the data package will automatically be added/linked to the DataStore project indicated.EML2.2.0 only supports a single project so `set_project()`will overwrite/replace an existing project element.
#'
#' @details This function will overwrite existing projects. To add a DataStore project to your metadata, the project must be publicly available. The person uploading and extracting the EML must be an owner on both the data package and project references in order to have the correct permissions for DataStore to create the desired link. If you have set NPS = TRUE and force = FALSE (the default settings), the function will also test whether you have owner-level permissions for the project which is necessary for DataStore to automatically connect your data package with the project.
#'
#' DataStore only add links between data packages and projects. DataStore cannot not remove data packages from projects. If need to remove a link between a data package and a project (perhaps you supplied the incorrect project reference ID at first), you will need to manually remove the connection using the DataStore web interface.
#'
#' @inheritParams set_title
#' @param project_reference_id String. The 7-digit number corresponding to the Project reference ID that the data package should be linked to.
#' @param dev Logical. Defaults to FALSE, meaning the function will validate user input based on the DataStore production server. You can set dev = TRUE to test the function on the development server.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_project(eml_object,
#'                          1234567)
#' }
set_project <- function(eml_object,
                        project_reference_id,
                        dev = FALSE,
                        force = FALSE,
                        NPS = TRUE) {

  if (nchar(project_reference_id) != 7) {
    cli::cli_abort(c("x" = "You must supply a 7-digit project_reference_id"))
    return(invisible())
  }

  if (dev == TRUE) {
    get_url <- paste0(.ds_dev_api(),
                       "Profile?q=",
                       project_reference_id)
  } else {
    get_url <- paste0(.ds_api(),
                       "Profile?q=",
                       project_reference_id)
  }

  req <- httr::GET(get_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'))

  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    cli::cli_abort(c("x" = "ERROR: DataStore connection failed.",
                     " " = "Are you connected to the internet?"))
    return(invisible())
  }

  #get project information:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)

  # if it doesn't exist or permissions are invalid:
  if (length(seq_along(rjson)) == 0) {
    cli::cli_abort(c("x" = "ERROR: Could not find the Project
                     {.var {project_reference_id}} on DataStore.",
                     "i" = "If {.var {project_reference_id}} is set to
                     restricted, you must set it to Public to add it to
                     metadata."))
    return(invisible())
  }

  # make sure the project_reference_id is a project:
  if (rjson$referenceType != "Project") {
    cli::cli_abort(c("x" = "The reference {.var {project_reference_id}}
                     is not a DataStore Project.",
                     " " = "Please supply a valid DataStore project
                     reference code."))
    return(invisible())
  }

  #test whether user has ownership permissions for the project.
  #only run this test if NPS is true and force is false:
  if (NPS == TRUE && force == FALSE) {
    email <- QCkit:::get_user_email()
    ownership <- rjson$permissions$referenceOwners

    if (sum(grepl(email, ownership)) < 1) {
      msg <- paste0("WARNING: {.email {email}} is not listed as an owner for ",
                    "the project{.var {project_reference_id}}.")
      info1 <- paste0("The person extracting the metadata on DataStore must ",
                      "have ownership-level permissions to succesfully link ",
                      "the data package to it's project.")
      info2 <- "Project owners can add new owners via the DataStore GUI."
      cli::cli_alert_warning(msg)
      cli::cli_alert_info(info1)
      cli::cli_alert_info(info2)
    }
  }

  #project title
  project_title <- rjson$bibliography$title

  if (sum(is.na(rjson$bibliography$publisher)) > 0) {
    project_org <- "No publisher name supplied"
  } else {
    project_org <- rjson$bibliography$publisher$publisherName
  }
  project_role <- "a DataStore Project"

  #generate URL (check whether project has DOI)
  if (dev == TRUE) {
    get_url <- paste0(.ds_dev_api(),
                      "ReferenceCodeSearch?q=",
                      project_reference_id)
  } else {
    get_url <- paste0(.ds_api(),
                      "ReferenceCodeSearch?q=",
                      project_reference_id)
  }

  req2 <- httr::GET(get_url,
                   httr::authenticate(":", "", "ntlm"),
                   httr::add_headers('Content-Type'='application/json'))

  status_code <- httr::stop_for_status(req2$status_code)
  if (!status_code == 200) {
    cli::cli_abort("ERROR: DataStore connection failed.
                   Are you logged in to the VPN?\n")
    return(invisible())
  }

  #get project information:
  json2 <- httr::content(req2, "text")
  rjson2 <- jsonlite::fromJSON(json2)

  project_url <- NULL
  if (rjson2$isDOI == "True") {
    project_url <- paste0("https://doi.org/10.57830/", project_reference_id)
  } else {
    project_url <- rjson2$referenceUrl
  }

  #create DataStore project:
  proj <- list(
    title = project_title,
    personnel = list(
      organizationName = project_org,
      onlineUrl = project_url,
      role = project_role
    ), id = "DataStore_project"
  )

  #whenever EML 3.0.0 comes out (or mutliple projects are allowd) this can
  #be uncommented to allow set_project to add to existing projects
  #rather than replace them:

  #get existing projects:
  #existing_projects <- eml_object$dataset$project

  #if (is.null(existing_projects)) {
    eml_object$dataset$project <- proj
  #} else {
    #if there are multiple projects:
  #  if (length(seq_along(existing_projects[[1]])) > 1) {
      # combine new and old projects (with new DataStore project at the top)
  #    proj <- append(list(proj), existing_projects)
      # overwrite the existing projects in EML with new project list:
  #    eml_object$dataset$project <- proj
  #  }
    #if there is only one existing project:
  #  if (length(seq_along(existing_projects[[1]])) == 1) {
  #    proj <- append(list(proj), list(existing_projects))
  #    eml_object$dataset$project <- proj
  #  }
  #}

  # Set NPS publisher, if it doesn't already exist. Also sets byorForNPS in additionalMetadata to TRUE.
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  if (force == FALSE) {
    msg1 <- paste0("The DataStore project {.var {project_reference_id}} with ",
                   "the title {.var {project_title}} has been added to your ",
                    "metadata.")
    msg2 <- paste0("Your data package will be automatically linked to this ",
                    "project when once it is uploaded to DataStore and the ",
                    "metadata are extracted.")
    cli::cli_inform(c("i" = msg1))
    cli::cli_inform(c("i" = msg2))
  }
  return(eml_object)
}


#' Set Publisher
#'
#' @description set_publisher should only be used if the publisher Is **NOT the National Park Service** or if the contact address for the publisher is NOT the central office in Fort Collins. All data packages are published by the Fort Collins office, regardless of where they are collected or uploaded from. If you are working on metadata for a data package, *Do not use this function* unless you are very sure you need to (most NPS users will not want to use this function). If you want the publisher to be anything other than NPS out of the Fort Collins Office, if you want the originating agency to be something other than NPS, _or_ your product is *not* for or by the NPS, use this function. It's probably a good idea to run args(set_publisher) to make sure you have all the arguments, especially those with defaults, properly specified.
#'
#' @inheritParams set_title
#' @param org_name String. The organization name that is publishing the digital product. Defaults to "NPS".
#' @param street_address String. The street address where the digital product is published
#' @param city String. The city where the digital product is published
#' @param state String. A two-letter code for the state where the digital product is being published.
#' @param zip_code String. The postal code for the publishers location.
#' @param country String. The country where the digital product is being published.
#' @param URL String. a URL for the publisher.
#' @param email String. an email for the publisher.
#' @param ror_id String. The ROR id for the publisher (see https://ror.org/ for more information).
#' @param for_or_by_NPS Logical. Defaults to TRUE. If your digital product is NOT for or by the NPS, set to FALSE.
#' @param NPS Logical. Defaults to TRUE. Set this to FALSE only if the party responsible for data collection and generation is *not* the NPS *or* the publisher is *not* the NPS central office in Fort Collins.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' set_publisher(eml_object,
#'   "BroadLeaf",
#'   "123 First Street",
#'   "Second City",
#'   "CO",
#'   "12345",
#'   "USA",
#'   "https://www.organizationswebsite.com",
#'   "contact@myorganization.com",
#'   "https://ror.org/xxxxxxxxx",
#'   for_or_by_NPS = FALSE,
#'   NPS = FALSE
#' )
#' }
set_publisher <- function(eml_object,
                          org_name = "NPS",
                          street_address,
                          city,
                          state,
                          zip_code,
                          country,
                          URL,
                          email,
                          ror_id,
                          for_or_by_NPS = TRUE,
                          force = FALSE,
                          NPS = FALSE) {
  # just in case someone at NPS wants to run this function, it will run .set_npspublisher instead unless they explicitly tell it not to by setting NPS = FALSE. This is an extra safeguard.
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
    if (force == FALSE) {
      cat("The publisher has been set to the ",
        crayon::blue$bold("Fort Collins office of the National Park Service"),
        ".\n",
        sep = ""
      )
      cat("The data package has been designated as ",
        crayon::bold("Created For and by the NPS = "),
        crayon::blue$bold("TRUE"), "\n\n",
        sep = ""
      )
      cat("To specifiy an alternative, set NPS=FALSE")
    }
  }
  # get existing publisher info for the data package:
  publish <- eml_object$dataset$publisher

  # create desired publisher info:
  pubset <- list(
    organizationName = org_name,
    address = list(
      deliveryPoint = street_address,
      city = city,
      administrativeArea = state,
      postalCode = zip_code,
      country = country
    ),
    onlineUrl = URL,
    electronicMailAddress = email,
    userId = list(
      directory = "https://ror.org/",
      userId = ror_id
    )
  )
  # set up byOrForNPS:
  for_by <- list(metadata = list(
    agencyOriginated = list(
      agency = org_name,
      byOrForNPS = for_or_by_NPS
    ),
    id = "agencyOriginated"
  ))
  # access additionalMetadata elements:
  add_meta <- eml_object$additionalMetadata

  # which table in add_meta has agencyOriginated?
  if (!is.null(add_meta)) {
    agency <- NULL
    add_meta_location <- NULL
    for (i in seq_along(add_meta)) {
      if (suppressWarnings(stringr::str_detect(
        add_meta[i],
        "agencyOriginated"
      ))) {
        agency <- add_meta[i]
        add_meta_location <- i
      }
    }
  }
  if (NPS == FALSE) {
    if (force == TRUE) {
      # change the publisher
      if (!identical(publish, pubset)) {
        eml_object$dataset$publisher <- pubset
      }



      # if no additionalMetadata, add in EMLeditor and current version:
      if (length(add_meta) == 0) {
        eml_object$additionalMetadata <- for_by
      }

      # if there are existing additionalMetadata elements:
      if (length(add_meta) > 0) {
        # if no info on ForOrByNPS, add ForOrByNPS to additionalMetadata
        if (is.null(agency)) {
          if (length(add_meta) == 1) {
            eml_object$additionalMetadata <- list(
              for_by,
              eml_object$additionalMetadata
            )
          }
          if (length(add_meta) > 1) {
            eml_object$additionalMetadata[[length(add_meta) + 1]] <- for_by
          }
        }
        # if there *IS* already ForOrByNPS, update it:
        if (!is.null(agency)) {
          # if that's all there is, replace all additionalMetadata:
          if (length(add_meta) == 1) {
            eml_object$additionalMetadata <- for_by
            # if there are multiple additionalMetadata items, replace just the agencyOriginated:
          }
          if (length(add_meta) > 1) {
            # replace just agencyOriginated

            eml_object$additionalMetadata[add_meta_location] <- list(for_by)
          }
        }
      }
    }
    if (force == FALSE) {
      if (is.null(publish)) {
        eml_object$dataset$publisher <- pubset
        cat("No publisher information was detected\n\n")
        cat("Your publisher has been set to:\n")
        cat("Organization Name: ", crayon::blue$bold(pubset$organizationName),
            "\n", sep = "")
        cat("Street address: ", crayon::blue$bold(pubset$address$deliveryPoint),
            "\n", sep = "")
        cat("City: ", crayon::blue$bold(pubset$address$city), "\n", sep = "")
        cat("State: ", crayon::blue$bold(pubset$address$administrativeArea),
            "\n", sep = "")
        cat("Zip Code: ", crayon::blue$bold(pubset$address$postalCode),
            "\n", sep = "")
        cat("Country: ", crayon::blue$bold(pubset$address$country),
            "\n", sep = "")
        cat("URL: ", crayon::blue$bold(pubset$onlineUrl), "\n", sep = "")
        cat("email: ", crayon::blue$bold(pubset$email), "\n", sep = "")
        cat("ROR ID: ", crayon::blue$bold(pubset$userID), "\n", sep = "")
      }
      if (!is.null(publish)) {
        if (identical(publish, pubset)) {
          cat("Your current publisher is identical to the information you entered.\n")
          cat("No changes were made to the publisher.\n\n")
        }
        if (!identical(publish, pubset)) {
          cat("Your current publisher is set to:\n\n")
          cat("Organization Name: ",
            crayon::blue$bold(publish$organizationName), "\n",
            sep = ""
          )
          cat("Street address: ",
            crayon::blue$bold(publish$address$deliveryPoint), "\n",
            sep = ""
          )
          cat("City: ", crayon::blue$bold(publish$address$city), "\n", sep = "")
          cat("State: ", crayon::blue$bold(publish$address$administrativeArea),
            "\n",
            sep = ""
          )
          cat("Zip Code: ", crayon::blue$bold(publish$address$postalCode), "\n",
            sep = ""
          )
          cat("Country: ", crayon::blue$bold(publish$address$country), "\n",
            sep = ""
          )
          cat("URL: ", crayon::blue$bold(publish$onlineUrl), "\n", sep = "")
          cat("email: ", crayon::blue$bold(publish$electronicMailAddress),
            "\n",
            sep = ""
          )
          cat("ROR ID: ", crayon::blue$bold(publish$userId$userId),
            "\n\n",
            sep = ""
          )
          cat("Would you like to replace your existing publisher?\n\n")
          var1 <- .get_user_input() #1: Yes; 2: No\n")
          if (var1 == 1) {
            eml_object$dataset$publisher <- pubset
            cat("Your new publisher is:\n\n")
            cat("Organization Name: ",
              crayon::blue$bold(pubset$organizationName), "\n",
              sep = ""
            )
            cat("Street address: ",
              crayon::blue$bold(pubset$address$deliveryPoint), "\n",
              sep = ""
            )
            cat("City: ",
              crayon::blue$bold(pubset$address$city), "\n",
              sep = ""
            )
            cat("State: ",
              crayon::blue$bold(pubset$address$administrativeArea), "\n",
              sep = ""
            )
            cat("Zip Code: ",
              crayon::blue$bold(pubset$address$postalCode), "\n",
              sep = ""
            )
            cat("Country: ",
              crayon::blue$bold(pubset$address$country), "\n",
              sep = ""
            )
            cat("URL: ", crayon::blue$bold(pubset$onlineUrl), "\n", sep = "")
            cat("email: ", crayon::blue$bold(pubset$electronicMailAddress),
              "\n",
              sep = ""
            )
            cat("ROR ID: ", crayon::blue$bold(pubset$userId$userId), "\n\n",
              sep = ""
            )
          }
          if (var1 == 2) {
            cat("Your existing publisher information was retained.\n\n")
          }
        }
      }

      # if there is no additionalMetadata at all:
      if (is.null(add_meta)) {
        # add agencyOriginated to Metadata:
        eml_object$additionalMetadata <- for_by
        # report results to user:
        cat("No agency was detected. Your agency has been updated to:\n")
        cat("Agency: ", crayon::bold$blue(org_name), "\n", sep = "")
        cat("Created By or For NPS: ", crayon::bold$blue(for_or_by_NPS),
          "\n",
          sep = ""
        )
      }

      # if there is additionalMetadata
      if (!is.null(add_meta)) {
        # but no agencyOriginated:
        if (is.null(add_meta_location)) {
          if (length(add_meta) == 1) {
            eml_object$additionalMetadata <- list(
              for_by,
              eml_object$additionalMetadata
            )
          }
          if (length(add_meta) > 1) {
            eml_object$additionalMetadata[[length(add_meta) + 1]] <- for_by
          }
          cat("No agency was detected. Your agency has been updated to:\n")
          cat("Agency: ", crayon::bold$blue(org_name), "\n", sep = "")
          cat("Created By or For NPS: ", crayon::bold$blue(for_or_by_NPS),
            "\n",
            sep = ""
          )
        }

        # if there is metadata, including agencyOrginated:
        if (!is.null(add_meta_location)) {
          cat("Your metadata already contains information about whether it was Created By or For the NPS.\n")
          cat("Agency: ", crayon::bold$blue(agency[[1]]$metadata$agencyOriginated$agency), "\n", sep = "")
          cat("Created By or For NPS: ",
            crayon::blue$bold(agency[[1]]$metadata$agencyOriginated$byOrForNPS),
            "\n\n",
            sep = ""
          )
          cat("Would you like to replace your agency?\n\n")
          var2 <- .get_user_input() #1 = yes, 2 = no
          if (var2 == 1) {
            # Since there are existing additionalMetadata elements:
            if (length(add_meta) == 1) {
              eml_object$additionalMetadata <- for_by
              # if there are multiple additionalMetadata items, replace just the agencyOriginated:
            }
            if (length(add_meta) > 1) {
              # replace just agencyOriginated
              eml_object$additionalMetadata[add_meta_location] <- list(for_by)
            }
            cat("Your new agency information has been set to:\n\n")
            cat("Agency: ", crayon::bold$blue(org_name), "\n", sep = "")
            cat("Created By or For NPS: ", crayon::bold$blue(for_or_by_NPS),
              "\n",
              sep = ""
            )
          }
          # if user does not want to make changes:
          if (var2 == 2) {
            cat("Your original agency was retained.\n")
          }
        }
      }
    }
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Set Intellectual Rights (and License)
#'
#' @description set_int_rights allows the intellectualRights and licenseName fields in EML to be surgically replaced.
#'
#' @details set_int_rights requires that CUI information be listed in additionalMetadata prior to being called. The verbose `force = FALSE` option will warn the user if there is no CUI specified. `set_int_rights` checks to make sure the CUI code specified (see `set_cui_code()`) is appropriate for the license type chosen. For most public NPS datasets, the CC0 license is appropriate.

#' @inheritParams set_title
#'
#' @param license String. Indicates the type of license to be used. The three potential options are "CC0", "public" and "restricted". CC0 and public can only be used if CUI is set to PUBLIC. Restricted can only be used if CUI is set to any code that is NOT set to PUBLIC (see `set_cui_code()` for a list of codes). To view the exact text that will be inserted for each license, please see https://nationalparkservice.github.io/NPS_EML_Script/stepbystep.html#intellectual-rights
#'
#' @importFrom stats complete.cases
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' set_int_rights(eml_object, "CC0", force=TRUE, NPS=FALSE)
#' set_int_rights(eml_object, "restricted")}
#'
set_int_rights <- function(eml_object,
                          license = c("CC0", "public", "restricted"),
                          force = FALSE,
                          NPS = TRUE){
  # verify license type selection; stop if does not equal one of 3 valid codes:
  license <- tolower(license)
  license <- match.arg(license)

  #set up license text:
  CCzero <- 'This product is released to the "public domain" under Creative Commons CC0 1.0 No Rights Reserved (see: https://creativecommons.org/publicdomain/zero/1.0/).'
  pub_domain <- 'This product is released to the "public domain" under U.S. Government Works No Rights Reserved (see: http://www.usa.gov/publicdomain/label/1.0/).'
  restrict <- "This product has been determined to contain Controlled Unclassified Information (CUI) or to be otherwise restricted by the National Park Service, and is intended for specific purposes. It is not published under an open license. Unauthorized access, use, and distribution are prohibited."

  # get CUI info from additionalMetadata:
  cui <- eml_object$additionalMetadata
  cui2 <- NULL #record CUI dissemination code
  for(i in seq_along(cui)){
    if("CUI" %in% names(cui[[i]][["metadata"]])){
      cui2 <- cui[[i]][["metadata"]][["CUI"]]
    }
  }
  # Verbose option with feedback:
  if(force == FALSE){
    # enforce CUI info prior to setting license:
    # if there is no CUI specified, stop.
    if(is.null(cui2)){
      return(paste0(cat("No CUI information found.\n"),
                    cat("You must set CUI prior to setting the license.\nUse",
                        crayon::bold$green("set_cui()"),
                        "to specify CUI.")))
    }
    # if CUI was specified:
    if(!is.null(cui2)){
      # make sure CUI and license agree:
      if(license == "CC0" || license == "public"){
        #set appropriate license:
        if(cui2 == "PUBLIC"){
          if(license == "CC0"){
            eml_object$dataset$intellectualRights <- CCzero
            eml_object$dataset$licensed$licenseName <- "Creative Commons Zero v1.0 Universal"
            cat("Your license has been set to:", crayon::blue$bold("CC0"))
          }
          if(license == "public"){
            eml_object$dataset$intellectualRights <- pub_domain
            eml_object$dataset$licensed$licenseName <- "Public Domain"
            cat("Your license has been set to:",
                crayon::blue$bold("Public Domain"))
          }
        }
        # warn user license not set, CUI and license don't agree:
        if(cui2 != "PUBLIC"){
          cat("Your CUI is set to ", crayon::blue$bold(cui2), ".")
          writeLines(paste0("To use a CC0 or public domain license",
                       " your CUI must be PUBLIC."))
          cat("Use", crayon::bold$green("set_cui()"), "to change your CUI.")
        }
      }
      if(license == "restricted"){
        if(cui2 == "PUBLIC"){
          cat("Your CUI is set to ", crayon::blue$bold(cui2), ".\n", sep="")
          writeLines(paste0("To use a restricted license, ",
                            "your CUI must NOT be set to PUBLIC."))
          cat("Use", crayon::bold$green("set_cui()"), "to change your CUI.")
        }
        if(cui2 != "PUBLIC"){
          eml_object$dataset$intellectualRights <- restrict
          eml_object$dataset$licensed$licenseName <-
            "Unlicensed (not for public dissemination)"
          cat("Your license has been set to ",
              crayon::bold$blue("Restricted"), ".", sep="")
        }
      }
    }
  }

  #silent option for scripting:
  if(force == TRUE){
    #if no CUI specified, stop.
    if(is.null(cui2)){
      return()
    }

    #if CUI has been specified:
    if(!is.null(cui2)){
      # retrieve just the cell containing the CUI code:
      # get CUI info from additionalMetadata:
      cui <- eml_object$additionalMetadata
      cui2 <- NULL #record CUI dissemination code
      for(i in seq_along(cui)){
        if("CUI" %in% names(cui[[i]][["metadata"]])){
          cui2 <- cui[[i]][["metadata"]][["CUI"]]
        }
      }

      if(license == "CC0" || license == "public"){
        if(cui2 == "PUBLIC"){
          if(license == "CC0"){
            eml_object$dataset$intellectualRights <- CCzero
          }
          if(license == "public"){
            eml_object$dataset$intellectualRights <- pub_domain
          }
        }
        if(cui2 != "PUBLIC"){
         return()
        }
      }
      if(license == "restricted"){
        if(cui2 == "PUBLIC"){
          return()
        }
        if(cui2 != "PUBLIC"){
          eml_object$dataset$intellectualRights <- restrict
        }
      }
    }
  }

  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}


#' Update (or add) data table URLs
#'
#' @description `set_data_urls()` inspects metadata and edits the online distribution url for each dataTable (data file) to correspond to the reference indicated by the DOI listed in the metadata. If your data files are stored on DataStore as part of the same reference as the data package, you do not need to supply a URL. If your data files will be stored to a different repository, you can supply that location.
#'
#' @details `set_data_urls()` sets the online distribution URL for all dataTables (data files in a data package) to the same URL. If you do not supply a URL, your metadata must include a DOI (use `set_doi()` or `set_datastore_doi()` to add a DOI - these will automatically update your data table urls to match the new DOI). `set_data_urls()` assumes that DOIs refer to digital objects on DataStore and that the last 7 digits of the DOI correspond to the DataStore Reference ID.
#'
#' @inheritParams set_title
#' @param url a string that identifies the online location of the data file (uniform resource locator)
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' # For data packages on DataStore, no url is necessary:
#' my_metadata <- set_data_urls(my_metadata)
#'
#' # If data files are NOT on (or going to be on) DataStore, you must supply their location:
#' my_metadata <- set_data_urls(my_metadata, "https://my_custom_repository.com/data_files")}
set_data_urls <- function(eml_object, url = NULL, force = FALSE, NPS = TRUE){
  #get data tables:
  data_table <- EML::eml_get(eml_object, "dataTable")
  data_table <- within(data_table, rm("@context"))

  #default: no URL supplied; assumes NPS DataStore URLs:
  if(is.null(url)){
    doi <- EMLeditor::get_doi(eml_object)
    if(is.na(doi)){
        return()
    }
    # to do: check DOI formatting to make sure it is an NPS doi
    ds_ref <- stringr::str_sub(doi, -7, -1)
    data_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                     ds_ref)

    #handle case when there is only one data table:
    if("physical" %in% names(data_table)){
      eml_object$dataset$dataTable$physical$distribution$online$url <- data_url
    }
    # handle case when there are multiple data tables:
    else {
      for(i in seq_along(data_table)){
        eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <-
          data_url
      }
    }
    if(force == FALSE){
      cat("The online URL listed for your digital files has been updated to correspond to the DOI in metadata.\n")
    }
  }
  else{
    #handle case when there is only one data table:
    if("physical" %in% names(data_table)){
      eml_object$dataset$dataTable$physical$distribution$online$url <- url
    }
    # handle case when there are multiple data tables:
    else {
      for(i in seq_along(data_table)){
        eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <-
          url
      }
    }
  }
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
  return(eml_object)
}

#' Allows user to add ORCids to the creator
#'
#' @description `set_creator_orcids()` allows users to add (or remove) ORCiDs to creators or edit/update existing ORCiDs associated with creators. ORCiDs are persistent digital identifiers associated with individual people and remain constant despite name changes. They can help disambiguate creators with similar names and associate all the products of one creator in one space despite variations in how the name was used (e.g. Rob Baker and Robert Baker and Robert L. Baker but NOT any of the 15 million or so other Robert Bakers). To register an ORCiD or manage your ORCiD profile, go to [https://orcid.org/](https://orcid.org/).
#'
#' @details ORCiDs should be supplied as a list in the order in which the creators are listed. If a creator does not have an ORCiD, put NA (NO quotes around NA!) in the list in that space. Only consider individual people who are creators (and not organizations, they will automatically be skipped). ORCiDs should be supplied as a 16-digit string with hyphens after every 4 digits: xxxx-xxxx-xxxx-xxxx. Please do not include the URL prefix for your ORCiDs; this will automatically be inserted for you.
#'
#' @inheritParams set_title
#' @param orcids String. One or more ORCiDs listed in the same order as the corresponding creators. Use "NA" if a creator does not have an ORCiD. Do not include the full URL. Format as: xxxx-xxxx-xxxx-xxxx (the https://orcid.org/ prefix will be added for you).
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' #only one creator:
#' mymetadata <- set_creator_orcids(mymetadata, 1234-1234-1234-1234)
#'
#' #three creators, the second of which does not have an ORCiD:
#' creator_orcids <- c("1234-1234-1234-1234", NA, "4321-4321-4321-4321")
#' mymetadata <- set_creator_orcids(mymetadata, creator_orcids)
#' }
set_creator_orcids <- function(eml_object, orcids, force = FALSE, NPS = TRUE) {
  #if NA supplied as "NA":
  if (sum(stringr::str_detect(na.exclude(orcids), "NA")) > 0) {
    if (force == FALSE) {
      cat("It appears some authors do not have orcids.\n")
      cat("Please specify these as NA (without quotes).")
    }
    return(invisible(eml_object))
  }

  #make sure they didn't include URLs:
  if (sum(grep("https://orcid.org/", orcids)) > 0) {
    if (force == FALSE) {
      cat("The ORCiD(s) you supplied appear to be incorrectly formatted.\n")
      cat("Please supply ORCiDs in the following format: xxxx-xxxx-xxxx-xxxx",
          " (No URLs).\n", sep="")
    }
    return(invisible(eml_object))
  }

  #get creators
  creator <- eml_object[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if (sum(names_list %in% names(creator)) > 0) {
    creator <- list(creator)
  }

  #get existing orcids (should this be its own function?):
  surName <- NULL
  existing_orcid <- NULL
  for (i in seq_along(creator)) {
    if ("individualName" %in% names(creator[[i]])) {
      #check for orcid directory id:
      surName <- append(surName,
                        creator[[i]][["individualName"]][["surName"]])
      #what if there is no orcid?
      if (is.null(creator[[i]][["userId"]][["userId"]])) {
        existing_orcid <- append(existing_orcid, NA)
      } else {
      existing_orcid <- append(existing_orcid,
                               creator[[i]][["userId"]][["userId"]])
      }
    }
  }

  # make sure provided list of orcids is the same length as the authors
  if (length(seq_along(surName)) != length(seq_along(orcids))) {
    if (force == FALSE) {
      cat("The list of orcids and existing authors must be the same length.\n")
      cat("Use NA (no quotes) to indicate an author does not have an orcid.")
    }
    return(invisible(eml_object))
  }

  #if verbose route:
  if (force == FALSE) {
    #if there are already orcids:
    if (!is.null(existing_orcid)) {
      #construct tibble of existing orcids:
      current_orcids <- tibble::tibble(surName, existing_orcid)
      #construct tibble of replacement orcids:
      new_orcids <- tibble::tibble(surName, orcids)
      for (i in 1:nrow(new_orcids)) {
        if(!is.na(new_orcids[i,2])) {
          new_orcids[i,2] <- paste0("https://orcid.org/", new_orcids[i,2])
        }
      }
      cat("Your data package contains ORCiDs for the following creators:\n\n")
      cat(format(tibble::as_tibble(current_orcids))[c(-3L, -1L)], sep = "\n")
      cat("\nAre you sure you want to replace the existing ORCiDs with the following:\n\n")
      cat(format(tibble::as_tibble(new_orcids))[c(-3L, -1L)], sep = "\n")
      var1 <- .get_user_input() #1: Yes; 2: No

      if(var1 == 2){
        cat("Your original ORCiDs were retained.")
        return(invisible(eml_object))
      }
    }
  }

  #generate list of new orcids:
  author_order <- 1
  for (i in seq_along(creator)) {
    if ("individualName" %in% names(creator[[i]])) {
      if (!is.na(orcids[author_order])) {
        replace_orcid <- paste0("https://orcid.org/", orcids[author_order])
        userId2 <- list(list(userId = replace_orcid),
                        directory = "https://orcid.org")
        creator[[i]][["userId"]] <- userId2
      } else {
        # if user specified NA for orcid, get rid of that field
        creator[[i]][["userId"]] <- NULL
      }
      author_order <- author_order +1
    }
  }

  #add/replace orcids with updated/new orcids:
  eml_object[["dataset"]][["creator"]] <- creator

  #if verbose route, display new orcids:
  if (force == FALSE) {
    new_creator <- eml_object[["dataset"]][["creator"]]
    #get existing orcids (should this be its own function?):
    surName <- NULL
    existing_orcid <- NULL
    for (i in seq_along(new_creator)) {
      if ("individualName" %in% names(new_creator[[i]])) {
        #check for orcid directory id:
        surName <- append(surName,
                          new_creator[[i]][["individualName"]][["surName"]])
        next_orcid <- new_creator[[i]][["userId"]][[1]][["userId"]]
        if (!is.null(next_orcid)) {
          existing_orcid <- append(existing_orcid,
                                 next_orcid)
        } else {
          existing_orcid <- append(existing_orcid, NA)
        }
      }
    }
    #construct tibble of existing orcids:
    new_orcids <- tibble::tibble(surName, existing_orcid)
    names(new_orcids) <- c("surName", "new_orcids")
    cat("Your new ORCiDs are:\n")
    cat(format(tibble::as_tibble(new_orcids))[c(-3L, -1L)], sep = "\n")
  }

  #add NPS publisher & for or by nps
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }

  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Add an organization as a creator to metadata (author in DataStore)
#'
#' @description `set_creator_orgs()` allows a user to add an organization as a creator to metadata. EMLassemblyline can link an individual person who is a creator to the organization they are associated with, but currently does not allow organizations themselves to be listed as creators. `set_creator_orgs()` takes a list of organizations (and their RORs, if they have them) and appends them to the end of the creator list. This allows organizations (such as a Network or a Park) to author data packages.
#'
#' @details because `set_creator_orgs()` merely appends organizations, it 1) assumes that there is already one creator (as required by EMLassemblyline) and 2) does not allow the user to change authorship order. To change the order of the authors, see `set_creator_order()`. Creator organizations and their RORs need to be supplied in two lists where the organization and ROR are correctly matched (i.e. the 3rd organization is associated with the 3rd ROR in a list). If one or more of your creator organizations does not have a ROR, enter "NA".
#'
#' You can use the park_units parameter to specify park units. This will utilize the IRMA units look-up service to fill in the organizationName element, thus ensuring that there are no spelling errors or deviations unit names. You can use either park_units or creator_orgs but not both because if you have specified any park_units, these will over-write the creator_orgs in your function call. Thus, if you would like to add park units as creators and some non-park unit organization as a creator you need to call the function twice. When specifying park_units, they will be added in the order they occur in the IRMA units list, not necessarily the order they were supplied to the function. Use `set_creator_order()` to re-order them if desired.
#'
#' @inheritParams set_title
#' @param creator_orgs List. Defaults to NA. A list of one or more organizations.
#' @param park_units List. Defaults to NA. A list of park units. If any park units are specified, it they will supersede anything listed under creator_orgs.
#' @param RORs List. Defaults to NA. An optional list of one or more ROR IDs (see [https://ror.org](https://ror.org)) that correspond to the organization in question. If an organization does not have a ROR ID (or you don't know it), enter "NA".
#'
#' @return eml_object
#' @export
#'
#' @examples
#'  \dontrun{
#'  #add one organization and it's ROR:
#'  mymetadata <- set_creator_orgs(mymetadata, "National Park Service", RORs="044zqqy65")
#'
#'  #add one organization that does not have a ROR:
#'  mymetadata <- set_creator_orgs(mymetadata, "My Favorite ROR-less Organization")
#'
#'  #add multiple organizations, some of which do not have RORs:
#'  my_orgs <- c("National Park Service", "My Favorite ROR-less Organization")
#'  my_RORs <- c("044zqqy65", "NA")
#'
#'  mymetadata <- set_creator_orgs(mymetadata, my_orgs, RORs=my_RORs)
#'
#'  #add multiple park units as organization names:
#'  park_units <- c("ROMN", "SFCN", "YELL")
#'
#'  mymetadata <- set_creator_orgs(mymetadata, park_units=park_units)
#'
#'  }
set_creator_orgs <- function(eml_object,
                             creator_orgs = NA,
                             park_units = NA,
                             RORs = NA,
                             force = FALSE,
                             NPS = TRUE){
  #stop if not enough info provided:
  if(sum(!is.na(creator_orgs) > 0) & sum(!is.na(park_units)) > 0) {
    cat("You must supply either a creator organization name or a park unit.\n")
    stop()
  }

  #get creators (what if there are none?)
  creator <- eml_object[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }

  #if park units are supplied - over-writes organization names!
  if(sum(!is.na(park_units)) > 0){
    creator_orgs <- NULL
    f <- file.path(tempdir(), "irmadownload.xml")
    if (!file.exists(f)) {
      # access all park codes from NPS xml file
      curl::curl_download("https://irmaservices.nps.gov/v2/rest/unit/", f)
      }
    result <- XML::xmlParse(file = f)
    dat <- XML::xmlToDataFrame(result) # xml to dataframe

    alpha <- dat %>% dplyr::filter(grepl(paste(park_units, collapse = '|'),
                                         UnitCode, ignore.case = TRUE))

    park_name <- alpha$FullName
    creator_orgs <- append(creator_orgs, park_name)
  }

  #generate list of creator orgs and directories
  create_orgs <- NULL
  for(i in seq_along(creator_orgs)){
      org <- EML::eml$creator(
        individualName = NULL,
        organizationName = creator_orgs[i],
        positionName = NULL,
        address = NULL,
        electronicMailAddress = NULL,
        userId = list(RORs[i], directory = "https://ror.org"))
      create_orgs <- append(create_orgs, list(org))
  }

  #append creator orgs to existing creator list:
  new_creator_list <- append(creator, create_orgs)

  #replace old creator list with new creator list
  eml_object[["dataset"]][["creator"]] <- new_creator_list

  if(force == FALSE){
    creator_new <- eml_object[["dataset"]][["creator"]]

    #if only one creator:
    if(sum(names_list %in% names(creator_new)) > 0){
      creator_new <- list(creator_new)
    }

    creator_list <- NULL
    for(i in seq_along(creator_new)){
      if(!is.null(creator_new[[i]][["individualName"]])){
        creator_list <- append(creator_list, creator_new[[i]][["individualName"]][["surName"]])
      }
      else{
        creator_list <- append(creator_list,
                               creator_new[[i]][["organizationName"]])
      }
    }
    if(force == FALSE){
      cat("Your newly assigned creators (authors on DataStore) are:\n")
      cat(creator_list, sep = "\n")
    }
  }

  #add NPS publisher & for or by nps
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadataa
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Adds creators to EML
#'
#' @description Sometimes it is necessary to change the creators (authors) of a data package. If the data package and EML have already been created, it can be tedious to have to re-run all of the EMLassemblyline functions and then have to re-do all of the EMLeditor functions. This function allows the user to add in one or more creators without having to re-run the entire workflow. This will not over-write the or remove the existing creators, just add to the list of creators.
#'
#' @details Each creator must have at minimum a last name. You may also supply a first name and one middle name. If you are adding a list of creators, you must supply all fields for each creator; if one creator is for instance missing or does not use a first name, do not skip this but instead list it as NA. If you need to re-arrange creators or remove creators, you can do so using the `set_creator_order` function.Do NOT use this function to add organizations as creators. Instead use `set_creator_orgs`. If your new creator has an orcid, add the orcid in via `set_creator_orgs`
#'
#' @inheritParams set_title
#' @param last_name String (or list of strings). The last name(s) of the creator(s) to add. You must supply a last name for each creator.
#' @param first_name String (or list of strings). The first name(s) or initials of the creator(s) to add. Use NA if there is no first name.
#' @param middle_name String (or list of strings). The middle name(s) or initial(s) of the creator(s) to add. Use NA if there is no middle name.
#' @param organization_name String (or list of strings). The organizational affiliation of the creator(s) to add, e.g. "National Park Service". Use NA if there is no organizational affiliation.
#' @param email_address String (or list of strings). The email address(es) of the creator(s) to add. Use NA if there are no email addresses.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' meta2 <- set_new_creator(metadata, "Doe", "John", "D.", "NPS", "John_Doe@@nps.gov")

#' meta2 <- set_new_creator(metadata,
#'                          last_name = c("Doe", "Smith"),
#'                          first_name = c("John", "Jane"),
#'                          middle_name = c(NA, "S."),
#'                          organization_name = c("NPS", "UCLA"),
#'                          email_address = c("john_doe@@nps.gov", NA))
#'}
set_new_creator <- function(eml_object,
                         last_name = NA,
                         first_name = NA,
                         middle_name = NA,
                         organization_name = NA,
                         email_address = NA,
                         force = FALSE,
                         NPS = TRUE) {
  #make sure all creators have at least a last name:
  if (sum(is.na(last_name)) > 0) {
    cat("At a minimum, each creator requires a last name")
    stop()
  }
  #get creators (what if there are none? does that even happen?)
  existing_creators <- eml_object[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if (sum(names_list %in% names(existing_creators)) > 0) {
    existing_creators <- list(existing_creators)
  }

  #### Add in new creator(s):
  new_creators <- NULL
  creator <- NULL
  for (i in seq_along(last_name)) {
    #generate a new creator
    creator <- list(
      individualName = list(
        surName = last_name[i],
        givenName = list(
          if (!is.na(first_name[i])){first_name[i]} else {NULL},
                         middle_name[i])),
      organizationName = organization_name[i],
      electronicMailAddress = email_address[i]#,

      # could not get adding orcids to work; fixme later.
      #if (!is.na(orcid[i])) {
      #  userId = list(directory = 'https://orcid.org',
      #                   userId = paste0("https://orcid.org/", orcid[i]))
      #}
    )
    #remove any null elements
    creator <- creator[!sapply(creator,is.null)]
    #add newest creator to list of new creators:
    new_creators <- append(new_creators, list(creator))
  }
  replace_creators <- append(existing_creators, new_creators)

  #replace old creator list with new creator list
  eml_object[["dataset"]][["creator"]] <- replace_creators

  #add NPS publisher & for or by nps
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Rearrange the order of creators (authors)
#'
#' @description The `set_creator_order()` requires that your metadata contain two or more creators. The function allows users to rearrange the order of creators (authors on DataStore) or delete a creator.
#'
#' @inheritParams set_title
#' @param new_order List. Defaults to NA for interactively re-ordering the creators.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' # fully interactive route:
#' meta2 <- set_creator_order(eml_object)
#'
#' # specify an order in advance (reverse the order of 2 creators):
#' meta2 <- set_creator_order(eml_object, c(2,1))
#'
#' # specify an order in advance (remove the second creator):
#' meta2 <- set_creator_order(eml_object, 1)
#'
#' # scripting route: turn off all function feedback (you must specify the
#' # new creator order when you call the function; you cannot do it
#' # interactively):
#' meta2 <- set_creator_order(eml_object, c(2,1), force=TRUE)
#' }
set_creator_order <- function(eml_object,
                              new_order = NA,
                              force = FALSE,
                              NPS = TRUE){
  #exit function if sufficient information not provided:
  if(sum(is.na(new_order)) > 0){
    if(force == TRUE){
      cat("If you set force=TRUE, you must supply a comma-separated list for the new_order argument, e.g. new_order = c(2,1).\n")
      return(eml_object)
    }
  }

  #get existing creators
  creator <- eml_object[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0 | length(creator) == 1){
    cat("Only one Creator was detected in metadata.\n")
    cat("Metadata must contain at least one Creator.\n")
    cli::cli_inform("Use {.fn EMLeditor::set_creator_orgs} to add additional organizations as creators.\n")
    return(eml_object)
  }

  #generate list of creators in current order:
  creator_list <- NULL
  for(i in seq_along(creator)){
    if(!is.null(creator[[i]][["individualName"]])){
      creator_list <- append(creator_list, creator[[i]][["individualName"]][["surName"]])
    }
    else{
      creator_list <- append(creator_list, creator[[i]][["organizationName"]])
    }
  }

  #if verbose and no order supplied, interactively ask for new creator order:
  if(force == FALSE){
    if(sum(is.na(new_order)) > 0){
      cat("Your current creators are in the following order:\n\n")
      creator_df<-data.frame(order=1:length(creator_list), creator_list)
      colnames(creator_df)<-c("Order", "Creator")
      print(creator_df, row.names=FALSE)
      cat("\n")
      cat("Please enter comma-separated numbers for the new creator order.\n")
      cat("Example: put 5 creators in reverse order, enter: 5, 4, 3, 2, 1\n")
      cat("Example: remove the 3rd item (out of 5) enter: 1, 2, 4, 5\n\n")
      var1 <- .get_user_input3() # waits for any user input
      #don't allow user to remove all creators!
      if(nchar(var1)==0){
        cat("You cannot delete all creators.")
        cat("Please enter comma-separated numbers for the new creator order.\n")
        var1 <-.get_user_input3()
      }
      ord <- var1
      if (nchar(ord) > 1) {
        ord <- as.list(strsplit(ord, ","))[[1]]
        ord <- trimws(ord)
      }
      new_order <- as.numeric(ord)
    }
  }

  #generate the new creator list based on the old one
  new_cr_order<-NULL
  for(num in new_order){
    curr_creator<-creator[[num]]
    new_cr_order <- append(new_cr_order, list(curr_creator))
  }

  #replace old creator list with new creator list
  eml_object[["dataset"]][["creator"]] <- new_cr_order

  #report back on new creator order:
  if(force == FALSE){
    creator_reordered <- eml_object[["dataset"]][["creator"]]

    # If there's only one creator (i.e. someone deletes all but one!), creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
    if(sum(names_list %in% names(creator_reordered)) > 0){
      creator_reordered <- list(creator_reordered)
    }
    #generate list of creators in current order:
    creator_list2 <- NULL
    for(i in seq_along(creator_reordered)){
      if(!is.null(creator_reordered[[i]][["individualName"]])){
        creator_list2 <- append(
                        creator_list2,
                        creator_reordered[[i]][["individualName"]][["surName"]])
      }
      else{
        creator_list2 <- append(creator_list2,
                                creator_reordered[[i]][["organizationName"]])
      }
    }
    cat("Your new creators order is:\n")
    creator_df2<-data.frame(order=1:length(creator_list2), creator_list2)
    colnames(creator_df2)<-c("Order", "Creator")
    print(creator_df2, row.names=FALSE)
  }

  #add NPS publisher & for or by nps
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadataa
  eml_object <- .set_version(eml_object)

  return(eml_object)
}

#' Adds a missing value code and definition to EML metadata
#'
#' @description Missing data must have a missing data code and missing data code definition. `set_missing_data()` can add a single missing value code and single missing value code definition. Missing data should be clearly indicated in the data with a missing data code (e.g "NA", "NaN", "Missing", "blank" etc.). It is generally a good idea to not use special characters for missing data codes (e.g. N/A is not advised). If it is absolutely necessary to leave a cell empty with no code, that cell still needs a missing value code and definition in the metadata. Acceptable codes in this case are "empty" and "blank" with a suitable definition that states the cells are purposefully left empty.
#'
#' @details The `set_missing_data()` be used on an individual column or can accept lists of files, column names, codes, and definitions. Make sure that each missing value has a file, column, single code, and single definition associated with it (if you need multiple missing value codes and definitions per column, please use the `set_more_missing()` function). If you have many missing value codes and definitions, you might consider constructing (or import) a dataframe to describe them:
#'
#' Example data frame:
#' df <- data.frame(files       = c("table1.csv",
#'                                  "table2.csv",
#'                                  "table3.csv",
#'                                  "table4.csv"),
#'                  columns     = c("EventDate",
#'                                  "scientificName",
#'                                  "eventID",
#'                                  "decimalLatitude"),
#'                  codes       = c("NA", "NA", "missing", "blank"),
#'                  definitions = c("not recorded",
#'                                  "not identified",
#'                                  "not recorded",
#'                                  "intentionally left blank - not recorded"))
#'
#' meta2 <- set_missing_data(eml_object = metadata,
#'                           files = df$files,
#'                           columns = df$columns,
#'                           codes = df$codes,
#'                           definitions = df$definitions)
#'
#'
#' @inheritParams set_title
#' @param files String or List of strings. These are the files that contain undocumented missing data, e.g "my_data_file1.csv".
#' @param columns String or List of strings. These are the columns with missing data for which you would like to add missing data codes and explanations in to the metadata, e.g. "scientificName".
#' @param codes String or list of strings. These are the missing value codes you would like associated with the column in question, e.g. "missing" or "NA".
#' @param definitions String or list of strings. These are the missing value code definitions associated with the missing value codes, e.g "not recorded" or "sample damaged when the lab flooded".
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' #For a single column of data in a single file:
#' meta2 <- set_missing_data(my_metadata,
#'                           "table1.csv",
#'                           "scientificName",
#'                           "NA",
#'                           "Unable to identify")
#'
#' #For multiple columns of data, potentially across multiple files:
#' #(blank cells must have the missing value code of "blank" or "empty")
#' meta2 <- set_missing_data(my_metadata,
#'                          files = c("table1.csv", "table1.csv", "table2.csv"),
#'                          columns = c("date", "time", "scientificName"),
#'                          codes = c("NA", "missing", "blank"),
#'                          definitions = c("date not recorded",
#'                                        "time not recorded",
#'                                        "intentionally left blank - missing")
#'                                        )
#'}
set_missing_data <- function(eml_object,
                             files,
                             columns,
                             codes,
                             definitions,
                             force = FALSE,
                             NPS = TRUE) {

  #test that the number of data files, columns, codes, and definitions match:
  lists <- list(files, columns, codes, definitions)
  if (sum(seq_along(unique(lapply(lists, seq_along))) != 1) > 0) {
    if (force == FALSE) {
      msg <- paste0("The number of filenames, columns, codes, ",
                    "and defintions must be the same.")
      cat(msg)
    }
    return()
  }
  #tun user input into a dataframe for easier manipulation later on:
  user_df <- data.frame(files, columns, codes, definitions)
  #get dataTable from metadata
  data_tbl <- eml_object$dataset$dataTable
  # If there's only one csv, data_tbl ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  if ("attributeList" %in% names(data_tbl)) {
    data_tbl <- list(data_tbl)
  }
  #get the attribute list for each file:
  for (i in seq_along(unique(files))) {
    for(j in seq_along(data_tbl)) {
      if (data_tbl[[j]][["physical"]][["objectName"]] == unique(files[i])) {
        attrs <- data_tbl[[j]][["attributeList"]][["attribute"]]
        #if only one attribute, renest so the code works consistently:
        if (!is.null(names(attrs))) {
          attrs <- list(attrs)
        }
        #get the new missing codes for that file:
        df <- user_df[which(user_df$files == files[i]),]
        #find the correct attribute for each column:
        for (k in seq_along(df$columns)) {
          for (l in seq_along(attrs)) {
            if (attrs[[l]][["attributeName"]] == df$columns[k]) {
              #extract that specific attribute
              attr_add_missing <- attrs[[l]]
              # if it already has a missing value code, ask before replacing:
              if (!is.null(attr_add_missing[["missingValueCode"]])) {
                if (force == FALSE) {
                  cat("File ", crayon::blue$bold(files[i]),
                      " column ", crayon::blue$bold(df$columns[k]),
                      " already has a missing value code.\n", sep ="")
                  cat("Would you like to replace it?\n")
                  var1 <- .get_user_input() #1 = yes, 2 = no
                  if (var1 == 2) { next } # Skip this column
                }
              }
              #add/replace the missing value codes:
              attr_add_missing$missingValueCode <- list(
                code = df$codes[k],
                codeExplanation = df$definitions[k])
              #put new attr_add_missing back into attrs:
              attrs[[l]] <- attr_add_missing
            }
          }
        }
        # put new attrs back into data_tbl:
        data_tbl[[j]][["attributeList"]][["attribute"]] <- attrs
      }
    }
    # put new data_tbl back into EML:
    eml_object$dataset$dataTable <- data_tbl
  }
  #add NPS publisher & for or by nps
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadataa
  eml_object <- .set_version(eml_object)

  return(eml_object)
}


