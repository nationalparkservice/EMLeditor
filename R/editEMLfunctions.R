#' Edit data package title
#'
#' @details The set_title function checks to see if there is an existing title and then asks the user if they would like to change the title. Some work is still needed on this function as get_eml() automatically returns all instances of a given tag. Specifying which title will be important for this function to work well.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#' @param data_package_title is a character string that will become the new title for the data package. It can be specified directly in the function call or it can be a previously defined object that holds a character string.
#' @param force logical. Defaults to false. If set to FALSE, a more interactive version of the function requesting user input and feedback. Setting force = TRUE facilitates scripting.
#' @param NPS Logical. Defaults to TRUE. **Most users should leave this as the default**. Only under specific circumstances should it be set to FALSE: if you are **not** publishing with NPS, if you need to set the publisher location to some place other than the Fort Collins Office (e.g. you are NOT working on a data package) or your product is "for" the NPS by not "by" the NPS and you need to specify a different agency, set NPS = FALSE. When NPS=TRUE, the function will over-write existing publisher info and inject NPS as the publisher along the the Central Office in Fort Collins as the location. Additionally, it sets the "for or by NPS" field to TRUE and specifies the originating agency as NPS.
#'
#' @return an EML-formatted R object
#' @export
#'
#' @examples
#' \dontrun{
#' data_package_title <- "New Title. Must match DataStore Reference title."
#' eml_object <- set_title(eml_object, data_package_title)
#' }
set_title <- function(eml_object, data_package_title, force = FALSE, NPS = TRUE) {
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
      cat("Your EML already has an title, ", crayon::blue$bold(doc), ".", sep = "")
      var1 <- readline(prompt = "Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
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
    eml_object$dataset$alternateIdentifier <- paste0("doi: https://doi.org/10.57830/", ds_ref)
    # update data URLs to correspond to new DOI:
    data_table <- EML::eml_get(eml_object, "dataTable")
    data_table <- within(data_table, rm("@context"))
    data_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                       ds_ref)
    #handle case when there is only one data table:
    if("physical" %in% names(data_table)){
      eml_object$dataset$dataTable$physical$distribution$online$url <- data_url
    }
    # handle case when there are multiple data tables:
    else {
      for(i in seq_along(data_table)){
        eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <- data_url
      }
    }
  }
  # interactive route:
  if (force == FALSE) {
    # Look for an existing data package DOI:
    doc <- arcticdatautils::eml_get_simple(
      eml_object,
      "alternateIdentifier"
    )

    # If there is no existing DOI, add a DOI to the metadata
    if (is.null(doc)) {
      eml_object$dataset$alternateIdentifier <- paste0(
        "doi: https://doi.org/10.57830/",
        ds_ref
      )
      doc <- arcticdatautils::eml_get_simple(
        eml_object,
        "alternateIdentifier"
      )
      doc <- sub(".*? ", "", doc)
      # print the new DOI to the screen:
      cat("No DOI detected.")
      cat("Your newly specified DOI is: ",
        crayon::blue$bold(doc),
        sep = ""
      )
    }

    # If there is a DOI, find the correct doi by searching for the text "doi: ".
    else {
      my_list <- NULL

      # hopefully deals with case when there are multiple DOIs specified under alternateIdentifier tags. Haven't run into this yet and so this remains untested.
      if (length(doc) > 1) {
        for (i in seq_along(doc)) {
          if (stringr::str_detect(doc[i], "doi:")) {
            my_list <- append(my_list, doc[i])
          }
        }
      }
      # if there is only one alternateIdentifier:
      else {
        my_list <- doc
      }
      doi <- my_list[[1]]

      # If a DOI exists, ask the user what to do about it:
      cat("Your EML already has a DOI specified in the <alternateIdentifier> tag:\n")
      cat(crayon::blue$bold(doc),
        "\n\n",
        sep = ""
      )
      var1 <- readline(prompt = cat("Enter 1 to retain this DOI\nEnter 2 to overwrite this DOI"))
      # if User opts to retain DOI, retain it
      if (var1 == 1) {
        # print the existing DOI to the screen:
        doi <- sub(".*? ", "", doi)
        cat("Your DOI remains: ", crayon::blue$bold(doi), sep = "")
      }
      # if User opts to change DOI, change it:
      if (var1 == 2) {
        eml_object$dataset$alternateIdentifier <- paste0("doi: https://doi.org/10.57830/", ds_ref)
        # get the new DOI:
        doc <- arcticdatautils::eml_get_simple(eml_object, "alternateIdentifier")
        doc <- sub(".*? ", "", doc)

        # update data URLs to correspond to new DOI:
        data_table <- EML::eml_get(eml_object, "dataTable")
        data_table <- within(data_table, rm("@context"))

        data_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                           ds_ref)
        #handle case when there is only one data table:
        if("physical" %in% names(data_table)){
          eml_object$dataset$dataTable$physical$distribution$online$url <- data_url
        }
        # handle case when there are multiple data tables:
        else {
            for(i in seq_along(data_table)){
              eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <- data_url
            }
        }
        # print the new DOI to the screen:
        cat("Your newly specified DOI is: ", crayon::blue$bold(doc),
            ".\n", sep = "")
        cat("Your data files url also been updated to: ",
            crayon::blue$bold(data_url), ".\n", sep = "")
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

#' Add Park Unit Connections to metadata
#'
#' @description `set_content_units()` adds all specified park units and their N, E, S, W bounding boxes to <geographicCoverage>. This information will be used to fill in the Content Unit Links field in DataStore. Invalid park unit codes will return an error and the function will terminate. If you don't know a park unit code, see [get_park_code()](https://nationalparkservice.github.io/NPSutils/reference/get_park_code.html) from the [NPSutils](https://nationalparkservice.github.io/NPSutils/index.html) package].
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
    is_unit <- .get_unit_polygon(park_units[i])
    null_units <- append(null_units, is_unit)
  }
  # if any park unit is not valid, and exit function.
  if(!identical(seq_along(null_units), seq_along(park_units))){
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
    W <- max(poly[, 1])
    E <- min(poly[, 1])
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
    myMap <- purrr::map(doc, 1)[[i]]
    if (suppressWarnings(stringr::str_detect(myMap,
                                             "NPS Content Unit Link")) == TRUE) {
      exist_units <- append(exist_units, myMap)
    }
  }
  # interactive route:
  if (force == FALSE) {
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
      cat("No previous Content Unit Links Detected\n")
      cat("Your Content Unit Links have been set to:\n")
      for(i in seq_along(park_units)){
        cat(crayon::blue$bold(park_units[i]), "\n")
        sep = ""
      }
    }
    # if there already content unit links:
    if (!is.null(exist_units)) {
      cat("Your metadata already has the following Content Unit Links Specified:\n")
      for (i in seq_along(exist_units)) {
        cat(crayon::blue$bold(exist_units[i]), "\n")
      }
      var1 <- readline(prompt = "Do you want to\n\n 1: Retain the existing Unit Connections\n 2: Add to the exsiting Unit Connections\n 3: Replace the existing Unit Connections")
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
          myMap2 <- purrr::map(doc, 1)[[i]]
          if (suppressWarnings(stringr::str_detect(
            myMap2,
            "NPS Content Unit Link"
          )) == FALSE) {
            no_units <- append(no_units, doc[i])
          }
        }
        # if the only geo unit was a previous connection, replace it:
        if (is.null(no_units)) {
          eml_object$dataset$coverage$geographicCoverage <- unit_list
        }
        #if there are geographic units other than content units, add to those:
        if (!is.null(no_units)) {
          #if there is only one non-content unit geographic coverage element:
          unit_list<-append(unit_list, no_units)
          #insert into EML:
          eml_object$dataset$coverage$geographicCoverage <- unit_list
          }
          if (length(no_units) > 1) {
            my_list <- append(unit_list, no_units)
            eml_object$dataset$coverage$geographicCoverage <- my_list
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
    # if the only geo unit was not a previous connection, add connections:
    if (is.null(exist_units)) {
      #if no geographic coverage at all:
      if (is.null(doc)) {
        eml_object$dataset$coverage$geographicCoverage <- unit_list
      }
      if (!is.null(doc)){
        unit_list <- append(unit_list, doc)
        eml_object$dataset$coverage$geographicCoverage <- unit_list
      }
    }
    else {
        # make sure everything is nested to the same hierarchical level:
      if(!is.null(names(doc))){
          doc <- list(doc)
      }
        #get all geographic coverage that is NOT content unit links:
      no_units <- NULL
      for (i in seq_along(doc)) {
        myMap2 <- purrr::map(doc, 1)[[i]]
        if (suppressWarnings(stringr::str_detect(
          myMap2,
          "NPS Content Unit Link"
        )) == FALSE) {
          no_units <- append(no_units, doc[i])
        }
      }
        # if the only geo unit was a previous connection, replace it:
      if (is.null(no_units)) {
        eml_object$dataset$coverage$geographicCoverage <- unit_list
       }
        #if there are geographic units other than content units, add to those:
      if (!is.null(no_units)) {
        #if there is only one non-content unit geographic coverage element:
        unit_list<-append(unit_list, no_units)
        #insert into EML:
        eml_object$dataset$coverage$geographicCoverage <- unit_list
      }
      if (length(no_units) > 1) {
        my_list <- append(unit_list, no_units)
        eml_object$dataset$coverage$geographicCoverage <- my_list
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

#' Adds CUI to metadata
#'
#' @description set_cui adds CUI codes to EML metadata
#'
#' @details set_cui adds a CUI code to the tag CUI under additionalMetadata/metadata.
#'
#' @inheritParams set_title
#' @param cui_code a string consisting of one of 7 potential CUI codes (defaults to "PUBFUL"). Pay attention to the spaces:
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
        cat("No previous CUI was detected. Your CUI has been set to ",
            crayon::bold$blue(cui_code), ".", sep = "")
      }
      # If existing CUI, stop.
      if (!is.null(exist_cui)) {
        cat("CUI has previously been specified as ",
            crayon::bold$blue(exist_cui),
            ".\n", sep = "")
        var1 <- readline(prompt = "Are you sure you want to reset it? \n\n 1: Yes\n 2: No\n")
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
set_drr <- function(eml_object, drr_ref_id, drr_title, org_name = "NPS", force = FALSE, NPS = TRUE) {
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
      cat("Your DRR, ", crayon::blue$bold(drr_title), " has been added.", sep = "")
    } else {
      cat("Your current DRR is: ", crayon::blue$bold(doc$title), ".\n", sep = "")
      cat("The current DOI is: ", crayon::blue$bold(doc$alternateIdentifier),
        ".\n",
        sep = ""
      )
      var1 <- readline(prompt = "Are you sure you want to change it? \n\n 1: Yes\n 2: No\n")
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
#' @description set_abstract adds (or replaces) a simple abstract.
#'
#' @details checks for an abstract. If no abstract is found, it inserts the abstract given in @param abstract. If an existing abstract is found, the user is asked whether they want to replace it or not and the appropriate action is taken. Currently set_abstract does not allow for paragraphs or complex formatting. You are strongly encouraged to open your abstract in a text editor such as notepad and make sure there are no stray characters. If you need multiple paragraphs, you will need to do that via EMLassemblyline (for now).
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
      var1 <- readline(prompt = "Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
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
#' @description `set_additional_info()` will add information to the additionalInfo element in EML. The contents of this element are used to populate the 'notes' field on the DataStore landing page. Users may want to edit the notes if errors or non-ASCII text characters are discovred because the notes are prominently displayed on DataStore.
#'
#' Users are highly encouraged to generate Notes using a text editor such as Notepad rather than a word processor such as MS Word.
#'
#' @inheritParams set_title
#' @param additional_info String. Will become the "notes" on the DataStore landing page.
#' @param force
#' @param NPS
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_additional_info(eml_object, "Here is some text for the Notes section on DataStore.")
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
      var1 <- readline(prompt = "Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
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
      var1 <- readline(prompt = "Are you sure you want to replace them? \n\n 1: Yes\n 2: No\n")
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
      var1 <- readline(prompt = "Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")

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
#' @description set_protocol adds a metadata link to the protocol under which the data being described were collected. It automatically inserts a link to the DataStore landing page for the protocol as well as ?????
#'
#' @details set_protocol requires that you have your protocols and projects organized in a specific fashion in DataStore. Errors generated by this function my stem from either a protocol that has not been published (or is not publicly available) or an obsolete protocol/project organization within DataStore.
#'
#' @inheritParams set_title
#' @param protocol_id a string. The 7-digit number identifying the DataStore reference number for the Project that describes your inventory or monitoring project.
#'
#' @return emlObject
#' @export
#'
#' @examples
#' \dontrun{
#' set_protocol(eml_object, 2222140)
#' }
#'
set_protocol <- function(eml_object, protocol_id, force = FALSE, NPS = TRUE) {
  # get data to construct project:

  # get protocol profile via rest services:
  ds_reference <- httr::content(httr::GET(paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/", protocol_id)))

  # extract project title
  proj_title <- ds_reference$bibliography$title

  # generate URL for the DataStore landing page:
  url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/", protocol_id)

  # get DataStore ref number for the organization Name:
  ref <- ds_reference$series$referenceId

  # rest services call to get organization name info:
  org_name <- httr::content(httr::GET(paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/", ref)))$bibliography$title

  # Construct a project to inject into EML. Note 'role' is required but not sure what to put there.
  # Also i find it confusing that onlineURL references projTitle not orgName but hopefully we will hash that out soon.

  proj <- list(
    title = proj_title,
    personnel = list(
      organizationName = org_name,
      onlineUrl = url,
      role = "originator"
    )
  )
  # scripting route:
  if (force == TRUE) {
    eml_object$dataset$project <- proj
  }
  # interactive route:
  if (force == FALSE) {
    # get existing project (if any)
    doc <- eml_object$dataset$project

    # if no previous project listed, add project
    if (is.null(doc)) {
      eml_object$dataset$project <- proj
      cat("The current project is now ", crayon::bold$blue(proj$title),
        ".",
        sep = ""
      )
    }

    # if an there is an existing project, ask whether to replace:
    else {
      cat("you already have a project(s) with the Title:\n",
        crayon::bold$blue(doc$title), ".",
        sep = ""
      )

      var1 <- readline(prompt = "Are you sure you want to replace it? \n\n
                     1: Yes\n 2: No\n")

      # if yes, change the project:
      if (var1 == 1) {
        eml_object$dataset$project <- proj
        cat("The current project is now ",
          crayon::bold$blue(proj$title), ".",
          sep = ""
        )
      }

      # if no, retain the existing project:
      if (var1 == 2) {
        cat("Your original project was retained.")
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
#' @return emlObject
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
        cat("Organization Name: ", crayon::blue$bold(pubset$organizationName), "\n", sep = "")
        cat("Street address: ", crayon::blue$bold(pubset$address$deliveryPoint), "\n", sep = "")
        cat("City: ", crayon::blue$bold(pubset$address$city), "\n", sep = "")
        cat("State: ", crayon::blue$bold(pubset$address$administrativeArea), "\n", sep = "")
        cat("Zip Code: ", crayon::blue$bold(pubset$address$postalCode), "\n", sep = "")
        cat("Country: ", crayon::blue$bold(pubset$address$country), "\n", sep = "")
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
          var1 <- readline(prompt = "Would you like to replace your existing publisher? \n\n 1: Yes\n 2: No\n")
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
          var2 <- readline(prompt = "Would you like to replace your agency? \n\n 1: Yes\n 2: No\n")
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

#' Set Intellectual Rights (and license name)
#'
#' @description set_int_rights allows the intellectualRights field in EML to be surgically replaced.
#'
#' @details set_int_rights requires that CUI information be listed in additionalMetadata prior to being called. The verbose `force = FALSE` option will warn the user if there is no CUI specified. set_int_rights checks to make sure the CUI code specified (see `set_cui()`) is appropriate for the license type chosen.

#' @inheritParams set_title
#'
#' @param license String. Indicates the type of license to be used. The three potential options are "CC0" (CC zero), "public" and "restricted". CC0 and public can only be used if CUI is set to either PUBFUL or PUBVER. Restricted can only be used if CUI is set to any code that is NOT PUBFUL or PUBVER (see `set_cui()` for a list of codes). To view the exact text that will be inserted for each license, please see https://nationalparkservice.github.io/NPS_EML_Script/stepbystep.html#intellectual-rights
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
  restrict <- "This product has been determined to contain Controlled Unclassified Information (CUI) by the National Park Service, and is intended for internal use only. It is not published under an open license. Unauthorized access, use, and distribution are prohibited."

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
            eml_object$dataset$licensed$licenseName <- "CC0 1.0 Universal"
            cat("Your license has been set to:", crayon::blue$bold("CC0"))
          }
          if(license == "public"){
            eml_object$dataset$intellectualRights <- pub_domain
            eml_object$dataset$licensed$licenseName <- "Public Domain"
            cat("Your license has been set to:", crayon::blue$bold("Public Domain"))
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
          eml_object$dataset$licensed$licenseName <- "No License/Controlled Unclassified Information"
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
#' @details `set_data_urls()` sets the online distribution URL for all dataTables (data files in a data package) to the same URL. If you do not supply a URL, your metadata must include a DOI (use `set_doi()` or `set_datastore_doi()` to add a DOI - these will automatically update your data table urls to match the new DOI). `set_data_urls()` assumes that DOIs refer to digital objects on DataStore and that the last 7 digist of the DOI correspond to the DataStore Reference ID.
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
        eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <- data_url
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
        eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <- url
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
#' @description `set_creator_orcids()` allows users to add ORCiDs to creators or edit/update existing ORCiDs associated with creators. ORCiDs are persistent digital identifiers associated with individual people and remain constant despite name changes. They can help disambiguate creators with similar names and associated all the products of one creator in one space despite variations in how the name was used (e.g. Rob Baker and Robert Baker and. Robert L. Baker but NOT any of the 15 million or so other Robert Bakers). To register an ORCiD or manage your ORCiD profile, go to [https://orcid.org/](https://orcid.org/).
#'
#' @details ORCiDs should be supplied as a list in the order in which the creators are listed. If a creator does not have an ORCiD, put "NA" in the list in that space. Only consider individual people who are creators (and not organizations, they will automatically be skipped). ORCiDs should be supplied as a 16-digit string with hyphens after every 4 digits: xxxx-xxxx-xxxx-xxxx. Please do not include the URL prefix for your ORCiDs; this will automatically be inserted for you.
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
#' #three creators, the second of which does not hae an ORCiD:
#' creator_orcids <- c("1234-1234-1234-1234", "NA", "4321-4321-4321-4321")
#' mymetadata <- set_creator_orcids(mymetadata, creator_orcids)
#' }
set_creator_orcids <- function(eml_object, orcids, force = FALSE, NPS = TRUE){

  #make sure they didn't include URLs:
  if(sum(grep("https://orcid.org/", orcids)) > 0){
    cat("The ORCiD(s) you supplied appear to be incorrectly formatted.\n")
    cat("Please supply ORCiDs in the following format: xxxx-xxxx-xxxx-xxxx",
        " (No URLs).\n", sep="")
    return()
  }

  #get creators
  creator <- eml_object[["dataset"]][["creator"]]

  # If there's only one creator, creator ends up with one less level of nesting. Re-nest it so that the rest of the code works consistently
  names_list <- c("individualName", "organizationName", "positionName")
  if(sum(names_list %in% names(creator)) > 0){
    creator <- list(creator)
  }

  #get existing orcids (should this be its own function?):
  surName <- NULL
  existing_orcid <- NULL
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      #check for orcid directory id:
      surName <- append(surName, creator[[i]][["individualName"]][["surName"]])
      existing_orcid <- append(existing_orcid, creator[[i]][["userId"]][["userId"]])
    }
  }

  #if verbose route:
  if(force == FALSE){
    #if there are already orcids:
    if(!is.null(existing_orcid)){
      #construct tibble of existing orcids:
      current_orcids <- tibble::tibble(surName, existing_orcid)
      #construct tibble of replacement orcids:
      new_orcids <- tibble::tibble(surName, orcids)
      cat("Your data package contains ORCiDs for the following creators:\n\n")
      cat(format(tibble::as_tibble(current_orcids))[c(-3L, -1L)], sep = "\n")
      cat("\nAre you sure you want to replace the existing ORCiDs with the following:\n\n")
      cat(format(tibble::as_tibble(new_orcids))[c(-3L, -1L)], sep = "\n")
      var1 <- readline(prompt = cat("\n\n1: Yes\n2: No\n\n"))

      if(var1 == 2){
        cat("Your original ORCiDs were retained.")
        return()
      }
    }
  }

  #generate list of new orcids:
  author_order <- 1
  for(i in seq_along(creator)){
    if("individualName" %in% names(creator[[i]])){
      replace_orcid <- paste0("https://orcid.org/", orcids[author_order])
      author_order <- author_order +1
      userId2 <- list(list(userId = replace_orcid), directory = "https://orcid.org")
      creator[[i]][["userId"]] <- userId2
    }
  }

  #add/replace orcids with updated/new orcids:
  eml_object[["dataset"]][["creator"]] <- creator

  #if verbose route, display new orcids:
  if(force == FALSE){
    #get new orcids - should this be it's own function?
    creator <- eml_object[["dataset"]][["creator"]]
    names_list <- c("individualName", "organizationName", "positionName")
    if(sum(names_list %in% names(creator)) > 0){
      creator <- list(creator)
    }

    surName <- NULL
    existing_orcid <- NULL
    for(i in seq_along(creator)){
      if("individualName" %in% names(creator[[i]])){
        #check for orcid directory id:
        surName <- append(surName, creator[[i]][["individualName"]][["surName"]])
        existing_orcid <- append(existing_orcid, creator[[i]][["userId"]][[1]][["userId"]])
      }
    }
    #construct tibble of existing orcids:
    new_orcids <- tibble::tibble(surName, existing_orcid)
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
#' @param RORs List. Defaults to NA. An optional list of one or more ROR IDs (see https:/ror.org) that correspond to the organization in question. If an organization does not hae a ROR ID (or you don't know it), enter "NA".
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

    alpha <- dat %>% dplyr::filter(grepl(paste(park_units, collapse = '|'), UnitCode, ignore.case = TRUE))

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
        creator_list <- append(creator_list, creator_new[[i]][["organizationName"]])
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
      var1 <- readline(prompt="")
      #don't allow user to remove all creators!
      if(nchar(var1)==0){
        cat("You cannot delete all creators.")
        cat("Please enter comma-separated numbers for the new creator order.\n")
        var1 <- readline(prompt="")
      }
      ord <- as.list(strsplit(var1, ","))[[1]]
      ord <- trimws(ord)
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


















