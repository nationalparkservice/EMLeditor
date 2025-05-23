#' Returns a simple list of eml elements
#'
#' This function wraps EML::get_eml and returns the requested object in a much simpler format. It works best with eml elements that do not have child elements.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param eml_element is a string indicating the element within the eml_object that should be accessed and returned.
#'
#' @returns a list of values
#' @export
#' @examples
#' \dontrun{
#' get_eml_simple(eml_object, "geographicDescription")
#' }
get_eml_simple <- function(eml_object, eml_element){
  out <- EML::eml_get(eml_object, eml_element, from = "list")
  out$`@context` <- NULL
  attributes(out) <- NULL
  out <- unlist(out)
  return(out)
}

#' returns the first date
#'
#' @description get_begin_date returns the date of the earliest data point in the data package
#'
#' @details returns the date from the <beginDate> tag. Although dates should be formatted according to ISO-8601 (YYYY-MM-DD) it will also check for a few other common formats and return the date as a text string: "DD Month YYYY"
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_begin_date(eml_object)
#' }
get_begin_date <- function(eml_object) {
  begin <- get_eml_simple(eml_object, "beginDate")
  if (is.null(begin)) {
    warning("Your metadata lacks a begining date.")
    begin <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    begin %>%
      as.Date() %>%
      format("%d %B %Y")
  }
}

#' returns the last date
#'
#' @description get_end_date returns the date of the last data point in the data package
#'
#' @details returns the date from the <endDate> tag. Although dates should be formatted according to ISO-8601 (YYYY-MM-DD) it will also check a few other common formats and return the date as a text string: "DD Month YYYY"
#'
#' @inheritParams get_begin_date
#' @return a text sting
#' @export
#' @examples
#' \dontrun{
#' get_end_date(eml_object)
#' }
get_end_date <- function(eml_object) {
  end <- get_eml_simple(eml_object, "endDate")
  if (is.null(end)) {
    warning("Your metadata lacks an ending date.")
    end <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    end %>%
      as.Date() %>%
      format("%d %B %Y")
  }
}


#' returns the abstract
#'
#' @description returns the text from the <abstract> tag.
#'
#' @details get_abstract returns the text from the <abstract> tag and attempts to clean up common text issues, such as enforcing UTF-8 formatting, getting rid of carriage returns, new lines, <para> and <literalLayout> tags and mucks about with layout, line breaks, etc. IF you see characters you don't like in the abstract, make sure to edit your abstract in a text editor (e.g. Notepad and NOT a Word). You should save the text to a new object and view it using writeLines()
#'
#' @inheritParams get_begin_date
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' abstract <- get_abstract(eml_object)
#' writeLines(abstract)
#' }
get_abstract <- function(eml_object) {
  doc <- eml_object[["dataset"]][["abstract"]]
  if (is.null(doc)) {
    warning("Your EML lacks an abstract. Use set_abstract() to add one.")
    txt <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    #doc<-enc2utf8(doc) # helps with weird characters
    txt <- NULL
    for (i in 1:length(seq_along(doc))) {
      if (nchar(doc[i]) > 0) {
        mypara <- gsub("[\r?\n|\r]", "", doc[i]) # get rid of line breaks and carriage returns
        mypara <- enc2utf8(mypara) #set encoding to UTF-8 deals with many stray characters
        mypara <- gsub("&#13;", " ", mypara) # get rid of carriage symbols
        mypara <- gsub("&amp;#13;", ". ", mypara)
        mypara <- gsub("<literalLayout>", "", mypara) # get rid of literalLayout tag
        mypara <- gsub("<para>", "", mypara) # get rid of para tag
        mypara <- gsub("</para>", "", mypara) # get rid of close para tag
        mypara <- gsub("</literalLayout>", "", mypara) # get rid of close par tag
        mypara <- gsub("   ", " ", mypara) # get rid of 3x spaces
        mypara <- gsub("  ", " ", mypara) # get rid of 2x spaces
        mypara <- gsub("  ", " ", mypara) # rerun for 4x spaces
        txt <- paste0(txt, mypara)
        if (i < length(seq_along(doc))) {
          txt <- paste0(txt, "\n\n\t") # add paragraph sep
        }
      }
    }
  }
  return(txt)
}

#' Get methods
#'
#' @description `get_methods()` returns the text stored in the methods element of EML metadata. The returned text is not manipulated in any way. DataStore unlists the returned object (get rid of tags such as $methodStep, $methodStep$description and $methodStep$description$para and remove the numbers in brackets). the "\\n" character combination is interpreted as a line break (as are blank lines). However, DataStore will not filter out stray characters such as &amp;#13;. Use the `set_methods()` function to edit and replace the text stored in the methods field.
#'
#' @inheritParams get_begin_date
#'
#' @return List
#' @export
#'
#' @examples
#' \dontrun{
#' get_methods(eml_object)
#' }
#'
#'
get_methods <- function(eml_object){
  doc <- eml_object$dataset$methods
  if(is.null(doc)){
    warning("Your EML lacks a methods section. Use set_methods() to add methods.")
  }
  return(doc)
}



#' Get additional information (Notes on DataStore)
#'
#' @description `get_additional_info()` returns the text in the additionalInformation element of EML. This text will be used to populate the "Notes" sectionon the DataStore reference page. There is no strict limit on what can and cannot go in to the additionalInformation/Notes section. However, DataStore will not filter out stray characters such as &amp;#13;. Use the `set_additional_info()` function to edit and replace the text stored in the additionalInformation (notes) field.
#'
#' @inheritParams get_begin_date
#'
#' @return String
#' @export
#'
#' @examples
#' \dontrun{
#' get_additional_info(eml_object)
#' }
get_additional_info <- function(eml_object) {
  doc <- eml_object$dataset$additionalInfo
  if(is.null(doc)) {
    warning("Your EML lacks additional info. Use set_additional_info() to add it.")
  }
  return(doc)

}

#' returns the data package title
#'
#' @description get_title returns a text string that is the title of the data package
#'
#' @details accesses all of the <title> tags (there can be several, if each file was given a separate title). Assumes that the first instance of <title> referes to the entire data package and returns it as a text string, ignoring the contents of all other <title> tags.
#'
#' @inheritParams get_begin_date
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_title(eml_object)
#' }
get_title <- function(eml_object) {
  doc <- get_eml_simple(eml_object, "title")[1]
  if (is.null(doc)) {
    doc <- NA
  }
  return(doc)
}

#' returns the DataStore Reference ID
#'
#' @description get_ds_id returns the DataStore Reference ID as a string of text.
#'
#' @details accesses the DOI listed in the <alternateIdentifier> tag and trims to to the last 7 digits, which should be identical to the DataStore Reference ID. If the <alternateIdentifier> tag is empty, it notifies the user that there is no DOI associate with the metadata and suggests adding one using set_doi() (edit_doi() would also work).
#'
#' @inheritParams get_begin_date
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_ds_id(eml_object)
#' }
get_ds_id <- function(eml_object) {
  pid <- eml_object$dataset$alternateIdentifier
  if (is.null(pid)) {
    warning("Your EML lacks a DOI in the \"alternateIdentifier\" tag.\n Please use the set_doi() function to add your DOI")
    ref_id <- NA # to do: check write.readMe whether NA needs to be in quotes.
  } else {
    for (i in seq_along(pid)) {
      if (stringr::str_detect(pid[i], "doi: ")) {
        doi <- pid[i]
      }
    }
    ref_id <- stringr::str_sub(doi, start = -7)
    if (suppressWarnings(is.na(as.numeric(ref_id)))) {
      warning("Your DOI is not consistent with an NPS DOI. Use set_doi() to update your DOI.")
      ref_id <- NA
    }
  }
  return(ref_id)
}

#' returns the data package citation
#'
#' @description returns a Chicago manual of style citation for the data package
#'
#' @details get_citation allows the user to preview the what the citation will look like. The Harper's Ferry Style Guide recommends using the Chicago Manual of Style for formatting citations. The citation is formatted according to to a modified version of the Chicago Manual of Style's Author-Date journal article format because currently there is no Chicago Manual of Style format specified for datasets or data packages. In compliance with DataCite's recommendations regarding including DOIs in citations, the citation displays the entire DOI as https://www.doi.org/10.58370/xxxxxx".
#'
#' @inheritParams get_begin_date
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_citation(eml_object)
#' }
get_citation <- function(eml_object) {
  # assemble the pieces:

  doi <- get_doi(eml_object)

  author_list <- get_author_list(eml_object)

  title <- get_title(eml_object)

  pub_date <- get_eml_simple(eml_object, "pubDate")
  pub_date <- lubridate::parse_date_time(pub_date, orders = "Y-m-d")
  pub_year <- lubridate::year(pub_date)

  publisher <- eml_object$dataset$publisher$organizationName


  city <- eml_object$dataset$publisher$address$city

  state <- eml_object$dataset$publisher$address$administrativeArea

  location <- paste0(city, ", ", state)
  # print(location)

  #### what to do if no doi ("set" eml?)?

  # print(doi)

  # piece it together:
  if (is.null(doi)) {
    warning("No doi specified. Please use set_doi() to add a DOI.")
  }
  if (is.null(author_list)) {
    warning("No creators detected. Please add at least one \"creator\" in EMLassemlbyline with a valid givenName and surName.")
  }
  if (is.null(title)) {
    warning("No title specified.")
  }
  if (is.null(pub_date)) {
    warning("No publication date specified.")
  }

  data_citation <- paste0(author_list, " ", pub_year, ". ", title, ". ", publisher, ". ", location, ".", doi)

  return(data_citation)
}

#' returns the authors
#'
#' @description `get_author_list()` returns a text string with all of the authors listed under the <creator> tag.
#'
#' @details `get_author_list()` assumes every author has at least 1 first name (either givenName or givenName1) and only one last name (surName). Middle names (givenName2) are optional. The author List is formatted with the last name, comma,  first name for the first author and the fist name, last name for all subsequent authors. The last author's name is preceded by an 'and'.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_author_list(eml_object)
#' }
get_author_list <- function(eml_object) {
  # get author names & affiliations
  authors <- eml_object[["dataset"]][["creator"]]

  # if no authors are specified (not really possible with EMLassemblyline)
  if (is.null(authors)) {
    warning("No authors specified in the <creator> tag.")
    last_first <- NA
  } else {
    authors <- unlist(authors)

    # extract givenName; should handle middle names too!
    first_name <- NULL
    for (i in seq_along(authors)) {
      if (stringr::str_detect(names(authors)[i], "givenName\\b")) {
        first_name <- append(first_name, authors[i][[1]])
      } else if (stringr::str_detect(names(authors)[i], "givenName1\\b")) {
        first_middle <- paste0(authors[i], " ", authors[i + 1])
        first_name <- append(first_name, first_middle)
      }
    }

    # extract surName
    last_name <- NULL
    for (i in seq_along(authors)) {
      if (stringr::str_detect(names(authors)[i], "surName")) {
        last_name <- append(last_name, authors[i][[1]])
      }
    }

    # create a single object that is a string consisting of the ith author, formatted according to the Chicago manual of style, Journal article:
    author <- NULL
    last_first <- NULL

    if (length(last_name) > 0) {
      # single author:
      if (length(last_name) == 1) {
        author <- paste0(last_name, ", ", first_name, ".")
        last_first <- author
      }

      # multi-author:
      else {
        for (i in 1:length(seq_along(last_name))) {
          if (i == 1) {
            author <- paste0(last_name[i], ", ", first_name[i])
          }
          if (i > 1 && i < length(last_name)) {
            author <- paste0(first_name[i], " ", last_name[i])
          }
          if (i > 1 && i == length(last_name)) {
            author <- paste0("and ", first_name[i], " ", last_name[i], ".")
          }
          last_first <- append(last_first, author)
        }
        # make it a string, not a list:
        last_first <- toString(last_first)
      }
    }
    if(is.null(last_first)){
      cat("There is something wrong with the creators field. Please check that you have a supplied a givenName and surName for each creator.")
    }
  }
  return(last_first)
}

#' returns the DOI
#'
#' @description returns a text string that is the DOI for the data package
#'
#' @details `get_doi()` accesses the contents of the<alternateIdentifier> tag and does some text manipulation to return a string with the DOI including the URL and prefaced by 'doi: '.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_doi(eml_object)
#' }
get_doi <- function(eml_object) {
  # where EMLassemblyline stores DOIs.
  pid <- eml_object$dataset$alternateIdentifier
  if (is.null(pid)) {
    cat("Your EML lacks a DOI in the \"alternateIdentifier\" tag. ")
    cat("Please use", crayon::green$bold("set_doi()"), "or", crayon::green$bold("set_datastore_doi()"), "to add your DOI.\n")
    doi <- NA # to do: does NA need to be in quotes for write.ReadMe?
  } else {
    my_list <- NULL
    if (length(pid) >= 1) {
      for (i in seq_along(pid)) {
        if (stringr::str_detect(pid[i], "doi:")) {
          my_list <- append(my_list, pid[i])
        }
      }
    }
    doi <- my_list[[1]]
    doi <- gsub("doi:", "", doi)
  }
  return(doi)
}

#' returns the park unit connections
#'
#' @description returns a string with the park unit codes where the data were collected
#'
#' @details `get_content_units()` accesses the contents of the <geographicDescription> tags and returns the contents of the tag that contains the text "NPS Unit Connections". If there is no <geographicDescription>, it alerts the user and suggests adding park unit connections using the set_park_units() function.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_content_units(eml_object)
#' }
get_content_units <- function(eml_object) {
  units <- get_eml_simple(eml_object, "geographicDescription")
  if (is.null(units)) {
    warning("No Park Unit Connections specified. Use the set_content_units() function to add Park Unit Connections.")
    punits <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    # pull out just geographic description for unit connections:
    unit_cons <- NULL
    for (i in seq_along(units)) {
      if (stringr::str_detect(units[i], "NPS Content Unit Link:")) {
        unit_cons <- append(unit_cons, units[i])
      }
    }
    if (is.null(unit_cons)) {
      warning("No Park Unit Connections specified. Use the set_content_units() function to add Park Unit Connections.")
    }
    # make a string that is just comma separated unit connection codes:
    punits <- NULL
    for (i in seq_along(unit_cons)) {
      if (unit_cons[i] == utils::tail(unit_cons, 1)) {
        rem_text <- sub("NPS Content Unit Link: ", "", unit_cons[i])
        punits <- append(punits, rem_text)
      } else {
        rem_text <- sub("NPS Content Unit Link: ", "", unit_cons[i])
        punits <- append(punits, paste0(rem_text, ", "))
      }
    }
    list_units <- paste(unlist(punits), collapse = "", sep = ",")

    # add "NPS Content Unit Links: " prefix back in to the sting:
    list_units <- paste0("NPS Content Unit Link(s): ", list_units)
  }
  return(list_units)
}

#' returns a CUI dissemination code statement
#'
#' @description `get_cui_code()` returns an English-language translation of the CUI dissemination codes. It supersedes `get_cui()`, which has been deprecated.
#'
#' @details `get_cui_code()` accesses the contents of the Controlled Unclassified Information (CUI) tag, <CUI> and returns an appropriate string of english-language text based on the properties of the CUI code. If thee <CUI> tag is empty or does not exist, get_cui alerts the user and suggests specifying CUI using the set_cui() funciton.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_cui(eml_object)
#' }
get_cui_code <- function(eml_object) {
  cui <- get_eml_simple(eml_object, "CUI")
  if (is.null(cui)) {
    cat("No CUI specified. Use the set_cui() function to add a properly formatted CUI code.")
    cui <- "No CUI specified."
  } else if (cui == "FED ONLY") {
    cui <- "Contains CUI. Only federal employees should have access (similar to \"internal only\" in DataStore)."
  } else if (cui == "FEDCON") {
    cui <- "Contains CUI. Only federal employees and federal contractors should have access (also very much like current \"internal only\" setting in DataStore)."
  } else if (cui == "DL ONLY") {
    cui <- "Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)."
  } else if (cui == "NOCON") {
    cui <- "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot."
  } else if (cui == "PUBVER") {
    cui <- "Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI."
  } else if (cui == "PUBFUL") {
    cui <- "Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package."
  } else if (cui == "PUBLIC") {
    cui <- "Does NOT contain CUI"
  }
  else {
    warning("CUI not properly specified. Use set_cui() to update the CUI code.")
    cui <- NA
  }
  return(cui)
}

#' returns a CUI statement
#'
#' @description
#' #' `r lifecycle::badge("deprecated")`
#' Deprecated in favor of `get_cui_code()`. `get_cui()` returns an English-language translation of the CUI codes
#'
#' @details `get_cui()` accesses the contents of the Controlled Unclassified Information (CUI) tag, <CUI> and returns an appropriate string of english-language text based on the properties of the CUI code. If thee <CUI> tag is empty or does not exist, get_cui alerts the user and suggests specifying CUI using the set_cui() funciton.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_cui(eml_object)
#' }
get_cui <- function(eml_object) {
  #add in deprecation
  lifecycle::deprecate_soft(when = "0.1.5", "set_cui()", "set_cui_dissem()")

  cui <- get_eml_simple(eml_object, "CUI")
  if (is.null(cui)) {
    cat("No CUI specified. Use the set_cui() function to add a properly formatted CUI code.")
    cui <- "No CUI specified."
  } else if (cui == "FED ONLY") {
    cui <- "Contains CUI. Only federal employees should have access (similar to \"internal only\" in DataStore)."
  } else if (cui == "FEDCON") {
    cui <- "Contains CUI. Only federal employees and federal contractors should have access (also very much like current \"internal only\" setting in DataStore)."
  } else if (cui == "DL ONLY") {
    cui <- "Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)."
  } else if (cui == "NOCON") {
    cui <- "Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot."
  } else if (cui == "PUBVER") {
    cui <- "Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI."
  } else if (cui == "PUBFUL") {
    cui <- "Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package."
  } else if (cui == "PUBLIC") {
    cui <- "Does NOT contain CUI"
  }
  else {
    warning("CUI not properly specified. Use set_cui() to update the CUI code.")
    cui <- NA
  }
  return(cui)
}

#' Returns the CUI marking
#'
#' @description
#' For data with controlled unclassified information (CUI), `get_cui_marking()` eturns the specific marking and the english language explanation of the marking. For data without CUI, it informs that there is no CUI and returns the code "PUBLIC".
#'
#' @details
#' CUI markings are defined by the U.S. National Archives (nara.gov). NPS users can designate one of three CUI markings, plus the code "PUBLIC" (essentially, no marking necessary). The three markings are: SP-NPSR, SP-HISTP or SP-ARCHR.
#' For more information on CUI markings, please visit the [CUI Markings](https://www.archives.gov/cui/registry/category-marking-list) list maintained by the National Archives.
#'
#'
##' @inheritParams get_begin_date
#'
#' @return String (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#' get_cui_marking(eml_object)
#' }
get_cui_marking <- function(eml_object) {
  # get existing additionalMetadata elements:
  add_meta <- eml_object$additionalMetadata

  #get location of CUI markings from addtiional metadata
  y <- NULL
  for (i in 1:length(seq_along(add_meta))) {
    if(names(add_meta[[i]][["metadata"]]) == "CUImarking") {
      y <- i
      break
    }
  }
  #if there is not yet any CUI marking:
  if (is.null(y)) {
    cat("Your metadata do not yet contain a CUI marking.\n")
    cat("Please use ",
        crayon::green$bold("set_cui_marking"),
        " to add the appropriate CUI marking to your metadata.",
        sep = "")
    return(invisible())
  }

  #if CUI marking exists:
  if (!is.null(y)) {
    #get existing CUI marking:
    existing_cui_marking <- add_meta[[y]][["metadata"]][["CUImarking"]]
    if(existing_cui_marking == "PUBLIC") {
      msg <- paste0("Your CUI marking is set to ",
                    crayon::blue("PUBLIC"),
                    ". This means the data do not contain CUI.")
    } else if (existing_cui_marking == "SP-NPSR") {
      msg <- paste0("Your CUI marking is set to ",
                    crayon::blue$bold(existing_cui_marking),
                    ". This means the CUI in the data is related to",
                    " information concerning the nature and specific location",
                    " of a National Park System resource that is endangered, ",
                    "threatened, rare, or commercially valuable, of mineral",
                    " or paleontological objects within System units, or of",
                    " objects of cultural patrimony within System unit")
    } else if (existing_cui_marking == "SP-HISTP") {
      msg <- paste0("Your CUI marking is set to ",
                    crayon::blue$bold(existing_cui_marking),
                    ". This means the CUI in the data is related to the",
                    " location character, or ownership of historic property.")
    } else if (existing_cui_marking == "SP-ARCHR") {
      msg <- paste0("Your CUI marking is set to ",
                    crayon::blue$bold(existing_cui_marking),
                    ". This means the CUI in the data is related to ",
                    "information about the nature and location of any",
                    " archaeological resource for which the excavation or",
                    " removal requires a permit or other permission.")
    } else {
      warning("CUI marking is not properly set. Please use set_cui_marking to fix it.")
      msg <- NA
    }
  }
  cat(msg)
  return(invisible(msg))
}


#' displays file names, sizes, and descriptions
#'
#' @description get_file_info returns a plain-text table containing file names, file sizes, and short descriptions of the files.
#'
#' @details get_file_info returns the file names (listed in the <objectName> tag), the size of the files (listed in the <size> tag) and converts it from bytes (B) to a more easily interpretable unit (KB, MB, GB, etc). Technically this uses powers of 2^10 so that KB is actually a kibibyte (1024 bytes) and not a kilobyte (1000 bytes). Similarly MB is a mebibyte not a megabyte, GB is a gibibyte not a gigabyte, etc. But for most practical purposes this is probably irrelevant. Finally, a short description is provided for each file (from the <entityDescription> tag).
#'
#' @inheritParams get_begin_date
#'
#' @importFrom magrittr %>%
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_file_info(eml_object)
#' }
get_file_info <- function(eml_object) {
  # get file names
  file_name <- get_eml_simple(eml_object, "objectName")

  if (is.null(file_name)) {
    warning("You have not specified data file names, sizes, or descriptions. If you used EMLassemblyline, double check for any issues generated after running make_eml. Missing data and undefined units will often cause this problem.")
    print("NA")
  } else {
    # get file sizes (assumes in bytes)
    file_size <- get_eml_simple(eml_object, "size")
    file_size <- suppressWarnings(as.numeric(file_size))
    file_byte <- unique(file_size)
    file_byte <- file_byte[!is.na(file_byte)]
    readable <- gdata::humanReadable(file_byte,
                                     standard = "Unix") %>% paste0("B")

    # get file descriptions
    file_descript <- get_eml_simple(eml_object, "entityDescription")

    # generate dataframe for display:
    dat <- data.frame(file_name, readable, file_descript)
    colnames(dat) <- c("FileName", "Size", "Description")

    #print("Current filenames and file descriptions:")
    return(dat)
  }
}

#' returns the DOI of the associated DRR
#'
#' @description get_drr_doi returns a text string with the associated Data Release Report (DRR)'s DOI.
#'
#' @details get_drr_doi accesses the <usageCitation> tag(s) and searches an alternateIdentifier tag. If that element is found, the contents of that element are returned. If the title element is empty or not present, the user is warned and pointed to the set_drr function to add the DOI of an associated DRR.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#' @examples
#' \dontrun{
#' get_drr_doi(eml_object)
#' }
get_drr_doi <- function(eml_object) {
  doi <- eml_object$dataset$usageCitation$alternateIdentifier
  if (is.null(doi)) {
    cat(crayon::red$bold("Warning: "),
    "You have not specified a DRR associated with this data package.
    If you have an associated DRR, specify its DOI using ",
    crayon::green$bold("set_drr()"), ".\n", sep="")
    drr_doi <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    return(doi)
  }
}


#' returns the title of the associated DRR
#'
#' @description get_drr_title returns a text string with the associated Data Release Report (DRR)'s Title.
#'
#' @details get_drr_title accesses useageCitation under dataset and returns the title element, if it is found. If it is not found, the user is warned and pointed ot the set_drr function to add the title of the associated DRR.
#'
#' @inheritParams get_begin_date
#'
#' @return a text string
#' @export
#'
#' @examples
#' \dontrun{
#' get_drr_title(eml_object)
#' }
get_drr_title <- function(eml_object) {
  doi <- eml_object$dataset$usageCitation$title
  if (is.null(doi)) {
    cat(crayon::red$bold("Warning: "),
    "You have not specified a DRR associated with this data package.
    If you have an associated DRR, specify its DOI using ",
    crayon::green$bold("set_drr()"), ".", sep="")
    drr_doi <- NA # to do: test whether NA needs quotes for write.README.
  } else {
    return(doi)
  }
}


#' Get literature cited
#'
#' @description get_lit prints bibtex fromated literature cited to the screen.
#'
#' @details get_lit currently only supports bibtex formatted references. get_lit gets items from the <literatureCited> tag and prints them to the screen.
#'
#' @inheritParams get_begin_date
#'
#' @return character string
#' @export
#'
#' @examples
#' \dontrun{
#' get_lit(eml_object)
#' }
get_lit <- function(eml_object) {
  lit <- get_eml_simple(eml_object, "literatureCited")
  return(lit)
}

#' Returns the Producing Units
#'
#' @description get_producing_units returns whatever is in the metadataProvider eml element. Set this to the producing units using the set_producing_units function.
#'
#' @inheritParams get_begin_date
#'
#' @return a character sting
#' @export
#'
#' @examples
#' \dontrun{
#' get_producing_units(eml_object)
#' }
get_producing_units <- function(eml_object) {
  punit <- get_eml_simple(eml_object, "metadataProvider")
  return(punit)
}

#' Returns the publisher information
#'
#' @description `get_publisher()` returns a list that includes all the information about the publisher stored in EML.
#'
#' @inheritParams get_begin_date
#'
#' @return List.
#' @export
#'
#' @examples
#' \dontrun{
#' get_publisher(eml_object)
#' }
get_publisher <- function(eml_object) {
  pub <- eml_object$dataset$publisher
}



#' Creates attribute tables from an EML object
#'
#' @description `get_attribute_tables` takes an EML object and returns a nested table of
#' all the attribute tables pulled from the metadata, using EML::get_attributes()

#' @param eml_object is an R object imported (typically from an EML-formatted .xml file)
#' using EML::read_eml(<filename>, from="xml").
#' @returns a nested table with one attribute table for each data table in the EML file
#' @export
#' @examples
#' \dontrun{
#' get_attribute_tables(example_EML)
#' }

get_attribute_tables <- function(eml_object) {

  # create empty attributes table
  attributes <- NULL

  # data packages with multiple tables
  if (is.null(eml_object$dataset$dataTable$attributeList)) {
    for (i in seq_along(eml_object$dataset$dataTable)) {
      # get table name
      table_name <- stringr::str_sub(eml_object$dataset$dataTable[[i]]$physical$objectName, 1, -5)
      # use EML function to get attributes
      attr_temp <- suppressMessages(EML::get_attributes(eml_object$dataset$dataTable[[i]]$attributeList)$attributes)
      attr_temp_cleaned <- attr_temp
      # add a unit column if none exists
      if (is.null(attr_temp$unit)) {
        attr_temp_cleaned <- attr_temp_cleaned %>%
          dplyr::mutate(unit = "")
      }
      # add a formatString column if none exists
      if (is.null(attr_temp$formatString)) {
        attr_temp_cleaned <- attr_temp_cleaned %>%
          dplyr::mutate(formatString = "")
      }
      # add a missingValueCode column if none exists
      if (is.null(attr_temp$missingValueCode)) {
        attr_temp_cleaned <- attr_temp_cleaned %>%
          dplyr::mutate(missingValueCode = "")
      }
      # add a missingValueCodeExplanation column if none exists
      if (is.null(attr_temp$missingValueCodeExplanation)) {
        attr_temp_cleaned <- attr_temp_cleaned %>%
          dplyr::mutate(missingValueCodeExplanation = "")
      }
      # add a class column
      # rename dttm format column
      attr_temp_cleaned <- attr_temp_cleaned %>%
        dplyr::mutate(class = dplyr::case_when(storageType %in% c("float", "double", "long", "int") ~ "numeric",
                                               (storageType == "string" & domain == "textDomain") ~ "character",
                                               (storageType == "string" & domain == "enumeratedDomain") ~ "categorical",
                                               storageType == "date" ~ "Date",
                                               TRUE ~ "")) %>%
        dplyr::rename(dateTimeFormatString = formatString) %>%
        dplyr::select(attributeName, attributeDefinition, class, unit, dateTimeFormatString, missingValueCode, missingValueCodeExplanation)
      # replace NAs with blanks
      attr_temp_cleaned[is.na(attr_temp_cleaned)] <- ""
      # add attributes table into nested table
      attributes[[table_name]] <- attr_temp_cleaned
    }
  } # data packages with just one table
  else {
    # get table name
    table_name <- stringr::str_sub(eml_object$dataset$dataTable$physical$objectName, 1, -5)
    # use EML function to get attributes
    attr_temp <- suppressMessages(EML::get_attributes(eml_object$dataset$dataTable$attributeList)$attributes)
    attr_temp_cleaned <- attr_temp
    # add a unit column if none exists
    if (is.null(attr_temp$unit)) {
      attr_temp_cleaned <- attr_temp_cleaned %>%
        dplyr::mutate(unit = "")
    }
    # add a formatString column if none exists
    if (is.null(attr_temp$formatString)) {
      attr_temp_cleaned <- attr_temp_cleaned %>%
        dplyr::mutate(formatString = "")
    }
    # add a missingValueCode column if none exists
    if (is.null(attr_temp$missingValueCode)) {
      attr_temp_cleaned <- attr_temp_cleaned %>%
        dplyr::mutate(missingValueCode = "")
    }
    # add a missingValueCodeExplanation column if none exists
    if (is.null(attr_temp$missingValueCodeExplanation)) {
      attr_temp_cleaned <- attr_temp_cleaned %>%
        dplyr::mutate(missingValueCodeExplanation = "")
    }
    # add a class column
    # rename dttm format column
    attr_temp_cleaned <- attr_temp_cleaned %>%
      dplyr::mutate(class = dplyr::case_when(storageType %in% c("float", "double", "long", "int") ~ "numeric",
                                             (storageType == "string" & domain == "textDomain") ~ "character",
                                             (storageType == "string" & domain == "enumeratedDomain") ~ "categorical",
                                             storageType == "date" ~ "Date",
                                             TRUE ~ "")) %>%
      dplyr::rename(dateTimeFormatString = formatString) %>%
      dplyr::select(attributeName, attributeDefinition, class, unit, dateTimeFormatString, missingValueCode, missingValueCodeExplanation)
    # replace NAs with blanks
    attr_temp_cleaned[is.na(attr_temp_cleaned)] <- ""
    # add attributes table into nested table
    attributes[[table_name]] <- attr_temp_cleaned
  }
  return(attributes)
}

#' Writes attribute tables from an EML object as text files
#'
#' @description `write_attribute_tables` is a wrapper around get_attribute_tables().
#' It takes an EML object and writes one attribute table for each data table in the EML file.
#' It writes the attribute tables as .txt files in the working directory or a specified path.
#' This function is useful for recreating metadata with EMLassemblyline, when the
#' user doesn't have access to the original text files.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file)
#' using EML::read_eml(<filename>, from="xml").
#' @param path default to the working directory. This path determines where the txt
#' files will be written.
#' @export
#' @examples
#' \dontrun{
#' write_attribute_tables(example_EML)
#' }

write_attribute_tables <- function(eml_object, path = here::here()) {
  attribute_table <- get_attribute_tables(eml_object)
  for (i in seq_along(attribute_table)) {
    readr::write_tsv(attribute_table[[i]], paste0(path,
                                             "/",
                                             "attributes_",
                                             names(attribute_table)[[i]], ".txt"))
  }
}


#' Creates categorical variable tables from an EML object
#'
#' @description `get_catvar_tables` takes an EML object and returns a nested table of
#' all the categorical variable tables pulled from the metadata, using EML::get_attributes()
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file)
#' using EML::read_eml(<filename>, from="xml").
#' @returns a nested table with one catvar table for each data table in the EML file
#' @export
#' @examples
#' \dontrun{
#' get_catvar_tables(example_EML)
#' }

get_catvar_tables <- function(eml_object) {

  # create empty attributes table
  catvars <- NULL

  # data packages with multiple tables
  if (is.null(eml_object$dataset$dataTable$attributeList)) {
    for (i in seq_along(eml_object$dataset$dataTable)) {
      # get table name
      table_name <- stringr::str_sub(eml_object$dataset$dataTable[[i]]$physical$objectName, 1, -5)
      # use EML function to get attributes
      catvars_temp <- suppressMessages(EML::get_attributes(eml_object$dataset$dataTable[[i]]$attributeList)$factors)
      # print message if there are no categorical variables
      if (is.null(catvars_temp)) {
        table_name <- eml_object$dataset$dataTable[[i]]$physical$objectName
        message(paste0("No categorical variables found for ", table_name))
      } else {
        # select necessary columns
        catvars_temp_cleaned <- catvars_temp %>%
          dplyr::select(attributeName, code, definition)
        # add attributes table into nested table
        catvars[[table_name]] <- catvars_temp_cleaned
      }
    }
  } # data packages with just one table
  else {
    # get table name
    table_name <- stringr::str_sub(eml_object$dataset$dataTable$physical$objectName, 1, -5)
    # use EML function to get attributes
    catvars_temp <- suppressMessages(EML::get_attributes(eml_object$dataset$dataTable$attributeList)$factors)
    # print message if there are no categorical variables
    if (is.null(catvars_temp)) {
      table_name <- eml_object$dataset$dataTable$physical$objectName
      message(paste0("No categorical variables found for ", table_name))
    } else {
      # select necessary columns
      catvars_temp_cleaned <- catvars_temp %>%
        dplyr::select(attributeName, code, definition)
      # add attributes table into nested table
      catvars[[table_name]] <- catvars_temp_cleaned
    }
  }
  return(catvars)
}

#' Writes categorical variable tables from an EML object as text files
#'
#' @description `write_catvar_tables` is a wrapper around get_catvar_tables().
#' It takes an EML object and writes one categorical variable table for each data table
#' in the EML file. It writes the attribute tables as .txt files in the working directory
#' or a specified path. This function is useful for recreating metadata with EMLassemblyline,
#' when the user doesn't have access to the original text files.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file)
#'  using EML::read_eml(<filename>, from="xml").
#' @param path default to the working directory. This path determines where the
#' txt files will be written.
#' @export
#' @examples
#' \dontrun{
#' write_catvar_tables(example_EML)
#' }

write_catvar_tables <- function(eml_object, path = here::here()) {
  catvar_table <- get_catvar_tables(eml_object)
  for (i in seq_along(catvar_table)) {
    readr::write_tsv(catvar_table[[i]], paste0(path,
                                          "/",
                                          "catvars_",
                                          names(catvar_table)[[i]], ".txt"))
  }
}

