#' Initiates a draft reference and inserts the reserved DOI into metadata
#'
#' @description `set_datastore_doi()` differs from `set_doi()` in that this function generates a draft reference on DataStore and uses that draft reference to auto-populate the DOI within metadata whereas the latter requires manually initiating a draft reference in DataStore and providing the reference ID to insert the DOI into metadata.
#'
#' @details To prevent generating too many (unused) draft references, `set_datastore_doi()` checks your metadata contents prior to initiating a draft reference on DataStore. If you already have a DOI specified, it will ask if you really want to over-write the DOI **and** initiate a new draft reference. Setting force = TRUE will over-ride this aspect of the function, so use with care. the `set_datastore_doi()` function requires that your metadata already contain a data package title and if it is missing will prompt you to insert it and quit. Setting force = TRUE will not override this check. If R cannot successfully initiate a draft reference on DataStore, the function will remind you to log on to the VPN. If the problem persists, email [NRSS_DataStore@nps.gov ](mailto:NRSS_DataStore@nps.gov ).
#'
#' @details This function generates a draft reference on DataStore. If you run with force = FALSE (default), the function will report the draft reference URL and the draft title for the draft reference. Make sure you upload your data and metadata to the correct draft reference! Your draft reference title should read: "DRAFT: <your data package title>". This will be updated to your data package title when you upload your metadata.
#'
#' If you set a new DOI with `set_datastore_doi()`, it will also update all the links within the metadata to the data files to reflect the new draft reference and DataStore location. If you didn't have links to your data files, `set_datastore_doi()` will add them - but only if you actually update the doi.
#'
#' If you would like to test out the function without making excess references on DataStore, set the parameter dev=TRUE. That will re-route all of the API requests to the development server. You must be logged in to the VPN to access irmadev.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#'
#' @param force logical. Defaults to false. If set to FALSE, a more interactive version of the function requesting user input and feedback. Setting force = TRUE facilitates scripting.
#'
#' @param NPS Logical. Defaults to TRUE. **Most users should leave this as the default**. Only under specific circumstances should it be set to FALSE: if you are **not** publishing with NPS, if you need to set the publisher location to some place other than the Fort Collins Office (e.g. you are NOT working on a data package) or your product is "for" the NPS by not "by" the NPS and you need to specify a different agency, set NPS = FALSE. When NPS=TRUE, the function will over-write existing publisher info and inject NPS as the publisher along the the Central Office in Fort Collins as the location. Additionally, it sets the "for or by NPS" field to TRUE and specifies the originating agency as NPS.
#'
#' @param dev Logical. Defaults to FALSE. When dev = TRUE, all api actions are re-routed to the development server (as opposed to when dev = FALSE when api actions will target the production server).
#'
#' @return an EML-formatted R object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_datastore_doi(eml_object)
#' }
set_datastore_doi <- function(eml_object,
                              force = FALSE,
                              NPS = TRUE,
                              dev = FALSE){
  # check for existing DOI:
  doc <- utils::capture.output(EMLeditor::get_doi(eml_object))
  #get data package title from metadata:
  data_package_title <- EMLeditor::get_title(eml_object)
  if(force == FALSE){
    # if there is NOT an existing DOI in metadata:
    if(length(seq_along(doc)) > 1 ){
      cat("Your metadata does not have a previously specified DOI.\n",
          sep = "")
      response <- paste0("Are you sure you want to create a new draft",
                         " reference on DataStore and insert the",
                         " corresponding DOI into your metadata?\n")
      cat(response)
      var1 <- .get_user_input() #1 = yes, 2 = no
      if (var1 == 2){
        response <- paste0("Function terminated. You have not created a new",
                          " draft reference on DataStore and a DOI has not",
                          " been added to your metadata.\n")
        cat(response)
        return()
      }
    }
    # if there is an existing DOI in the metadata:
    if(length(seq_along(doc)) == 1){
      #get Datastore Reference ID:
      DS_ref <- get_ds_id(eml_object)
      url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", DS_ref)
      #API call to look for an existing reference:
      test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
      status_code <- httr::stop_for_status(test_req)$status_code

      #if API call fails, alert user and remind them to log on to VPN:
      if(!status_code == 200){
        stop("DataStore connection failed.")
      }

      test_json <- httr::content(test_req, "text")
      test_rjson <- jsonlite::fromJSON(test_json)

      #tell user their current DOI:
      cat("The current data package DOI in your metadata is:\n",
          crayon::blue$bold("https://doi.org/10.57830/", DS_ref, sep = ""),
          ".\n\n", sep = "")
      #if API search did not find a corresponding reference on DataStore:
      if(length(test_rjson) == 0){
        cat("However, there is no draft reference associated with this DOI on DataStore.\n")
      }
      #if there is already a reference associated with this DOI on DataStore:
      if(length(test_rjson > 0)){
        cat("You already have a draft reference for this data package on DataStore.\n")
        cat("The existing DataStore draft reference ID is:\n",
            crayon::blue$bold(test_rjson$referenceId), ".\n", sep = "")
        cat("The existing DataStore draft reference title is:\n",
            crayon::blue$bold(test_rjson$title), ".\n", sep = "")
        cat("The existing DataStore reference was created on:\n",
            crayon::blue$bold(substr(test_rjson$dateOfIssue, 1, 10)),
            ".\n\n", sep = "")
      }
      #Ask if they really want a new DOI & new draft reference?
      cat("Are you sure you want to create a new draft reference on DataStore and insert the corresponding DOI into your metadata?\n")
      var1 <- .get_user_input() # 1 = yes, 2 = no
      # if chooses not to add a new doi/generate a new draft reference:
      if (var1 == 2){
        cat("Function terminated. You have not created a new draft reference on DataStore and your original DOI has been retained.")
        return()
      }
    }
  }
  # enforce existence of a title prior to proceeding with DOI, regardless of force=FALSE or force=TRUE (could change this: it doesn't really need a title for DataStore, it's just to help the user keep track of draft references in DataStore):
  if(is.null(data_package_title)){
    cat("Your data package does not have a title.\n")
    cat("Use ", crayon::green$bold("set_title()"),
        "to set the title before adding your DOI.\n", sep = "")
    stop()
  }
  #generate draft title:
  dynamic_title <- paste0("[DRAFT]: ", data_package_title)
  #generate json body for rest api call:
  mylist <- list(referenceTypeId = "dataPackage",
                 title = dynamic_title,
                 location = "",
                 issuedDate = list(year = 0,
                                   month = 0,
                                   day = 0,
                                   precision = ""))
  bdy <- jsonlite::toJSON(mylist, pretty = TRUE, auto_unbox = TRUE)
  #Create empty draft reference:
  if(dev == TRUE){
    post_url <- paste0(.ds_dev_api(), "Reference/CreateDraft")
  } else {
    post_url <- paste0(.ds_secure_api(), "Reference/CreateDraft")
  }
  req <- httr::POST(post_url,
                    httr::authenticate(":", "", "ntlm"),
                    httr::add_headers('Content-Type'='application/json'),
                    body = bdy)
  #check status code; suggest logging in to VPN if errors occur:
  status_code <- httr::stop_for_status(req)$status_code
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  #get draft reference code:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  ds_ref <- rjson$referenceCode
  # insert/replace DOI:
  if(dev == TRUE){
    eml_object$dataset$alternateIdentifier <- paste0(
      "doi: https://doi.org/10.82495/",
      ds_ref)
  } else {
  eml_object$dataset$alternateIdentifier <- paste0(
    "doi: https://doi.org/10.57830/",
    ds_ref)
  }
  # update data URLs to correspond to new DOI:
  data_table <- EML::eml_get(eml_object, "dataTable")
  data_table <- within(data_table, rm("@context"))
  if(dev == TRUE){
    data_url <- paste0("https://irmadev.nps.gov/DataStore/Reference/Profile/",
                       ds_ref)
  } else {
    data_url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                     ds_ref)
  }
  # handle case when there is only one data table:
  if("physical" %in% names(data_table)){
    eml_object$dataset$dataTable$physical$distribution$online$url <- data_url
  }
  # handle case when there is only one data table:
  else {
    for(i in seq_along(data_table)){
      eml_object$dataset$dataTable[[i]]$physical$distribution$online$url <-
        data_url
    }
  }
  if(force == FALSE){
    #print DOI to screen
    doc1 <- eml_object$dataset$alternateIdentifier
    doc1 <- sub(".*? ", "", doc1)
    cat("Your newly specified DOI is: ", crayon::blue$bold(doc1), ".\n",sep = "")
    cat("You can check on your draft reference at:\n")
    cat(crayon::blue$bold(data_url), "\n")
    cat("Your draft title is:\n")
    cat(crayon::blue$bold(dynamic_title), "\n")
    cat("Your draft title will be updated upon metadata upload.")
  }
  # Set NPS publisher, if it doesn't already exist
  if (NPS == TRUE) {
    eml_object <- .set_npspublisher(eml_object)
  }
  # add/update EMLeditor and version to metadata:
  eml_object <- .set_version(eml_object)
}

#' Upload a data package to DataStore
#'
#' @description `upload_data_package()` inspects a data package and, if a DOI is supplied in the metadata, uploads the data files and metadata to the appropriate reference on DataStore. This function requires that you are logged on to the VPN. `upload_data_package()` will only work if each individual file in the data package is less than 32Mb. Larger files still require manual upload via the DataStore web interface. `upload_data_package()` just uploads files. It does not extract EML metadata to populate the reference fields on DataStore and it does not activate the reference - the reference remains fully editable via the web interface. After using `upload_data_package()` you do not need to "save" the reference on DataStore; the files are automatically saved to the reference.
#'
#' @details Currently, only .csv data files and EML metadata files are supported. All .csvs must end in ".csv". The single metadata file must end in "_metadata.xml". If you have included a DOI in your metadata, using `upload_data_package()` is preferable to using the web interface to manually upload files because it insures that your files are uploaded to the correct reference (i.e. the DOI in your metadata corresponds to the draft reference code on DataStore).
#'
#' Once uploaded, you are advised to look at the 'Files and Links' tab on the DataStore web interface to make sure your files are there and you do not have any duplicates. You can delete files as necessary from the 'Files and Links' tab until the reference is activated.
#'
#' This function is primarily intended for uploading files to the data package reference type on DataStore, but will upload .csvs and a single EML metadata file saved as *_metadata.xml file to any reference type, assuming the metadata has a DOI listed in the expected location and there is a corresponding draft reference on DataStore.
#'
#' Due to a known bug in the DataStore API, you can only use this function to upload files once. If you need to replace files, you must do that through the DataStore GUI.
#'
#' #' If you would like to test out the function without making uploading to DataStore, set the parameter dev=TRUE. That will re-route all of the API requests to the development server. You must be logged in to the VPN to access irmadev and you must have a draft reference on the dev server (using set_datastore_doi() with dev=TRUE).
#'
#' @param directory the location (path) to your data package files
#'
#' @param force logical, defaults to FALSE for a verbose interactive version. Set to TRUE to suppress interactions and facilitate scripting.
#'
#' @param dev Logical. Defaults to FALSE. When dev = TRUE, all api actions are re-routed to the development server (as opposed to when dev = FALSE when api actions will target the production server).
#'
#' @return invisible(NULL)
#' @export
#'
#' @examples
#'  \dontrun{
#' dir <- here::here("..", "Downloads", "BICY")
#' upload_data_package(dir)
#' }
upload_data_package <- function(directory = here::here(),
                                force = FALSE,
                                dev = FALSE){

  # get list of all files ending in metadata.xml
  lf <- list.files(path = directory,
                   pattern = "metadata.xml",
                   ignore.case = TRUE)

  metadata_file <- file.path(directory, lf)

  if (length(metadata_file) == 1) {
    metadata <- EML::read_eml(metadata_file, from = "xml")
    if (is.null(metadata)) {
      cli::cli_abort(c("x" = "Could not load metadata."))
    }
  } else if (length(metadata_file) < 1) {
    # if no metadata file exists, stop the function and warn the user
    cli::cli_abort(c
                   ("x" = "No metadata found. Your metadata file name must end in {.file _metadata.xml}."))
  } else if (length(metadata_file) > 1) {
    # if multiple metadata files exist, stop the function and warn the user
    cli::cli_abort(c("x" = "The data package format only allows one metadata file per data package. Please remove extraneous metadata files or combine them into a single file."))
  }

  #get doi from metadata
  doi <- get_doi(metadata)
  #extract DS reference - assumes 7 digit DS reference code:
  DS_ref <- stringr::str_sub(doi,-7,-1)
  #list files in data package

  #test whether reference already exists or the DOI:
  if (dev == TRUE) {
    url <- paste0(.ds_dev_api(), "ReferenceCodeSearch?q=", DS_ref)
  } else {
    url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", DS_ref)
  }
  #verbose approach:
  if (force == FALSE) {
    #API call to look for an existing reference:
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code <- httr::stop_for_status(test_req)$status_code

    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
    }
    test_json <- httr::content(test_req, "text")
    test_rjson <- jsonlite::fromJSON(test_json)
    if (length(test_rjson) > 0) {
      cat("A draft reference for this data package exists on DataStore.\n")
      cat("The existing DataStore draft reference ID is:\n",
          crayon::blue$bold(test_rjson$referenceId), ".\n", sep = "")
      cat("The existing DataStore draft reference title is:\n",
          crayon::blue$bold(test_rjson$title), ".\n", sep = "")
      cat("The existing DataStore reference was created on:\n",
          crayon::blue$bold(substr(test_rjson$dateOfIssue, 1, 10)),
          ".\n\n", sep = "")
      if (test_rjson$fileCount > 0) {
        cat("The existing Reference already has files attached. ")
        cat("Use EMLeditor::remove_datastore_files() before uploading new files.")
        return()
      }
    }
    if (length(test_rjson) == 0) {
      cat("There is no draft reference on DataStore corresponding to your metadata DOI to upload your files to.\n")
      cat("Please use ", crayon::bold$green("set_datastore_doi()"),
          " to create a draft reference and insert the corresponding DOI into your metadata prior to uploading files.", sep = "")
      return()
    }
    cat("Are you sure you want to upload your data package files to this reference?\n")
    var1 <- .get_user_input() # 1 = yes, 2 = no
    if (var1 == 2) {
      cat("Function terminated. You have not uploaded any files to DataStore.")
    }
    if (var1 == 1) {
      #check for DOI & referenceId mismatch...this should never happen.
      if (!DS_ref == test_rjson$referenceId) {
        cat("The DOI in your metadata, ", crayon::blue$bold(doi),
            ", does not match the reference ID, ",
            crayon::blue$bold(test_rjson$referenceId), ".\n", sep = "")
        cat("Your files were not uploaded.\n")
        return()
      }
      #if DOI and referenceId match:
      else {
        #get list of files for terminal output (just names, not paths)
        files_names <- list.files(path = directory,
                                  pattern = "*\\.csv",
                                  ignore.case = TRUE)
        #add metadata
        files_names <- append(files_names,
                              list.files(path = directory,
                                         pattern = "*metadata.xml"))
        #get list of .csvs for upload (names and paths)
        files <- list.files(path = directory,
                            pattern = "*\\.csv",
                            full.names = TRUE,
                            ignore.case = TRUE)
        #add metadata
        files <- append(files,
                        list.files(path = directory,
                                   pattern = "*metadata.xml",
                                   full.names = TRUE))
        for (i in seq_along(files)) {
          #test for files <32Mb:
          file_size_error <- NULL
          if (file.size(files[i]) > 33554432) {
            #warn for each file >32Mb
            cat(crayon::blue$bold(files_names[i]),
                "is greater than 32Mb and cannot be uploaded with this funcion.\n",
                sep = "")
            file_size_error <- 1
          }
        }
        # stop if any files >32Mb
        if (!is.null(file_size_error)) {
          stop()
        }
        if (is.null(file_size_error)) {
          if (dev == TRUE) {
            api_url <- paste0(.ds_dev_api(),
                              "Reference/", DS_ref, "/UploadFile")
          } else {
            api_url <- paste0(.ds_secure_api(),
                              "Reference/", DS_ref, "/UploadFile")
          }
          #upload the files
          for (i in seq_along(files)) {
            req <- httr::POST(
              url = api_url,
              httr::add_headers('Content-Type' = 'multipart/form-data'),
              httr::authenticate(":", "", "ntlm"),
              body = list(addressFile = httr::upload_file(files[i])),
              encode = "multipart",
              httr::progress(type = "up", con = ""))
            status_code <- httr::stop_for_status(req)$status_code
            if (status_code != 201) {
              x <- "DataStore Connection failed.
                    Your file was not successfully uploaded."
              x <- strwrap(x, width = 10000, simplify = TRUE)
              stop(x)
            }
            else {
              cat("Your file, ", crayon::blue$bold(files_names[i]),
                  ", has been uploaded to:\n", sep = "")
              cat(req$headers$location, "\n", sep="")
            }
          }
        }
      }
    }
  }
  #suppress interactive/verbose portions and facilitate scripting:
  if (force == TRUE) {
    #API call to look for an existing reference:
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code <- httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if (!status_code == 200) {
      stop("DataStore connection failed. Are you logged in to the VPN?")
    }
    test_json <- httr::content(test_req, "text")
    test_rjson <- jsonlite::fromJSON(test_json)
    if (length(test_rjson) == 0) {
      x <- "There is no draft reference on DataStore corresponding to your
            metadata DOI to upload your files to.\n"
      x <- strwrap(x, width = 10000, simplify = TRUE)
      cat(x)
      return()
    }
    if (test_rjson$fileCount > 0) {
      x <- "The existing reference already has files uploaded.
            To add, remove, or change files please use the DataStore GUI.\n"
      x <- strwrap(x, width = 10000, simplify = TRUE)
      cat(x)
      return(invisible())
    }
    #test that metadata DOI corresponds to the draft reference on DataStore
    #this test should never fail.
    if (!DS_ref == test_rjson$referenceId) {
      cat("The DOI in your metadata, ", crayon::blue$bold(doi),
          ", does not match the reference ID, ",
          crayon::blue$bold(test_rjson$referenceId), ".\n", sep = "")
      cat("Your files were not uploaded.\n")
      return()
    }
    #get list of .csvs
    files <- list.files(path = directory,
                        pattern = "*\\.csv",
                        full.names = TRUE,
                        ignore.case = TRUE)
    #add metadata
    files <- append(files,
                    list.files(path = directory,
                               pattern = "*metadata.xml",
                               full.names = TRUE))
    for (i in seq_along(files)) {
      #test for files <32Mb:
      file_size_error <- NULL
      if (file.size(files[i]) > 33554432) {
        #warn for each file <32Mb
        cat(crayon::blue$bold(files[i]),
            "is greater than 32Mb and cannot be uploaded with this funcion.\n",
            sep = "")
        file_size_error <- 1
      }
    }
    # stop if any files >32Mb
    if (!is.null(file_size_error)) {
      stop()
    }
    if (is.null(file_size_error)) {
      if (dev == TRUE) {
        api_url <- paste0(.ds_dev_api(), "Reference/", DS_ref, "/UploadFile")
      } else {
        api_url <- paste0(.ds_secure_api(), "Reference/", DS_ref, "/UploadFile")
      }
      #upload the files
      for (i in seq_along(files)) {
        req <- httr::POST(
          url = api_url,
          httr::add_headers('Content-Type' = 'multipart/form-data'),
          httr::authenticate(":", "", "ntlm"),
          body = list(addressFile = httr::upload_file(files[i])),
          encode = "multipart")
        status_code <- httr::stop_for_status(req)$status_code
        if (status_code != 201) {
          x <- "DataStore connection failed.
                Your file was not successfully uploaded."
          x <- strwrap(x, width = 10000, simplify = TRUE)
          stop(x)
        }
      }
    }
  }
}


#' Remove files from a data package Reference on DataStore
#'
#' @description The `remove_datastore_files()` function requires that you are logged in to the VPN. You must also have permissions to edit the DataStore Reference in question, and the Reference must not be activated. In that case, `remove_datastore_files()` detaches all files associated with the relevant DataStore Reference. Once the files are detached, they cannot be re-attached.  Instead, updated files can be re-uploaded and attached to the reference via `upload_data_package()`.
#'
#' In the interactive mode (force = FALSE), you will be informed of the Reference number, Reference title, Reference creation date, and a list of files currently attached to the Reference. Once the files are successfully detached, you will be informed of a list of file names that were detached.
#'
#' Note that `remove_datastore_files()` is intended to be used with the data package Reference type on DataStore but in theory will allow you to remove files from ANY reference type.
#'
#' @param data_store_reference Integer. the 7-digit DataStore Reference number
#' @param force Logical. Defaults to FALSE. Set to TRUE to avoid interactive components and most user feedback.
#' @param dev Logical. Defaults to FALSE. set to TRUE if you want to detete files from a DataStore reference on the Development server.
#'
#' @return NULL
#' @export
#'
#' @examples
#' \dontrun{
#' remove_datastore_files(1234567)
#' remove_datastore_files(1234567, force = TRUE, dev = TRUE)
#' }
remove_datastore_files <- function(data_store_reference,
                                   force = FALSE,
                                   dev = FALSE) {
  DS_ref <- data_store_reference
  #list files in data package

  #test whether reference already exists or the DOI:
  if(dev == TRUE){
    url <- paste0(.ds_dev_api(), "ReferenceCodeSearch?q=", DS_ref)
  } else {
    url <- paste0(.ds_secure_api(), "ReferenceCodeSearch?q=", DS_ref)
  }
  #API call to look for an existing reference:
  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(test_req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if (!status_code == 200) {
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  test_json <- httr::content(test_req, "text")
  test_rjson <- jsonlite::fromJSON(test_json)

  #if the Reference doesn't appear to exist:
  if (length(test_rjson) == 0) {
    if (force == FALSE) {
      long_text <- "Please double check the
                    DataStore Reference number you provided."
      long_text <- strwrap(long_text, width = 10000, simplify = TRUE)
      cat("Reference ", crayon::red$bold(DS_ref), " does not exist. ",
          long_text, sep = "")
    }
    return()
  }
  #if there are no files already associated with the reference:
  if (length(test_rjson) > 0) {
    if (force == FALSE) {
      cat("A draft reference for this data package exists on DataStore.\n")
      cat("The existing DataStore draft reference ID is:\n",
        crayon::blue$bold(test_rjson$referenceId), ".\n", sep = "")
      cat("The existing DataStore draft reference title is:\n",
        crayon::blue$bold(test_rjson$title), ".\n", sep = "")
      cat("The existing DataStore reference was created on:\n",
        crayon::blue$bold(substr(test_rjson$dateOfIssue, 1, 10)),
        ".\n\n", sep = "")
      if (test_rjson$fileCount == 0) {
        if (force == FALSE) {
          cat("The existing Reference does not have files attached.")
          cat("Use EMLeditor::upload_data_package() to upload files.")
        }
        return(invisible())
      }
    }

  }

  #get DataStore Reference Resources (files)
  if(dev == TRUE){
    url <- paste0(.ds_dev_api(), "Reference/", DS_ref, "/DigitalFiles")
  } else {
    url <- paste0(.ds_secure_api(), "Reference/", DS_ref, "/DigitalFiles")
  }
  test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
  status_code <- httr::stop_for_status(test_req)$status_code

  #if API call fails, alert user and remind them to log on to VPN:
  if(!status_code == 200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }
  test_json <- httr::content(test_req, "text")
  test_rjson <- jsonlite::fromJSON(test_json)
  # get list of attached files:
  resource_id <- test_rjson$resourceId

  #if there ARE files already associated with the reference:
  if (force == FALSE) {
    cat("The existing Reference already has the following files attached:\n")
    for (i in seq_along(resource_id)) {
      cat(crayon::bold$blue(test_rjson$fileName[i]), "\n")
    }
    x <- "Would you like to remove ALL of the attached files from the
        Reference?"
    x <- strwrap(x, width = 10000, simplify = TRUE)
    cat(x)
    var1 <- .get_user_input() # 1 = yes, 2 = no
    if (var1 == 2) {
      cat("You have not removed any files from the Reference.")
      return(invisible())
    }
  }

  #construct delete request urls
  for (i in seq_along(resource_id)) {
    if (dev == TRUE) {
      api_url <- paste0(.ds_dev_api(), "Reference/", DS_ref,
                    "/DigitalFiles/", resource_id[i])
    } else {
      api_url <- paste0(.ds_secure_api(), "Reference/", DS_ref,
                    "/DigitalFiles/", resource_id[i])
    }
    #delete each file
    deleted_file <- NULL
    req <- httr::DELETE(
        url = api_url,
        httr::add_headers('Content-Type' = 'multipart/form-data'),
        httr::authenticate(":", "", "ntlm"),
        encode = "multipart")
    #check status code
    status_code <- req$status_code
    #return status code error
    if (status_code == 500) {
      x <- paste0("Internal Server Error 500. Is this Reference ",
                  DS_ref,
                  " already activated?")
      stop(x)
    }
    if (status_code != 200) {
      err <- "DataStore connection failed.
              Your file(s) were not successfully removed."
      err <- strwrap(err, width = 10000, simplify = TRUE)

      stop(err)
    }
  }
  #report on deleted files if verbose
  if (force == FALSE) {
    cat("The following files have been removed from Reference ",
        DS_ref, ":\n", sep = "")
    for (i in seq_along(resource_id)) {
      cat(crayon::bold$blue(test_rjson$fileName[i]), "\n")
    }
  }
  return(invisible())
}
