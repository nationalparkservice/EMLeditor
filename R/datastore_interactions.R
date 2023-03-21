#' Initiates a draft reference and inserts the reserved DOI into metadata
#'
#' @description `set_datastore_doi()` differs from `set_doi()` in that this function generates a draft reference on DataStore and uses that draft reference to auto-populate the DOI within metadata whereas the later requires manually initiating a draft reference in DataStore and providing the reference ID to insert the DOI into metadata.
#'
#' @details To prevent generating too many (unused) draft references, `set_datastore_doi()` checks your metadata contents prior to initiating a draft reference on DataStore. If you already have a DOI specified, it will ask if you really want to over-write the DOI **and** initiate a new draft reference. Setting force = TRUE will over-ride this aspect of the function, so use with care. the `set_datastore_doi()` function requires that your metadata already contain a data package title and if it is missing will prompt you to insert it and quit. Setting force = TRUE will not override this check. If R cannot successfully initiate a draft reference on DataStore, the function will remind you to log on to the VPN. If the problem persists, email irma@nps.gov.
#'
#' @details This function generates a draft reference on DataStore. If you run with force = FALSE (default), the function will report the draft reference URL and the draft title for the draft reference. Make sure you upload your data and metadata to the correct draft reference! Your draft reference title should read: "DRAFT: <your data package title>". This will be updated to your data package title when you upload your metadata.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#'
#' @param force logical. Defaults to false. If set to FALSE, a more interactive version of the function requesting user input and feedback. Setting force = TRUE facilitates scripting.
#'
#' @param NPS Logical. Defaults to TRUE. **Most users should leave this as the default**. Only under specific circumstances should it be set to FALSE: if you are **not** publishing with NPS, if you need to set the publisher location to some place other than the Fort Collins Office (e.g. you are NOT working on a data package) or your product is "for" the NPS by not "by" the NPS and you need to specify a different agency, set NPS = FALSE. When NPS=TRUE, the function will over-write existing publisher info and inject NPS as the publisher along the the Central Office in Fort Collins as the location. Additionally, it sets the "for or by NPS" field to TRUE and specifies the originating agency as NPS.
#'
#' @return an EML-formatted R object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' eml_object <- set_datastore_doi(eml_object)
#' }
set_datastore_doi <- function(eml_object, force=FALSE, NPS=TRUE){
  # check for existing DOI:
  doc<- get_doi(eml_object)
  #get data package title from metadata:
  data_package_title <- EMLeditor::get_title(eml_object)
  if(force == FALSE){
    #if there is an existing DOI in the metadata:
    if(!is.null(doc)){
      #get Datastore Reference ID:
      DS_ref <- get_ds_id(eml_object)
      url <- paste0("https://irmaservices.nps.gov/datastore-secure/v4/rest/ReferenceCodeSearch?q=", DS_ref)
      #API call to look for an existing reference:
      test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
      status_code<-httr::stop_for_status(test_req)$status_code

      #if API call fails, alert user and remind them to log on to VPN:
      if(!status_code==200){
        stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
      }

      test_json <- httr::content(test_req, "text")
      test_rjson <- jsonlite::fromJSON(test_json)

      #tell user their current DOI:
      cat("The current data package DOI in your metadata is:\n",
          crayon::blue$bold(doc), ".\n\n", sep="")
      #if API search did not find a corresponding reference on DataStore:
      if(length(test_rjson) == 0){
        cat("However, there is no draft reference associated with this DOI on DataStore.\n")
      }
      #if there is already a reference associated with this DOI on DataStore:
      if(length(test_rjson > 0)){
        cat("You already have a draft reference for this data package on DataStore.\n")
        cat("The existing DataStore draft reference ID is:\n",
            crayon::blue$bold(test_rjson$referenceId), ".\n", sep="")
        cat("The existing DataStore draft reference title is:\n",
            crayon::blue$bold(test_rjson$title), ".\n", sep="")
        cat("The existing DataStore reference was created on:\n",
            crayon::blue$bold(substr(test_rjson$dateOfIssue, 1, 10)),
            ".\n\n", sep="")
      }
      #Ask if they really want a new DOI & new draft reference?
      cat("Are you sure you want to create a new draft reference on DataStore and insert the corresponding DOI into your metadata?\n")
      var1 <- readline(prompt = "1: Yes\n2: No\n")
      if (var1 == 2){
        cat("Function terminated. You have not created a new draft reference on DataStore and your original DOI has been retained.")
        return()
      }
    }
  }
  #enforce existence of a title prior to proceeding with DOI, regardless of force=FALSE or force=TRUE (could change this: it doesn't really need a title for DataStore, it's just to help the user keep track of draft references in DataStore):
  if(is.null(data_package_title)){
    cat("Your data package does not have a title.\n")
    cat("Use ", crayon::green$bold("set_title()"),
        "to set the title before adding your DOI.\n", sep="")
    stop()
  }

  #generate draft title:
  dynamic_title <- paste0("[DRAFT]: ", data_package_title)

  #generate json body for rest api call:
  mylist <- list(referenceTypeId="dataPackage",
                 title=dynamic_title,
                 location="string",
                 issuedDate=list(year=0, month=0, day=0, precision="string"))
  bdy<-jsonlite::toJSON(mylist, pretty=TRUE, auto_unbox=TRUE)

  #Create empty draft reference:
  req <- httr::POST("https://irmaservices.nps.gov/datastore-secure/v4/rest/Reference/CreateDraft",
                httr::authenticate(":", "", "ntlm"),
                httr::add_headers('Content-Type'='application/json'),
                body = bdy)

  #check status code; suggest logging in to VPN if errors occur:
  status_code<-httr::stop_for_status(req)$status_code
  if(!status_code==200){
    stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
  }

  #get draft reference code:
  json <- httr::content(req, "text")
  rjson <- jsonlite::fromJSON(json)
  ds_ref <- rjson$referenceCode

  # insert/replace DOI:
  eml_object$dataset$alternateIdentifier <- paste0(
      "doi: https://doi.org/10.57830/",
      ds_ref)

  if(force == FALSE){
    #print DOI to screen
    doc <- eml_object$dataset$alternateIdentifier
    doc <- sub(".*? ", "", doc)
    cat("Your newly specified DOI is: ", crayon::blue$bold(doc), ".\n",sep = "")

    #tell user location of draft reference:
    url <- paste0("https://irma.nps.gov/DataStore/Reference/Profile/",
                ds_ref)
    cat("You can check on your draft reference at:\n")
    cat(crayon::blue$bold(url), "\n")
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
#' @description `upload_data_package()` inspects a data package and, if a DOI is supplied in the metadata, uploads the data files and metadata to the appropriate reference on DataStore. This function requires that you are logged on to the VPN. `upload_data_package()` will only work if each individual file in the data package is less than 4Mb. Larger files still require manual upload via the DataStore web interface.
#'
#' @details Currently, only .csv data files and EML metadata files are supported. All .csvs must end in ".csv". The single metadata file must end in "_metadata.xml".
#'
#' This function is primarily intended for uploading files to the data package reference type on DataStore, but will upload .csvs and a .xml file to any reference type, assuming the metadata has a DOI listed in the expected location and there is a corresponding draft reference on DataStore.
#'
#' @param directory the location (path) to your data package files
#' @param force logical, defaults to FALSE for a verbose interactive version. Set to TRUE to suppress interactions and facilitate scripting.
#' @return
#' @export
#'
#' @examples
upload_data_package <- function(directory = here::here(), force = FALSE){
  #load metadata
  metadata <- DPchecker::load_metadata
  #get doi from metadata
  doi <- get_doi(metadata)
  #extract DS reference - assumes 7 digit DS reference code:
  DS_ref <- stringr::str_sub(doi,-7,-1)
  #list files in data package

  #test whether reference already exists or the DOI:
  url <- paste0("https://irmaservices.nps.gov/datastore-secure/v4/rest/ReferenceCodeSearch?q=", DS_ref)
  #verbose approach:
  if(force = FALSE){
    #API call to look for an existing reference:
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code<-httr::stop_for_status(test_req)$status_code

    #if API call fails, alert user and remind them to log on to VPN:
    if(!status_code==200){
      stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
    }
    test_json <- httr::content(test_req, "text")
    test_rjson <- jsonlite::fromJSON(test_json)
    if(length(test_rjson > 0)){
      cat("A draft reference for this data package exists on DataStore.\n")
      cat("The existing DataStore draft reference ID is:\n",
        crayon::blue$bold(test_rjson$referenceId), ".\n", sep="")
      cat("The existing DataStore draft reference title is:\n",
        crayon::blue$bold(test_rjson$title), ".\n", sep="")
      cat("The existing DataStore reference was created on:\n",
        crayon::blue$bold(substr(test_rjson$dateOfIssue, 1, 10)),
        ".\n\n", sep="")
    }
    if(length(test_rjson == 0)){
      cat("There is no draft reference on DataStore corresponding to your metadata DOI to upload your files to.\n")
      cat("Please use ", crayon::bold$green("set_datastore_doi()"),
          " to create a draft reference and insert the corresponding DOI into your metadata prior to uploading files.", sep="")
      return()
    }
    cat("Are you sure you want to upload your data package files to this reference?\n")
    var1 <- readline(prompt = "1: Yes\n2: No\n")
    if (var1 == 2){
      cat("Function terminated. You have not uploaded any files to DataStore.")
    }
    if (var1 ==1){
      #check for DOI & referenceId mismatch...this should never happen.
      if(!DS_ref == test_rjson$referenceId){
        cat("The DOI in your metadata, ", crayon::blue$bold(doi),
          ", does not match the reference ID, ",
          crayon::blue$bold(test_rjson$referenceId), ".\n", sep="")
        cat("Your files were not uploaded.\n")
        return()
      }
      #if DOI and referenceId match:
      else{
        #get list of .csvs
        files<-list.files(path=directory, pattern="*.csv", full.names = TRUE)
        #add metadata
        files<-append(files,
                      list.files(path=directory, pattern="*metadata.xml"))
        for(i in seq_along(files)){
          #test for files >4Mb:
          file_size_error<-NULL
          if(file.size(files[i])> 4194304){
            #warn for each file >4Mb
            cat(crayon::blue$bold(files[i]),
              "is greater than 4Mb and cannot be uploaded with this funcion.\n",
              sep="")
            file_size_error<-1
          }
          # stop if any files >4Mb
          if(!is.null(file_size_error)){
            stop()
          }
          if(is.null(file_size)){
            api_url <- paste0
            ("https://irmaservices.nps.gov/datastore-secure/v4/rest/Reference/",
              DS_ref, "/UploadFile")
            #upload the files
            for(i in seq_along_files){
              cat("Uploading: ", crayon::blue$bold(files[i]), sep="")
              req<- httr::POST(
                url = api_url,
                httr::add_headers('Content-Type' = 'multipart/form-data'),
                httr::authenticate(":", "", "ntlm"),
                body = list(addressFile = httr::upload_file(files[i])),
                encode="multipart",
                httr::progress(type="up", con=""))
              req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
              status_code<-httr::stop_for_status(req)$status_code
              if(status_code!=201){
                stop("ERROR: DataStore connection failed. Your file was not successfully uploaded.")
              }
              else{
                cat("Your file, ", crayon::blue$bold(files[i]),
                    ", has been uploaded to:\n", sep="")
                cat(req$headers$location)
              }
            }
          }
        }
      }
    }
  }
  if(force = TRUE){
    #API call to look for an existing reference:
    test_req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
    status_code<-httr::stop_for_status(test_req)$status_code
    #if API call fails, alert user and remind them to log on to VPN:
    if(!status_code==200){
      stop("ERROR: DataStore connection failed. Are you logged in to the VPN?\n")
    }
    if(length(test_rjson == 0)){
      cat("There is no draft reference on DataStore corresponding to your metadata DOI to upload your files to.\n")
      return()
    }
    #test that metadata DOI corresponds to the draft reference on DataStore
    #this test should never fail.
    if(!DS_ref == test_rjson$referenceId){
      cat("The DOI in your metadata, ", crayon::blue$bold(doi),
          ", does not match the reference ID, ",
          crayon::blue$bold(test_rjson$referenceId), ".\n", sep="")
      cat("Your files were not uploaded.\n")
      return()
    }
    #get list of .csvs
    files<-list.files(path=directory, pattern="*.csv", full.names = TRUE)
    #add metadata
    files<-append(files,
                  list.files(path=directory, pattern="*metadata.xml"))
    for(i in seq_along(files)){
      #test for files >4Mb:
      file_size_error<-NULL
      if(file.size(files[i])> 4194304){
        #warn for each file >4Mb
        cat(crayon::blue$bold(files[i]),
            "is greater than 4Mb and cannot be uploaded with this funcion.\n",
            sep="")
        file_size_error<-1
      }
      # stop if any files >4Mb
      if(!is.null(file_size_error)){
        stop()
      }
      if(is.null(file_size)){
        api_url <- paste0
        ("https://irmaservices.nps.gov/datastore-secure/v4/rest/Reference/",
          DS_ref, "/UploadFile")
        #upload the files
        for(i in seq_along_files){
          req<- httr::POST(
            url = api_url,
            httr::add_headers('Content-Type' = 'multipart/form-data'),
            httr::authenticate(":", "", "ntlm"),
            body = list(addressFile = httr::upload_file(files[i])),
            encode="multipart")
          req <- httr::GET(url, httr::authenticate(":", ":", "ntlm"))
          status_code<-httr::stop_for_status(req)$status_code
          if(status_code!=201){
            stop("ERROR: DataStore connection failed. Your file was not successfully uploaded.")
          }
        }
      }
    }
  }
}
