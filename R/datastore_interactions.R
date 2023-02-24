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
    if(!is.null(doc)){
      cat("Your current DOI is: ", crayon::blue$bold(doc), ".\n", sep="")
      cat("Are you sure you want to create a new draft reference on DataStore with a DOI?\n")
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
    url <- paste0("https://irmadev.nps.gov/DataStore/Reference/Profile/",
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
