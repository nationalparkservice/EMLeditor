#' inject NPS Publisher info into metadata
#'
#' @description injects static NPS-specific publisher info into eml documents. Calls the sub-function set.forOrByNPS, which adds an additionalMetadata element with for or by NPS = TRUE.
#'
#' @details checks to see if the publisher element exists, and if not injects NPS-specific info into EML such as publisher, publication location, and ROR id - the types of things that will be the same for all NPS data or non-data publications and do not require user input. This function will be embedded in all set. and write. class functions (and get. functions?).
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#'
#' @return emlObject
#' @export
#'
#' @examples
#'  \dontrun{
#' set.NPSpublisher(emlObject)
#' }
set.NPSpublisher<-function(emlObject){
  #get existing publisher info for the data package:
  publish<-emlObject$dataset$publisher

  #create desired publisher info:
  pubset<- list(organizationName =
            "National Park Service",
          address = list(deliveryPoint = "1201 Oakridge Drive, Suite 150",
                         city = "Fort Collins",
                         administrativeArea="CO",
                         postalCode="80525",
                         country="USA"),
          onlineUrl = "http://www.nps.gov",
          electronicMailAddress = "irma@nps.gov",
          userId = list(directory="https://ror.org/", userId="https://ror.org/044zqqy65"))

  #if existing and desired publisher don't match, replace existing with desired.
  if(!identical(publish, pubset)){
    emlObject$dataset$publisher<-pubset
  }

  #since the publisher is NPS, sets an additionalMetadata field for For or By NPS to TRUE.
  emlObject<-set.forByNPS(emlObject)

  return(emlObject)
}

#' Add/update EMLeditor version
#'
#' @description set.version adds the current version of EMLeditor to the EML document.
#'
#' @details set.version adds the current version of EMLeditor to the metadata, specifically in the "additionalMetadata" element
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#'
#' @return emlObject
#' @export
#'
#' @examples
#'  \dontrun{
#' set.version(emlObject)
#' }
set.version<-function(emlObject){
  #get current EMLeditor package version:
  currentvers<-as.character(utils::packageVersion("EMLeditor"))

  #set up additionalMetadata elements for EMLeditor:
  EMLed<-list(metadata=list(emlEditor=
                              list(app="EMLeditor",
                                   release=currentvers)),
                                   id="emlEditor")

  #access additionalMetadata elements:
  addMeta<-EML::eml_get(emlObject, "additionalMetadata")

  #if no additionalMetadata, add in EMLeditor and current version:
  if(sum(names(addMeta)!="@context")==0){
    emlObject$additionalMetadata<-EMLed
  }

  #if there are existing additionalMetadata elements:
  if(sum(names(addMeta)!="@context")>0){

    mylist<-NULL
    #ditch the '@context' list from the goeCoverage:
    for(i in seq_along(names(addMeta))){
      if(!names(addMeta)[i]=='@context' && !names(addMeta)[i]=="id"){
        mylist<-append(mylist, addMeta[i])
      }
    }
    x<-length(mylist)

    #does it include EMLeditor?
    app<-NULL
    for(i in seq_along(addMeta)){
      if(suppressWarnings(stringr::str_detect(addMeta[i], "EMLeditor"))){
        app<-"EMLeditor"
      }
    }

    #if no info on EMLeditor, add EMLeditor to additionalMetadata
    if(is.null(app)){
      if(x==1){
        emlObject$additionalMetadata<-list(EMLed, emlObject$additionalMetadata)
      }
      if(x>1){
        emlObject$additionalMetadata[[x+1]]<-EMLed
      }
    }
  }
  return(emlObject)
}

#' Get Park Unit Polygon
#'
#' @description get.unitPolygon gets the polygon for a given park unit.
#'
#' @details retrieves a geoJSON string for a polygon of a park unit from NPS Rest services. Note: This is not the official boundary (erm... ok then what is it?!?).
#'
#' @param Unit_Code a string (typically 4 characters) that is the park unit code.
#'
#' @return
#' @export
#'
#' @examples
#'  \dontrun{
#' poly<-get.unitPolygon("BICY")
#' }
get.unitPolygon <- function(Unit_Code) {
  # get geography from NPS Rest Services
  UnitsURL <- paste0("https://irmaservices.nps.gov/v2/rest/unit/", Unit_Code, "/geography")
  xml <- httr::content(httr::GET(UnitsURL))

  # Create spatial feature from polygon info returned from NPS
  parkpolygon <- sf::st_as_sfc(xml[[1]]$Geography, geoJSON = TRUE)

  return(parkpolygon)
}

#' Set "For or By" NPS
#'
#' @description set.forByNPS adds an element to additionalMetadata with For or By NPS set to TRUE and a second element agencyOriginated set to "NPS" with the understanding that all data products created for or by the NPS have NPS as the originating agency.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#'
#' @return emlObject
#'
#' @export
#'
#' @examples
#'  \dontrun{
#' set.forByNPS(emlObject)
#' }
set.forByNPS<-function(emlObject){

  #set up additionalMetadata elements for EMLeditor:
  forby<-list(metadata=list(For_or_by_NPS="TRUE",
                            agencyOriginated="NPS"),
                            id="agencyOriginated")

  #access additionalMetadata elements:
  addMeta<-EML::eml_get(emlObject, "additionalMetadata")
  addMeta<-within(addMeta, rm('@context'))

  #if no additionalMetadata, add in EMLeditor and current version:
  if(length(names(addMeta))==0){
    emlObject$additionalMetadata<-forby
  }

  #if there are existing additionalMetadata elements:
  if(length(names(addMeta))>0){
    x<-length(addMeta)

    #does it include EMLeditor?
    For_or_by_NPS<-NULL
    for(i in seq_along(addMeta)){
      if(suppressWarnings(stringr::str_detect(addMeta[i], "For_or_by_NPS"))){
        For_or_by_NPS<-"TRUE"
      }
    }

    #if no info on ForOrByNPS, add ForOrByNPS to additionalMetadata
    if(is.null(For_or_by_NPS)){
      if(x==1){
        emlObject$additionalMetadata<-list(forby, emlObject$additionalMetadata)
      }
      if(x>1){
        emlObject$additionalMetadata[[x+1]]<-forby
      }
    }
  }
  return(emlObject)
}
