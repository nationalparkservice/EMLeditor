#' inject NPS info into metadata
#'
#' @description injects static NPS-specific info into eml documents
#'
#' @details checks to see if the publisher element exists, and if not injects NPS-specific info into EML such as publisher, publication location, and ROR id - the types of things that will be the same for all NPS data or non-data publications and do not require user input. This function will be embedded in all set. and write. class functions (and get. functions?)
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#'
#' @return emlObject
#' @export
#'
#' @examples
#' set.NPSpublisher(emlObject)
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
#' set.version(emlObject)
#'
set.version<-function(emlObject){
  #get current EMLeditor package version:
  currentvers<-as.character(packageVersion("EMLeditor"))

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


##################################################
#To add in later: update EML version if it is out of date. What follows is some old/eneffective code for a first attempt:
##################################################
#if EMLeditor is included, but the version is wrong, update version
#    if(!is.null(app)){
#      for(i in seq_along(addMeta)){
#        if(suppressWarnings(stringr::str_detect#(addMeta[i], "EMLeditor"))){
          #could prob remove if statement and just sub it in no matter what
#          if(release[[i]][2]!=currentvers){
            #sub old version with new version:
#            release[[i]][2]<-currentvers
#          }
#        }
#      }
#      mylist<-NULL
#      for(i in seq_along(names(release))){
#        if(!names(release)[i]=='@context'){
#          mylist<-append(mylist, release[i])
#        }
#      }
      #names to null critical for writing to xml
#      names(mylist)<-NULL
      #overwrite existing additionalMetadata with new version info
#      emlObject$additionalMetadata$metadata$emlEditor<-mylist
#   }
#  }
#  return(emlObject)
#}

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
#' poly<-get.unitPolygon("BICY")
get.unitPolygon <- function(Unit_Code) {
  # get geography from NPS Rest Services
  UnitsURL <- paste0("https://irmaservices.nps.gov/v2/rest/unit/", Unit_Code, "/geography")
  xml <- httr::content(httr::GET(UnitsURL))

  # Create spatial feature from polygon info returned from NPS
  parkpolygon <- sf::st_as_sfc(xml[[1]]$Geography, geoJSON = TRUE)

  return(parkpolygon)
}
