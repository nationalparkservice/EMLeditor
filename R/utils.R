#' alias for eml_get_simple
#'
#' @decription eml_get_simpleNPS is an alias for arcticdatautils eml_get_simple function
#'
#' @details eml_get_simpleNPS serves as an alias for the arcticdatautils function eml_get_simple. This means the eml_get_simple() function will be called using current local version of arcticdatautils on the users's machine rather than whatever version was available when the package was built.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param elment is the name of the element to be extracted. If multiple occurances are found, will extract all.
#' @export
eml_get_simpleNPS<-function(...)arcticdatautils::eml_get_simple()

#' alias for eml_get
#'
#' @decription eml_getNPS is an alias for the EML get_simple function
#'
#' @details eml_getNPS serves as an alias for the EML function eml_get. This means the eml_get() function will be called using current local version of EML on the users's machine rather than whatever version was available when the package was built.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param elment is the name of the element to be extracted. If multiple occurrences are found, will extract all.
#' @export
eml_getNPS<-function(...)EML::eml_get()


#' inject NPS info into metadata
#'
#' @description injects static NPS-specific info into eml documents
#'
#' @details checks to see if it exists, and if not injects NPS-specific info into eml such as publisher, publication location, and ROR id - the types of things that will be the same for all NPS data or non-data publications and do not require user input. This function will be embedded in all set. and write. class functions (and get. functions?)

#' @param emlObject
#'
#' @return
#' @export
#'
#' @examples
set.NPSpublisher<-function(emlObject){
  #check for and isert publisher information:
  publish<-arcticdatautils::eml_get_simple(emlObject, "publisher")
    if(is.null(publish)){
        emlObject$dataset$publisher <-list(organizationName = "U.S. National Park Service",
                                      onlineUrl = "http://www.nps.gov",
                                      userId = "https://ror.org/044zqqy65",
                                      electronicMailAddress = "irma@nps.gov")
        #note: oddly defaults to alphabetical order in .xml. Not sure why or how to stop this not sure it matters.
        emlObject$dataset$publisher$address<-list(deliveryPoint = "1201 Oakridge Drive, Suite 150",
                                                  city = "Fort Collins",
                                                  administrativeArea = "CO",
                                                  postalCode = "80525",
                                                  country = "USA")
    }
  return(emlObject)
}


test$dataset$publisher<-list(organizationName = "NSF Arctic Data Center",
     onlineUrl = "http://arcticdata.io",
     userId = list(directory = "https://www.wikidata.org/", userId = "Q77285095"),
     electronicMailAddress = "support@arcticdata.io")
