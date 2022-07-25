#' Check & set a DOI
#'
#' @details
#' This function  checks to see if there is a DOI in the <alternateIdentifier> tag. The EMLassemblyline package stores datapackage DOIs in this tag (although the official EML schema has the DOI in a different location). If there is no DOI in the <alternateIdentifier> tag, the function adds a DOI. If there is a DOI, the function alerts the users to this fact, reports the existing DOI, and suggests using a separate function to edit an existing DOI (edit.DOI()).
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DOI is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated. Don't worry about the https://wwww.doi.org and the datapackage prefix - those will all automatically be added in by the function.
#' @returns an EML-formatted R object
#' @export
set.DOI<-function(emlObject, DOI){
  doc<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier") #where EMLassemblyline stores DOIs.
  #if a DOI exists, report that it already exists and prompt to edit:
  if(!is.null(doc)){
    mylist<-NULL
    #doc<-unlist(doc)
    if(length(doc)>1){
      for(i in 1:length(names(doc))){
        if(stringr::str_detect(doc[i], "doi:" )){
          mylist<-append(mylist, doc[i])
        }
      }
    }
    else{
      mylist<-doc
    }
    doi<-mylist[[1]]
    cat("Your EML already has a DOI specified in the <alternateIdentifier> tag:\n",  doi, "\n\nIf this is correct, no further action is needed. To edit your existing DOI, run \'edit.DOI()\'")
  }
  #if there is no DOI, add it directly:
  else{
    emlObject$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830", DOI)
  }
  return(emlObject)
}

#' Force-edits an existing DOI
#'
#' @description edit.DOI forces changes to an existing DOI
#'
#' @details
#' If a DOI already exists in the <alternateidentifier> tag (get.DOI() to check), this allows the user to over-write the existing DOI.  WARNING: will cause loss of the system="https://doi.org" setting. So only use this if you really don't already have a DOI.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DOI is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated. Don't worry about the https://wwww.doi.org and the datapackage prefix - those will all automatically be added in by the function.
#' @returns an EML-formatted R object
#' @export
edit.DOI<-function(emlObject, DOI){
  emlObject$dataset$alternateIdentifier<-paste0("doi:", DOI)
  return(emlObject)
}


# Add Park Unit Connections to metadata
#if geographic coverage exists, add to it.
#if geographic coverage exists, doesn't yet add in id= attribute. Grr.

#' Adds Park Unit Connections to metadata
#'
#' @details
#' Add the Park Unit Connection(s) to a <geographicDescription> tag under <coverage>. Park Unit Connection(s) are the (typically) four-letter codes describing the park unit(s) where data were collected (e.g. ROMO, not ROMN). If there are already =items listed under geographicCoverage, Park Unit Connections will be inserted as the first item in the list of geographicCoverages and will be prefaced by the string "NPS Unit Connections:".
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param ParkUnits a string of comma-separated park unit codes
#' @returns an EML-formatted R object
#' @export
#' @examples
#' ParkUnits<-("ROMO, GRSD, TRYME")
#' set.parkUnits(emlObject, ParkUnits)
set.parkUnits<-function(emlObject, ParkUnits){
  #get geographic coverage from emlObject
  doc<-EML::eml_get(emlObject, "geographicCoverage")

  #if there is no geo coverage, add it directly to emlObject
  if(is.null(doc)){
    emlObject$dataset$coverage$geographicCoverage$id<-"UnitConnections"
    emlObject$dataset$coverage$geographicCoverage$geographicDescription<-paste0("NPS Unit Connections: ",ParkUnits)
  }

  #if there are already geographicCoverage(s)
  #(unfortunately currently there is no way to ad id="UnitConnections" in this case)
  else{
    mylist<-NULL
    #ditch the '@context' list from the goeCoverage:
    for(i in 1:length(names(doc))){
      if(!names(doc)[i]=='@context')
        mylist<-append(mylist, doc[i])
    }
    #remove names from list (critical for writing back to xml)
    names(mylist)<-NULL

    #add text to indicate that these are park unit connections:
    units<-paste0("NPS Unit Connections: ", ParkUnits)

    #generate the new geographic coverage elements:
    geocov2<-list(geographicDescription=units)

    #combine new and old geo coverages (new always at the top!)
    mylist<-append(list(geocov2), mylist)

    #write over the existing geographic coverage
    emlObject$dataset$coverage$geographicCoverage<-mylist
  }
  return(emlObject)
}

#' Adds CUI to metadata
#'
#' @description set.CUI adds CUI codes to EML metadata
#'
#' @details set.CUI adds a CUI code to the tag <CUI> under <additionalMetadata><metadata>.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param CUI a string consisting of one of 5 potential CUI codes. Pay attention to the spaces:
#' FED ONLY - Contains CUI. Only federal employees should have access (similar to "internal only" in DataStore)
#' FEDCON - Contains CUI. Only federal employees and federal contractors should have access (also very much like current "internal only" setting in DataStore)
#' DL ONLY - Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)
#' NOCON - Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.
#' PUBVER - Does NOT contain CUI. The original data contained CUI, but in this datapackage CUI have been obscured so that it no longer contains CUI.
#' PUBFUL - Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package
#' @returns an EML-formatted R object
#' @export
#' @examples
#' set.CUI(emlObject, "PUBFUL")
set.CUI<-function(emlObject, CUI){
  #add those random spaces in case people forget
  if(CUI=="FEDONLY"){
    CUI=="FED ONLY"
  }
  if(CUI=="DLONLY"){
    CUI=="DL ONLY"
  }
  #add CUI to EML
  emlObject$additionalMetadata$metadata$CUI<-CUI
  return(emlObject)
}



#' adds DRR connection
#'
#' @description set.DRRdoi adds the DOI of an associated DRR
#'
#' @details adds uses the DataStore Reference ID for an associate DRR to the <useageCitation> as a properly formatted DOI (prefaced with "DRR: ") to the <useageCitation> tag. ####CAUTION: in the current implimentation this may overwrite any other useageCitation info.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DRRrefID a 7-digit string that is the DataStore Reference ID for the DRR associated with the datapackage.
#' @returns an EML-formatted R object
#' @export
#' @examples
#' set.DRRdoi(emlObject, "2293234")
set.DRRdoi<-function(emlObject, DRRrefID){
  doi<-paste0("DRR: https://doi.org/10.57830", DRRdoi)
  emlObject$dataset$useageCitation<-doi
  return(emlObject)
}
