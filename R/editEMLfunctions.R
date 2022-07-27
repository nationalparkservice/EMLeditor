#' Check & set a DOI
#'
#' @details
#' This function  checks to see if there is a DOI in the <alternateIdentifier> tag. The EMLassemblyline package stores datapackage DOIs in this tag (although the official EML schema has the DOI in a different location). If there is no DOI in the <alternateIdentifier> tag, the function adds a DOI & reports the new DOI. If there is a DOI, the function reports the existing DOI, and prompts the user for input to either retain the existing DOI or overwrite it. Reports back the existing or new DOI, depending on the user input..
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DSref is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated.
#' @returns an EML-formatted R object
#' @export
set.DOI<-function(emlObject, DSref){
  #Look for an existing datapackage DOI:
  doc<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier") #where EMLassemblyline stores DOIs.

  #If there is no existing DOI, add a DOI to the metadata
  if(is.null(doc)){
    emlObject$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", DSref)
    doc<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier")
    doc<-sub(".*? ", "", doc)
    #print the new DOI to the screen:
    cat("Your newly specified DOI is: ", doc)
  }

  #If there is a DOI, find the correct doi by searching for the text "doi: ".
  else{
    mylist<-NULL

    #hopefully deals with case when there are multiple DOIs specified under alternateIdentifier tags. Haven't run into this yet and so this remains untested.
    if(length(doc)>1){
      for(i in 1:length(doc)){
        if(stringr::str_detect(doc[i], "doi:" )){
          mylist<-append(mylist, doc[i])
        }
      }
    }
    #if there is only one alternateIdentifier:
    else{
      mylist<-doc
    }
    doi<-mylist[[1]]

    #If a DOI exists, ask the user what to do about it:
    var1<-readline(prompt=cat("Your EML already has a DOI specified in the <alternateIdentifier> tag:\n\n",  doi, "\n\nEnter 1 to retain this DOI\nEnter 2 to overwrite this DOI"))
    #if User opts to retain DOI, retain it
    if(var1==1){
      #print the existing DOI to the screen:
      doi<-sub(".*? ", "", doi)
      cat("Your DOI remains: ", doi)
    }
    #if User opts to change DOI, change it:
    if(var1==2){
      emlObject$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", DSref)
      #get the new DOI:
      doc<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier")
      doc<-sub(".*? ", "", doc)
      #print the new DOI to the screen:
      cat("Your newly specified DOI is: ", doc)
    }
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
#' @param DSref is the same as the 7-digit reference code generated on DataStore when the draft reference is initiated. Don't worry about the https://wwww.doi.org and the datapackage prefix - those will all automatically be added in by the function.
#' @returns an EML-formatted R object
#' @export
edit.DOI<-function(emlObject, DSref){
  emlObject$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", DSref)
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
  doi<-paste0("DRR: https://doi.org/10.36967", DRRdoi)
  emlObject$dataset$useageCitation<-doi
  return(emlObject)
}

#' adds an abstract
#'
#' @description set.abstract adds (or replaces) a simple abstract.
#'
#' @details checks for an abstract. If no abstract is found, it inserts the abstract given in @param abstract. If an existing abstract is found, the user is asked whether they want to replace it or not and the appropriate action is taken. Currently set.abstract does not allow for paragraphs or complex formatting. You are strongly encouraged to open your abstract in a text editor such as notepad and make sure there are no stray characters. If you need multiple paragraphs, you will need to do that via EMLassemblyline (for now).
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#' @param abstract is a text string that is your abstract. You can generate this directly in R or import a .txt file.
#'returns an EML-formatted R object
#' @export
#' @examples
#' set.abstract(emlObject "This is a very short abstract")
set.abstract<-function(emlObject, abstract){
  doc<-arcticdatautils::eml_get_simple(emlObject, "abstract")
  if(is.null(doc)){
    emlObject$eml$dataset$abstract<-paste0(abstract)
    print("No previous abstract was detected. Your new abstract has been added. View the current abstract using get.abstract.")
  }
  else{
    var1<-readline(prompt="Your EML already has an abstract. Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
    #if User opts to retain DOI, retain it
    if(var1==1){
      #print the existing DOI to the screen:
      emlObject$eml$dataset$abstract<-paste0(abstract)
      print("You have replaced your abstract. View the current abstract using get.abstract.")
    }
    #if User opts to change DOI, change it:
    if(var1==2){
      print("Your original abstract was retained. View the current abstract using get.abstract.")
    }
  }
  return(emlObject)
}




