#' Edit data package title
#'
#' @details The set.Title function checks to see if there is an existing title and then asks the user if they would like to change the title. Some work is still needed on this function as get_eml() automatically returns all instances of a given tag. Specifying which title will be important for this function to work well.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#' @param DataPackageTitle is a character string that will become the new title for the data package. It can be specified directly in the function call or it can be a previously defined object that holds a character string.
#' @param NPS is a logical that defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#'
#' @return an EML-formatted R object
#' @export
#'
#' @examples
#' DataPackageTitle<-"New Title. Must match DataStore Reference title."
#' set.title(emlObject, DataPackageTitle)
set.title<-function(emlObject, DataPackageTitle, NPS=TRUE){
  doc<-arcticdatautils::eml_get_simple(emlObject, "title")
  if(is.null(doc)){
    emlObject$eml$dataset$title<-paste0(DataPackageTitle)
    print("No previous title was detected. Your new title has been added. View the current title using get.title.")
  }
  else{
    var1<-readline(prompt="Your EML already has an title. Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
  #if User opts to retain DOI, retain it
    if(var1==1){
      #print the existing DOI to the screen:
      emlObject$eml$dataset$title<-paste0(DataPackageTitle)
      print("You have replaced your title. View the current title using get.title.")
    }
    #if User opts to change DOI, change it:
    if(var1==2){
      print("Your original title was retained. View the current abstract using get.title.")
    }
  }
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }
  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)
  return(emlObject)
  }



#' Check & set a DOI
#'
#' @details
#' This function  checks to see if there is a DOI in the <alternateIdentifier> tag. The EMLassemblyline package stores data package DOIs in this tag (although the official EML schema has the DOI in a different location). If there is no DOI in the <alternateIdentifier> tag, the function adds a DOI & reports the new DOI. If there is a DOI, the function reports the existing DOI, and prompts the user for input to either retain the existing DOI or overwrite it. Reports back the existing or new DOI, depending on the user input..
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DSref is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated.
#' @param NPS is a logical that defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
set.DOI<-function(emlObject, DSref, NPS=TRUE){
  #Look for an existing data package DOI:
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
      for(i in seq_along(doc)){
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

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
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
#'
#' @param DSref is the same as the 7-digit reference code generated on DataStore when the draft reference is initiated. Don't worry about the https://wwww.doi.org and the data package prefix - those will all automatically be added in by the function.
#'
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#'
#' @returns an EML-formatted R object
#'
#' @export
#'
#' @example
#' edit.DOI(emlObject, "1111111")
edit.DOI<-function(emlObject, DSref, NPS=TRUE){
  emlObject$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", DSref)

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }

  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)

  return(emlObject)
  }



#' Add Park Unit Connections to metadata
#'
#' @description adds all specified park unit connections and their N, E, S, W bounding boxes to <geographicCoverage>.
#'
#' @details Adds the Park Unit Connection(s) to a <coverage>. Park Unit Connection(s) are the (typically) four-letter codes describing the park unit(s) where data were collected (e.g. ROMO, not ROMN). Each park unit connection is given a separate <geographicCoverage> element. For each park unit connection, the unit name will be listed under <geographicDescription> and prefaced with "NPS Unit Connections:". Required child elements (bounding coordinates) are auto populated. If other <geographicCoverage> elements exist, set.parkUnits will add to them, not overwrite them. If not other <geographicCoverage> elements exist, set.parkUnits will create a new set of <geographicCoverage> elements.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param ParkUnits a list of comma-separated strings where each string is a park unit code.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#' ParkUnits<-("ROMO, GRSD, TRYME")
#' set.parkUnits(emlObject, ParkUnits)
set.parkUnits<-function(emlObject, ParkUnits, NPS=TRUE){
  #add text to indicate that these are park unit connections. Note that bounding coordinates are DUMMY COORDINATES
  units<-paste0("NPS Unit Connections: ", ParkUnits)

  unit_list<-NULL
  for(i in seq_along(ParkUnits)){
    poly<-get.unitPolygon(ParkUnits[i])
    poly<-as.data.frame(poly[[1]][1])
    N<-max(poly[,2])
    S<-min(poly[,2])
    W<-max(poly[,1])
    E<-min(poly[,1])
    geocov<- EML::eml$geographicCoverage(geographicDescription =
                    paste0("NPS Unit Connections: ", ParkUnits[i]),
                    boundingCoordinates = EML::eml$boundingCoordinates(
                      northBoundingCoordinate = N,
                      eastBoundingCoordinate = E,
                      southBoundingCoordinate = S,
                      westBoundingCoordinate = W))
    unit_list<-append(unit_list, list(geocov))
  }

  #get geographic coverage from emlObject
  doc<-EML::eml_get(emlObject, "geographicCoverage")

  #if there is no geo coverage, add it directly to emlObject
  if(is.null(doc)){
    emlObject$dataset$coverage$geographicCoverage<-unit_list
  }

  #if there are already geographicCoverage(s)
  else{
    mylist<-NULL
    #ditch the '@context' list from the geographicCoverage:
    for(i in seq_along(names(doc))){
      if(!names(doc)[i]=='@context'){
        mylist<-append(mylist, doc[i])
      }
    }
    #remove names from list (critical for writing back to xml)
    names(mylist)<-NULL

    #combine new and old geo coverages (new always at the top!)
    mylist<-append(unit_list, mylist)

    #write over the existing geographic coverage
    emlObject$dataset$coverage$geographicCoverage<-mylist
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }

  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)

  return(emlObject)
}

#' Adds CUI to metadata
#'
#' @description set.CUI adds CUI codes to EML metadata
#'
#' @details set.CUI adds a CUI code to the tag <CUI> under <additionalMetadata><metadata>.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param CUI a string consisting of one of 6 potential CUI codes (defaults to "PUBFUL"). Pay attention to the spaces:
#' FED ONLY - Contains CUI. Only federal employees should have access (similar to "internal only" in DataStore)
#' FEDCON - Contains CUI. Only federal employees and federal contractors should have access (also very much like current "internal only" setting in DataStore)
#' DL ONLY - Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)
#' NOCON - Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.
#' PUBVER - Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI.
#' PUBFUL - Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#' set.CUI(emlObject, "PUBFUL")
set.CUI<-function(emlObject, CUIcode=c("PUBFUL", "PUBVER", "NOCON", "DL ONLY", "FEDCON", "FED ONLY"), NPS=TRUE){

  #verify CUI code entry; stop if does not equal one of six valid codes listed above:
  CUIcode<-match.arg(CUIcode)

  #Generate new CUI element for additionalMetadata
  myCUI<-list(metadata=list(CUI=CUIcode), id="CUI")

  #get existing additionalMetadata elements:
  doc<-EML::eml_get(emlObject, "additionalMetadata")

  #if no prior additionalMetadata elements, add CUI to additionalMetadata:
  if(sum(names(doc)!="@context")==0){
    emlObject$additionalMetadata<-myCUI
  }

  #if additionalMetadata already exists:
  if(sum(names(doc)!="@context")>0){
    mylist<-NULL
    #ditch the '@context' list from doc:
    for(i in seq_along(names(doc))){
      if(!names(doc)[i]=='@context' && !names(doc)[i]=="id"){
        mylist<-append(mylist, doc[i])
      }
    x<-length(mylist)
    }

    #Is CUI already specified?
    existCUI<-NULL
    for(i in seq_along(doc)){
      if(suppressWarnings(stringr::str_detect(doc[i], "CUI"))){
        existCUI<-"CUI"
      }
    }

    #If existing CUI, stop.
    if(!is.null(existCUI)){
      stop("CUI has already been specified")
    }
    #If no existing CUI, add it in:
    if(is.null(existCUI)){
      if(x==1){
        emlObject$additionalMetadata<-list(myCUI, emlObject$additionalMetadata)
      }
      if(x>1){
        emlObject$additionalMetadata[[x+1]]<-myCUI
      }
    }
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }

  #add/updated EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)

  return(emlObject)
}



#' adds DRR connection
#'
#' @description set.DRRdoi adds the DOI of an associated DRR
#'
#' @details adds uses the DataStore Reference ID for an associate DRR to the <usageCitation> as a properly formatted DOI (prefaced with "DRR: ") to the <usageCitation> element. Creates and populates required children elements for usageCitation including the DRR title, creator organization name, and report number. Note the default NPS=TRUE sets the DRR creator organization to NPS. If you do NOT want the organization name for the DRR to be NPS, set NPS="Your Favorite Organization". sets the id flag for this usageCitation to "associatedDRR".
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param DRRrefID a 7-digit string that is the DataStore Reference ID for the DRR associated with the data package.
#' @param DRRtitle the title of the DRR as it appears in the DataStore Reference.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. Also fills in organizationName for the DRR creator. If you are NOT publishing with NPS/for, set NPS="your organization name".
#' @returns an EML-formatted R object
#' @export
#' @examples
#' DRRtitle<-"Data Release Report for Data Package 1234"
#' set.DRRdoi(emlObject, "2293234", DRRtitle)
set.DRRdoi<-function(emlObject, DRRrefID, DRRtitle, NPS=TRUE){
  if(NPS==TRUE){org<-"NPS"}
  else{org<-NPS}

  doi<-paste0("DRR: https://doi.org/10.36967/", DRRrefID)

  cite<-EML::eml$usageCitation(alternateIdentifier = doi,
                  title = DRRtitle,
                  creator = EML::eml$creator(
                    organizationName = org),
                  report = EML::eml$report(reportNumber = DRRrefID),
                  id = "associatedDRR")

  emlObject$dataset$usageCitation<-cite
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
    }
  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)
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
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#' set.abstract(emlObject "This is a very short abstract")
set.abstract<-function(emlObject, abstract, NPS=TRUE){
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
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }
  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)
  return(emlObject)
}

#' Edit literature cited
#'
#' @description set.lit is an interactive method for editing the literature cited sections.
#'
#' @details looks for literature cited in the <literatureCited> tag and if it finds none, inserts bibtex-formatted literature cited from a the supplied *.bib file. If literature cited exists it asks to either do nothing, replace the existing literature cited with the supplied .bib file or append additional references from the supplied .bib file.
#' @param emlObject  is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @param bibtex is a text file with one or more bib-formatted references with the extension .bib. Make sure the .bib file is in your working directory, or supply the path to the file.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @return an EML object
#' @export
#'
#' @examples
#' litcited2<-set.lit(emlObject, "bibfile.bib")
set.lit<-function(emlObject, bibtex_file, NPS=TRUE){
  bibtex_citation<-readr::read_file(bibtex_file)
  lit<-arcticdatautils::eml_get_simple(emlObject, "literatureCited")
  if(is.null(lit)){
    emlObject$dataset$literatureCited$bibtex<-bibtex_citation
  }
  else{
    var1<-readline(prompt="You have already specified literature cited. To view your current literature cited, use get.litCited. Would you like to:\n\n 1: Make no changes\n 2: Replace your literature cited\n 3: add to your literature cited\n\n")
    if(var1==1){
      print("No changes were made to literature cited.")
    }
    if(var1==2){
      print("Your literature cited section has been replaced. To view your new literature cited use get.lit")
      emlObject$dataset$literatureCited$bibtex<-bibtex_citation
    }
    if(var1==3){
      bib2<-paste0(lit, "\n", bibtex_citation, sep="")
      emlObject$dataset$literatureCited$bibtex<-bib2
      print("You have added to your literature cited section. To view your new literature cited use get.lit")

    }
  }
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    emlObject<-set.NPSpublisher(emlObject)
  }
  #add/update EMLeditor and version to metadata:
  emlObject<-set.version(emlObject)
  return(emlObject)
}
