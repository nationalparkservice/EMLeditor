#' Edit data package title
#'
#' @details The set_title function checks to see if there is an existing title and then asks the user if they would like to change the title. Some work is still needed on this function as get_eml() automatically returns all instances of a given tag. Specifying which title will be important for this function to work well.
#'
#' @param eml_object is an R object imported (typically from an EML-formatted .xml file) using EML::read_eml(<filename>, from="xml").
#' @param data_package_title is a character string that will become the new title for the data package. It can be specified directly in the function call or it can be a previously defined object that holds a character string.
#' @param NPS is a logical that defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, it will be replaced by NPS (with publisher location set to the Fort Collins Office). If you are not publishing with NPS, set to FALSE.
#'
#' @return an EML-formatted R object
#' @export
#'
#' @examples
#'  \dontrun{
#' data_package_title<-"New Title. Must match DataStore Reference title."
#' eml_object<-set_title(eml_object, data_package_title)
#' }
set_title<-function(eml_object, data_package_title, NPS=TRUE){
  doc<-arcticdatautils::eml_get_simple(eml_object, "title")
  if(is.null(doc)){
    eml_object$eml$dataset$title<-paste0(data_package_title)
    print("No previous title was detected. Your new title has been added. View the current title using get.title.")
  }
  else{
    var1<-readline(prompt="Your EML already has an title. Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
  #if User opts to retain DOI, retain it
    if(var1==1){
      #print the existing DOI to the screen:
      eml_object$eml$dataset$title<-paste0(data_package_title)
      print("You have replaced your title. View the current title using get.title.")
    }
    #if User opts to change DOI, change it:
    if(var1==2){
      print("Your original title was retained. View the current abstract using get.title.")
    }
  }
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }
  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)
  return(eml_object)
  }



#' Check & set a DOI
#'
#' @details set_doi checks to see if there is a DOI in the <alternateIdentifier> tag. The EMLassemblyline package stores data package DOIs in this tag (although the official EML schema has the DOI in a different location). If there is no DOI in the <alternateIdentifier> tag, the function adds a DOI & reports the new DOI. If there is a DOI, the function reports the existing DOI, and prompts the user for input to either retain the existing DOI or overwrite it. Reports back the existing or new DOI, depending on the user input..
#'
#' @inheritParams set_title
#'
#' @param ds_ref is the same as the 7-digit reference code generated on DataStore when a draft reference is initiated.
#' @param NPS is a logical that defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#'  \dontrun{
#'  eml_object<-set_doi(eml_object, 1234567)
#'  }
set_doi<-function(eml_object, ds_ref, NPS=TRUE){
  #Look for an existing data package DOI:
  doc<-arcticdatautils::eml_get_simple(eml_object, "alternateIdentifier") #where EMLassemblyline stores DOIs.

  #If there is no existing DOI, add a DOI to the metadata
  if(is.null(doc)){
    eml_object$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", ds_ref)
    doc<-arcticdatautils::eml_get_simple(eml_object, "alternateIdentifier")
    doc<-sub(".*? ", "", doc)
    #print the new DOI to the screen:
    cat("Your newly specified DOI is: ", doc)
  }

  #If there is a DOI, find the correct doi by searching for the text "doi: ".
  else{
    my_list<-NULL

    #hopefully deals with case when there are multiple DOIs specified under alternateIdentifier tags. Haven't run into this yet and so this remains untested.
    if(length(doc)>1){
      for(i in seq_along(doc)){
        if(stringr::str_detect(doc[i], "doi:" )){
          my_list<-append(my_list, doc[i])
        }
      }
    }
    #if there is only one alternateIdentifier:
    else{
      my_list<-doc
    }
    doi<-my_list[[1]]

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
      eml_object$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", ds_ref)
      #get the new DOI:
      doc<-arcticdatautils::eml_get_simple(eml_object, "alternateIdentifier")
      doc<-sub(".*? ", "", doc)
      #print the new DOI to the screen:
      cat("Your newly specified DOI is: ", doc)
    }
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}

#' Force-edits an existing DOI
#'
#' @description new_doi forces changes to an existing DOI
#'
#' @details
#' If a DOI already exists in the <alternateidentifier> tag (get_doi() to check), this allows the user to over-write the existing DOI.  WARNING: will cause loss of the system="https://doi.org" setting. So only use this if you really don't already have a DOI.
#'
#' @inheritParams set_title
#'
#' @param ds_ref is the same as the 7-digit reference code generated on DataStore when the draft reference is initiated. Don't worry about the https://www.doi.org and the data package prefix - those will all automatically be added in by the function.
#'
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE.
#'
#' @returns an EML-formatted R object
#'
#' @export
#'
#' @examples
#'  \dontrun{
#'  eml_object<-new_doi(eml_object, "1111111")
#' }
new_doi<-function(eml_object, ds_ref, NPS=TRUE){
  eml_object$dataset$alternateIdentifier<-paste0("doi: https://doi.org/10.57830/", ds_ref)

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
  }



#' Add Park Unit Connections to metadata
#'
#' @description set_park_units adds all specified park unit connections and their N, E, S, W bounding boxes to <geographicCoverage>.
#'
#' @details Adds the Park Unit Connection(s) to a <coverage>. Park Unit Connection(s) are the (typically) four-letter codes describing the park unit(s) where data were collected (e.g. ROMO, not ROMN). Each park unit connection is given a separate <geographicCoverage> element. For each park unit connection, the unit name will be listed under <geographicDescription> and prefaced with "NPS Unit Connections:". Required child elements (bounding coordinates) are auto populated. If other <geographicCoverage> elements exist, set_park_units will add to them, not overwrite them. If not other <geographicCoverage> elements exist, set_park_units will create a new set of <geographicCoverage> elements.
#'
#' @inheritParams set_title
#'
#' @param park_units a list of comma-separated strings where each string is a park unit code.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#'  \dontrun{
#' park_units<-("ROMO, GRSD, TRYME")
#' set_park_units(eml_object, park_units)
#' }
set_park_units<-function(eml_object, park_units, NPS=TRUE){
  #add text to indicate that these are park unit connections.
  units<-paste0("NPS Unit Connections: ", park_units)

  unit_list<-NULL
  for(i in seq_along(park_units)){
    poly<-.get_unit_polygon(park_units[i])
    poly<-as.data.frame(poly[[1]][1])
    N<-max(poly[,2])
    S<-min(poly[,2])
    W<-max(poly[,1])
    E<-min(poly[,1])
    geocov<- EML::eml$geographicCoverage(geographicDescription =
                    paste0("NPS Unit Connections: ", park_units[i]),
                    boundingCoordinates = EML::eml$boundingCoordinates(
                      northBoundingCoordinate = N,
                      eastBoundingCoordinate = E,
                      southBoundingCoordinate = S,
                      westBoundingCoordinate = W))
    unit_list<-append(unit_list, list(geocov))
  }

  #get geographic coverage from eml_object
  doc<-EML::eml_get(eml_object, "geographicCoverage")

  #if there is no geo coverage, add it directly to eml_object
  if(is.null(doc)){
    eml_object$dataset$coverage$geographicCoverage<-unit_list
  }

  #if there are already geographicCoverage(s)
  else{

    #remove @context from list
    my_list<-within(doc, rm("@context"))

    #remove names from list (critical for writing back to xml)
    names(my_list)<-NULL

    #if there is more than 1 geo coverage:
    if(length(my_list)>2){

      #combine new and old geo coverages (new always at the top!)
      my_list<-append(unit_list, my_list)

      #write over the existing geographic coverage
      eml_object$dataset$coverage$geographicCoverage<-my_list
    }

    #if there is only one geo coverage:
    if(length(my_list)==2){

     geocov2 <- EML::eml$geographicCoverage(geographicDescription =
                         doc$geographicDescription,
                         boundingCoordinates = doc$boundingCoordinates)

      #add park unit connections and existing geo coverage (park units always on top!)
      eml_object$dataset$coverage$geographicCoverage <- list(geocov, geocov2)
    }
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}

#' Adds CUI to metadata
#'
#' @description set_cui adds CUI codes to EML metadata
#'
#' @details set_cui adds a CUI code to the tag <CUI> under <additionalMetadata><metadata>.
#'
#' @inheritParams set_title
#' @param cui_code a string consisting of one of 7 potential CUI codes (defaults to "PUBFUL"). Pay attention to the spaces:
#' FED ONLY - Contains CUI. Only federal employees should have access (similar to "internal only" in DataStore)
#' FEDCON - Contains CUI. Only federal employees and federal contractors should have access (also very much like current "internal only" setting in DataStore)
#' DL ONLY - Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)
#' NOCON - Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot.
#' PUBVER - Does NOT contain CUI. The original data contained CUI, but in this data package CUI have been obscured so that it no longer contains CUI.
#' PUBFUL - Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package.
#' NPSONLY - Contains CUI. For NPS access only.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#'  \dontrun{
#' set_cui(eml_object, "PUBFUL")
#' }
set_cui<-function(eml_object, cui_code=c("PUBFUL", "PUBVER", "NOCON", "DL ONLY", "FEDCON", "FED ONLY", "NPSONLY"), NPS=TRUE){

  #verify CUI code entry; stop if does not equal one of six valid codes listed above:
  cui_code<-match.arg(cui_code)

  #Generate new CUI element for additionalMetadata
  my_cui<-list(metadata=list(CUI=cui_code), id="CUI")

  #get existing additionalMetadata elements:
  doc<-EML::eml_get(eml_object, "additionalMetadata")

  #if no prior additionalMetadata elements, add CUI to additionalMetadata:
  if(sum(names(doc)!="@context")==0){
    eml_object$additionalMetadata<-my_cui
  }

  #if additionalMetadata already exists:
  if(sum(names(doc)!="@context")>0){
    my_list<-NULL
    #ditch the '@context' list from doc:
    for(i in seq_along(names(doc))){
      if(!names(doc)[i]=='@context' && !names(doc)[i]=="id"){
        my_list<-append(my_list, doc[i])
      }
    x<-length(my_list)
    }

    #Is CUI already specified?
    exist_cui<-NULL
    for(i in seq_along(doc)){
      if(suppressWarnings(stringr::str_detect(doc[i], "CUI"))){
        exist_cui<-"CUI"
      }
    }

    #If existing CUI, stop.
    if(!is.null(exist_cui)){
      stop("CUI has already been specified")
    }
    #If no existing CUI, add it in:
    if(is.null(exist_cui)){
      if(x==1){
        eml_object$additionalMetadata<-list(my_cui, eml_object$additionalMetadata)
      }
      if(x>1){
        eml_object$additionalMetadata[[x+1]]<-my_cui
      }
    }
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/updated EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}



#' adds DRR connection
#'
#' @description set_drr_doi adds the DOI of an associated DRR
#'
#' @details adds uses the DataStore Reference ID for an associate DRR to the <usageCitation> as a properly formatted DOI (prefaced with "DRR: ") to the <usageCitation> element. Creates and populates required children elements for usageCitation including the DRR title, creator organization name, and report number. Note the default NPS=TRUE sets the DRR creator organization to NPS. If you do NOT want the organization name for the DRR to be NPS, set NPS="Your Favorite Organization". sets the id flag for this usageCitation to "associatedDRR".
#'
#' @inheritParams set_title
#' @param drr_ref_id a 7-digit string that is the DataStore Reference ID for the DRR associated with the data package.
#' @param drr_title the title of the DRR as it appears in the DataStore Reference.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. Also fills in organizationName for the DRR creator. If you are NOT publishing with NPS/for, set NPS="your organization name".
#' @returns an EML-formatted R object
#' @export
#' @examples
#'  \dontrun{
#' drr_title<-"Data Release Report for Data Package 1234"
#' set_drr_doi(eml_object, "2293234", drr_title)
#' }
set_drr_doi<-function(eml_object, drr_ref_id, drr_title, NPS=TRUE){
  if(NPS==TRUE){org<-"NPS"}
  else{org<-NPS}

  doi<-paste0("DRR: https://doi.org/10.36967/", drr_ref_id)

  cite<-EML::eml$usageCitation(alternateIdentifier = doi,
                  title = drr_title,
                  creator = EML::eml$creator(
                    organizationName = org),
                  report = EML::eml$report(reportNumber = drr_ref_id),
                  id = "associatedDRR")

  eml_object$dataset$usageCitation<-cite
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
    }
  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)
  return(eml_object)
}

#' adds an abstract
#'
#' @description set_abstract adds (or replaces) a simple abstract.
#'
#' @details checks for an abstract. If no abstract is found, it inserts the abstract given in @param abstract. If an existing abstract is found, the user is asked whether they want to replace it or not and the appropriate action is taken. Currently set_abstract does not allow for paragraphs or complex formatting. You are strongly encouraged to open your abstract in a text editor such as notepad and make sure there are no stray characters. If you need multiple paragraphs, you will need to do that via EMLassemblyline (for now).
#'
#' @inheritParams set_title
#' @param abstract is a text string that is your abstract. You can generate this directly in R or import a .txt file.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @returns an EML-formatted R object
#' @export
#' @examples
#'  \dontrun{
#' eml_object<-set_abstract(eml_object, "This is a very short abstract")
#' }
set_abstract<-function(eml_object, abstract, NPS=TRUE){
  doc<-arcticdatautils::eml_get_simple(eml_object, "abstract")
  if(is.null(doc)){
    eml_object$eml$dataset$abstract<-paste0(abstract)
    print("No previous abstract was detected. Your new abstract has been added. View the current abstract using get.abstract.")
  }
  else{
    var1<-readline(prompt="Your EML already has an abstract. Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")
    #if User opts to retain DOI, retain it
    if(var1==1){
      #print the existing DOI to the screen:
      eml_object$eml$dataset$abstract<-paste0(abstract)
      print("You have replaced your abstract. View the current abstract using get.abstract.")
    }
    #if User opts to change DOI, change it:
    if(var1==2){
      print("Your original abstract was retained. View the current abstract using get.abstract.")
    }
  }
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }
  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)
  return(eml_object)
}

#' Edit literature cited
#'
#' @description set_lit is an interactive method for editing the literature cited sections.
#'
#' @details looks for literature cited in the <literatureCited> tag and if it finds none, inserts bibtex-formatted literature cited from a the supplied *.bib file. If literature cited exists it asks to either do nothing, replace the existing literature cited with the supplied .bib file or append additional references from the supplied .bib file.
#'
#' @inheritParams set_title
#' @param bibtex_file is a text file with one or more bib-formatted references with the extension .bib. Make sure the .bib file is in your working directory, or supply the path to the file.
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE
#' @return an EML object
#' @export
#'
#' @examples
#'  \dontrun{
#' eml_object<-litcited2<-set_lit(eml_object, "bibfile.bib")
#' }
set_lit<-function(eml_object, bibtex_file, NPS=TRUE){
  bibtex_citation<-readr::read_file(bibtex_file)
  lit<-arcticdatautils::eml_get_simple(eml_object, "literatureCited")
  if(is.null(lit)){
    eml_object$dataset$literatureCited$bibtex<-bibtex_citation
  }
  else{
    var1<-readline(prompt="You have already specified literature cited. To view your current literature cited, use get.litCited. Would you like to:\n\n 1: Make no changes\n 2: Replace your literature cited\n 3: add to your literature cited\n\n")
    if(var1==1){
      print("No changes were made to literature cited.")
    }
    if(var1==2){
      print("Your literature cited section has been replaced. To view your new literature cited use get.lit")
      eml_object$dataset$literatureCited$bibtex<-bibtex_citation
    }
    if(var1==3){
      bib2<-paste0(lit, "\n", bibtex_citation, sep="")
      eml_object$dataset$literatureCited$bibtex<-bib2
      print("You have added to your literature cited section. To view your new literature cited use get.lit")

    }
  }
  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }
  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)
  return(eml_object)
}



#' Sets Producing Units for use in DataStore
#'
#' @description set_producing_units inserts the unit code for the producing unit for the data/metadata into the EML metdata file.
#'
#' @details inserts the unit code into the metadataProvider element. Currently cannot add to existing metadataProvider fields; it will just over-write them. It also currently only handles a single producing unit. See @param NPS for details on sub-functions. Additionally, information about the version of EML editor used will be injected into the metadata.
#'
#' @inheritParams set_title
#' @param prod_units A string that is the producing unit Unit Code or a list of unit codes, for example "ROMO" or c("ROMN", "SODN")
#' @param NPS defaults to TRUE. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE. If NPS=TRUE, the originatingAgency will be set to NPS and the field that maps to DataStore's "by or for NPS" will be set to TRUE.
#'
#' @return an EML object
#' @export
#'
#' @examples
#' \dontrun{
#' prod_units<-c("ABCD", "EFGH")
#' set_producing_units(eml_object, prod_units)
#' set_producing_units(eml_object, c("ABCD", "EFGH"))
#' set_producing_units(eml_object, "ABCD")
#' }
set_producing_units<-function(eml_object, prod_units, NPS=TRUE){
  #get existing metadataProvider info, if any:
  doc<-EML::eml_get(eml_object, "metadataProvider")

  #make metadataProvider fields with producing units filled in:
  if(length(prod_units==1)){
  plist<-EML::eml$metadataProvider(organizationName=prod_units)
  }
  if(length(prod_units>1)){
  plist<-NULL
    for(i in seq_along(prod_units)){
      punit<-EML::eml$metadataProvider(organizationName = prod_units[i])
      plist<-append(plist,list(punit))
    }
  }

  #if no existing metadataprovider info:
  if(is.null(doc)){
    eml_object$dataset$metadataProvider<-plist
    cat("No previous producing Units were detected. \nYour new producing units have been added. \nView the current title using get.producingUnits")
  }
  #if there *is* existing metadataProvider info, choose whether to overwrite or not:
  else{
    cat("Your metadata already contain Producing Units (use get.producingUnits to view them). Are you sure you want to replace the existing Producing Units?")
    var1<-readline(prompt="Are you sure you want to replace them? \n\n 1: Yes\n 2: No\n")
    #if User opts to replace metadataProvider, replace it:
    if(var1==1){
      eml_object$dataset$metadataProvider<-plist
      cat("You have replaced your producing Units.\nView the current producing units using get.producingUnits.")
    }
    #if User opts to retain metadataProvider, retain it:
    if(var1==2){
      cat("Your original producing Units were retained.\nView the current producing units using get.producingUnits.")
    }
  }

  #Set NPS publisher, if it doesn't already exist
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}



#' Set the human language used for metadata
#'
#' @description set_language allows the user to specify the language that the metadata (and data) were constructed in. This field is intended to hold the human language, i.e. English, Spanish, Cherokee.
#'
#' @details The English words for the language the data and metadata were constructed in (e.g. "English") is automatically converted to the the 3-letter codes for languages listed in ISO 639-2 (available at https://www.loc.gov/standards/iso639-2/php/code_list.php) and inserted into the metadata.
#'
#' @inheritParams set_title
#'
#' @param lang is a string consisting of the language the data and metadata were constructed in, for example, "English", "Spanish", "Navajo". Capitalization does not matter, but spelling does! The input provided here will be converted to 3-digit ISO 639-2 codes.
#'
#' @param NPS Logical. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE. If NPS=TRUE, the originatingAgency will be set to NPS and the field that maps to DataStore's "by or for NPS" will be set to TRUE.
#'
#' @return eml_object
#' @export
#'
#' @examples
#' \dontrun{
#' set_language(eml_object, "english")
#' set_language(eml_object, "Spanish")
#' set_language(eml_object, "nAvAjO")
#' }
set_language<-function(eml_object, lang, NPS=TRUE){
  #enforces ISO capitalization formatting:
  lang<-stringr::str_to_title(lang)

  #a few common one-off name translations. Probably could improve the dplyr filter to filter for anything containing, but then you run into issues. There are 5 different languages whose English language name includes the word "English" ("English", "English, Old", "Creole and pigeons, English based", etc)).

  if(lang=="Spanish"){
    lang<-"Spanish; Castilian"
  }
  if(lang=="Iroquois"){
    lang<="Iroquoian languages"
  }

  #get ISO language codes
  langcodes<-ISOcodes::ISO_639_2

  #get language code in the ISO language codes?
  nlang<-dplyr::filter(langcodes, Name==lang)[[1]]

  #if the language supplied is not part of ISO 639-2 (spelling?):
  if(!nchar(nlang==3)){
    stop(message("Please check that your language is included in the ISO 639-2 language code. The codes are available at https://www.loc.gov/standards/iso639-2/php/code_list.php"))
  }

  #get current language from the metadata provided:
  lng<-eml_object$dataset$language

  #if there is no language specified in the metadata:
  if(is.null(lng)){
    eml_object$dataset$language<-nlang
    cat("The language has been set to ", nlang, "the ISO 639-2 code for ",lang,".")
  }

  #if the language is already specified in the metadata:
  else{
    if(nchar(lng)==3){
      full_lang<-dplyr::filter(langcodes, Alpha_3_B==lng)[[4]]
      cat("The current language is set to ", crayon::blue$bold(lng),", the ISO 639-2 code for ", full_lang, ".", sep="")
    }
    else{
      cat("The current language is set to ", crayon::blue$bold(lng),".", sep="")
    }

    #does the user want to change the language?
    var1<-readline(prompt="Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")

    #if yes, change the language and report the change:
    if(var1==1){
      eml_object$dataset$language<-nlang
      cat("You have replaced the language with ", crayon::blue$bold(nlang), ", the 3-letter ISO-639-2 code for ", crayon::blue$bold(lang), ".", sep="")
    }

    #if User opts to retain metadataProvider, retain it:
    if(var1==2){
      cat("Your original language was retained.")
    }
  }

  #Set NPS publisher, if it doesn't already exist. Also sets byorForNPS in additionalMetadata to TRUE.
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}


#' Adds a connection to the protocol under which the data were collected
#'
#' @description set_protocol adds a metadata link to the protocol under which the data being described were collected. It automatically inserts a link to the DataStore landing page for the protocol as well as ?????
#'
#' @details set_protocol requires that you have your protocols and projects organized in a specific fashion in DataStore. Errors generated by this function my stem from either a protocol that has not been published (or is not publicly available) or an obsolete protocol/project organization within DataStore.
#'
#' @inheritParams set_title
#' @param protocol_id a string. The 7-digit number identifying the DataStore reference number for the Project that describes your inventory or monitoring project.
#' @param NPS Logical. Checks EML for NPS publisher info and injects it if publisher is empty. If publisher already exists, does nothing. If you are not publishing with NPS, set to FALSE. If NPS=TRUE, the originatingAgency will be set to NPS and the field that maps to DataStore's "by or for NPS" will be set to TRUE.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' set_protocol(eml_object, 2222140)
#' }
#'
set_protocol<-function(eml_object, protocol_id, NPS=TRUE){

  #get data to construct project:

  #get protocol profile via rest services:
  ds_reference <- httr::content(httr::GET(paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/",protocol_id)))

  #extract project title
  proj_title<-ds_reference$bibliography$title

  #generate URL for the DataStore landing page:
  url<-paste0("https://irma.nps.gov/DataStore/Reference/Profile/", protocol_id)

  #get DataStore ref number for the organization Name:
  ref<-ds_reference$series$referenceId

  #rest services call to get organization name info:
  org_name<-httr::content(httr::GET(paste0("https://irmaservices.nps.gov/datastore/v4/rest/Profile/", ref)))$bibliography$title

  #Construct a project to inject into EML. Note 'role' is required but not sure what to put there.
  #Also i find it confusing that onlineURL references projTitle not orgName but hopefully we will hash that out soon.

  proj<-list(title=proj_title,
             personnel=list(organizationName=org_name,
                            onlineUrl=url,
                            role="originator"))

  #get existing project (if any)
  doc<-eml_object$dataset$project

  #if no previous project listed, add project
  if(is.null(doc)){
    eml_object$dataset$project<-proj
    cat("The current project is now ", crayon::bold$blue(proj$title), ".", sep="")
  }

  #if an there is an existing project, ask whether to replace:
  else{
    cat("you already have a project(s) with the Title:\n", crayon::bold$blue(doc$title),".", sep="")

    var1<-readline(prompt="Are you sure you want to replace it? \n\n 1: Yes\n 2: No\n")

    #if yes, change the project:
    if(var1==1){
      eml_object$dataset$project<-proj
      cat("The current project is now ", crayon::bold$blue(proj$title), ".",sep="")
    }

    #if no, retain the existing project:
    if(var1==2){
      cat("Your original project was retained.")
    }

  }

  #Set NPS publisher, if it doesn't already exist. Also sets byorForNPS in additionalMetadata to TRUE.
  if(NPS==TRUE){
    eml_object<-.set_npspublisher(eml_object)
  }

  #add/update EMLeditor and version to metadata:
  eml_object<-.set_version(eml_object)

  return(eml_object)
}




#' Set Publisher
#'
#' @description set_publisher should only be used if the publisher Is **NOT the National Park Service** or if the contact address for the publisher is NOT the central office in Fort Collins. All data packages are published by the Fort Collins office, regardless of where they are collected or uploaded from. If you are working on metadata for a data package, *Do not use this function* unless you are very sure you need to (most NPS users will not want to use this function). If you want the publisher to be anything other than NPS out of the Fort Collins Office, if you want the originating agency to be something other than NPS, _or_ your product is *not* for or by the NPS, use this function.
#'
#' @inheritParams set_title
#' @param org_name String. The organization name that is publishing the digital product. Defaults to "NPS".
#' @param street_address String. The street address where the digital product is published
#' @param city String. The city where the digital product is published
#' @param state String. A two-letter code for the state where the digital product is being published.
#' @param zip_code String. The postal code for the publishers location.
#' @param country String. The country where the digital product is being published.
#' @param URL String. a URL for the publisher.
#' @param email String. an email for the publisher.
#' @param ror_id String. The ROR id for the publisher (see https://ror.org/ for more information).
#' @param for_or_by_NPS Logical. Defaults to TRUE. If your digital product is NOT for or by the NPS, set to FALSE.
#' @param NPS Logical. Defaults to TRUE. Set this to FALSE only if the party responsible for data collection and generation is *not* the NPS *or* the publisher is *not* the NPS central office in Fort Collins.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' set_publisher(eml_object,
#'               "BroadLeaf",
#'               "123 First Street",
#'               "Second City",
#'               "CO",
#'               "12345",
#'               "USA",
#'               "https://www.organizationswebsite.com",
#'               "contact@myorganization.com",
#'               "https://ror.org/xxxxxxxxx",
#'               for_or_by_NPS=FALSE,
#'               NPS=FALSE)
#' }
set_publisher<-function(eml_object,
                        org_name = "NPS",
                        street_address,
                        city,
                        state,
                        zip_code,
                        country,
                        URL,
                        email,
                        ror_id,
                        for_or_by_NPS = TRUE,
                        NPS = TRUE) {

#just in case someone at NPS wants to run this function, it will run .set_npspublisher instead unless they explicitly tell it not to by setting NPS = FALSE. This is an extra safeguard.
  if(NPS == TRUE){
    .set_npspublisher(eml_object)
  }
  else{
    # get existing publisher info for the data package:
    publish <- eml_object$dataset$publisher

    # create desired publisher info:
    pubset <- list(organizationName = org_name,
                   address = list(deliveryPoint = street_address,
                                  city = city,
                                  administrativeArea = state,
                                  postalCode = zip_code,
                                  country = country),
                   onlineUrl = URL,
                   electronicMailAddress = email,
                   userId = list(directory = "https://ror.org/",
                                 userId = ror_id))

    # if existing and new publisher don't match, replace with new publisher
    if (!identical(publish, pubset)) {
      eml_object$dataset$publisher <- pubset
    }

    # set up byOrForNPS:
    for_by <- list(metadata = list(
                      agencyOriginated = list(
                        agency = org_name,
                        byOrForNPS = for_or_by_NPS),
                      id = "agencyOriginated"))
    # access additionalMetadata elements:
    add_meta <- EML::eml_get(eml_object, "additionalMetadata")
    add_meta <- within(add_meta, rm("@context"))

      # if no additionalMetadata, add in EMLeditor and current version:
    if (length(names(add_meta)) == 0) {
      eml_object$additionalMetadata <- for_by
    }

    # if there are existing additionalMetadata elements:
    if (length(names(add_meta)) > 0) {
      x <- length(add_meta)

      # does it include byOrForNPS?
      For_or_by_nps <- NULL
      for (i in seq_along(add_meta)) {
        if (suppressWarnings(stringr::str_detect(add_meta[i], "byOrForNPS"))) {
          For_or_by_nps <- "TRUE"
        }
      }
      # if no info on ForOrByNPS, add ForOrByNPS to additionalMetadata
      if (is.null(For_or_by_nps)) {
        if (x == 1) {
          eml_object$additionalMetadata <- list(for_by,
                                                eml_object$additionalMetadata)
        }
        if (x > 1) {
          eml_object$additionalMetadata[[x + 1]] <- for_by
        }
      }
    }
  }
  return(eml_object)
}
