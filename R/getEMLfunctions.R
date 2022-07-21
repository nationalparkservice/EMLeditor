#' returns the first date
#'
#' @description get.beginDate returns the date of the earliest data point in the datapackage
#'
#' @details returns the date from the <beginDate> tag. Although dates should be formatted according to ISO-8601 (YYYY-MM-DD) it will also check for a few other common formats and return the date as a text string: "DD Month YYYY"
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @examples
#' get.beginDate(emlObject)
get.beginDate<-function(emlObject){
  begin<-arcticdatautils::eml_get_simple(emlObject, "beginDate")
  if(is.null(begin)){
    warning("Your metadata lacks a begining date.")
    begin<-NA #to do: test whether NA needs quotes for write.README.
  }
  else{
  begin %>% as.Date %>% format("%d %B %Y")
  }
}

#' returns the last date
#'
#' @description get.endDate returns the date of the last data point in the datapackage
#'
#' @details returns the date from the <endDate> tag. Although dates should be formatted accoriding to ISO-8601 (YYYY-MM-DD) it will also check a few other common formats and return the date as a text string: "DD Month YYYY"
#'
#' #' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text sting
#' @export
#' @examples
#' get.endDate(emlObject)
get.endDate<-function(emlObject){
  end<-arcticdatautils::eml_get_simple(emlObject, "endDate")
  if(is.null(end)){
    warning("Your metadata lacks an ending date.")
    end<-NA #to do: test whether NA needs quotes for write.README.
  }
  else{
  end %>% as.Date %>% format("%d %B %Y")
  }
}


#' returns the abstract
#'
#' @description returns the text from the <abstract> tag.
#'
#' @details returns the text from the <abstract> tag and attempts to clean up common text issues, such as enforcing UTF-8 formatting, getting rid of carriage returns, new lines, <para> and <literalLayout> tags and mucks about with layout, line breaks, etc. IF you see characters you don't like in the abstract, make sure to edit your abstract in a text editor (e.g. Notepad and NOT a Word). You should save the text to a new object and view it using writeLines()
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' abstract<-get.abstract(emlObject)
#' writeLines(abstract)
get.abstract<-function(emlObject){
  doc<-arcticdatautils::eml_get_simple(emlObject, "abstract")
  if(is.null(doc)){
    stop("Your EML lacks an abstract. Use set.abstract() to add one.")
  }

  Encoding(doc)<-"UTF-8" #helps with weird characters
  txt<-NULL
  for(i in 1:length(doc)){
    if(nchar(doc[i])>0){
      mypara <- gsub("[\r?\n|\r]", "", doc[i]) #get rid of line breaks and carriage returns
      mypara <- gsub("&#13;", " ", mypara) #get rid of carriage symbols
      mypara <- gsub("&amp;#13;", ". ", mypara)
      mypara <- gsub("<literalLayout>", "", mypara) #get rid of literalLayout tag
      mypara <- gsub("<para>", "", mypara) #get rid of para tag
      mypara <- gsub("</para>", "", mypara) #get rid of close para tag
      mypara <- gsub("</literalLayout>", "", mypara) #get rid of close par tag
      txt<-paste(txt, mypara, sep="\n\n\t")
    }
  }
  return(txt)
}

#' returns the datapackage title
#'
#' @description returns a text string that is the title of the datapackage
#'
#' @details accesses all of the <title> tags (there can be several, if each file was given a separate title). Assumes that the first instance of <title> referes to the entire datapackage and returns it as a text string, ignoring the contents of all other <title> tags.
#'
#' #' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.title(emlObject)
get.title<-function(emlObject){
  doc<-arcticdatautils::eml_get_simple(emlObject, "title")[1]
}

#' returns the DataStore Reference ID
#'
#' @description get.DSRefID returns the DataStore Reference ID as a string of text.
#'
#' @details accesses the DOI listed in the <alternateIdentifier> tag and trims to to the last 7 digits, which should be identical to the DataStore Reference ID. If the <alternateIdentifier> tag is empty, it notifies the user that there is no DOI associate with the metadata and suggests adding one using set.DOI() (edit.DOI() would also work).
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.DSRefID(emlObject)
get.DSRefID<-function(emlObject){
  pid<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier")
  if(is.null(pid)){
    stop("Your EML lacks a DOI in the \"alternateIdentifier\" tag.\n Please use the set.DOI() function to add your DOI")
  }
  for(i in 1:length(pid)){
    if(stringr::str_detect(pid[i], "doi:10.")){
      doi<-pid[i]
    }
  }
  RefID<-stringr::str_sub(doi, start=-7)
  return(RefID)
}

#' returns the datapackage citation
#'
#' @description returns a Chicago manual of style citation for the datapackage
#'
#' @details allows the user to preview the what the citation will look like. The Harper's Ferry Style Guide recommends using the Chicago Manual of Style for formatting citations. The citation is formatted according to to a modified version of the Chicago Manual of Style's Author-Date journal article format because currently there is no Chicago Manual of Style format specified for datasets or datapackages. In compliance wiht DataCite's recommendations regarding including DOIs in citations, the citation displays the entire DOI as https://www.doi.org/10.58370/xxxxxx".
#'
#' #' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.citation(emlObject)
get.citation<-function(emlObject){
  #assemble the pieces:

  pid<-get.DOI(emlObject)

  authorList<-get.authorList(emlObject)

  title<-get.title(emlObject)

  pubDate<-arcticdatautils::eml_get_simple(emlObject, "pubDate")
  pubDate<-lubridate::parse_date_time(pubDate, orders="Y-m-d")
  pubYear<-lubridate::year(pubDate)

  publisher<-"The U.S National Park Service. "

  location<-"Fort Collins, CO. "
  #print(location)

  #### what to do if no doi ("set" eml?)?

  #print(doi)

  #piece it together:
  data.Citation<-paste0(authorList, " ", pubYear, ". ", title, ". ", publisher, location, doi)

  return(data.Citation)
}

#' returns the authors
#'
#' @description get.authorList returns a text string with all of the authors listed under the <creator> tag.
#'
#' @details get.authorList assumes every author has at least 1 first name (either <givenName> or <givenName1>) and only one last name (<surName>). Middle names (<givenName2>) are optional. The author List is formatted with the last name, comma,  first name for the first author and the fist name, last name for all subsequent authors. The last author's name is preceeded by an 'and'.
#'
#' #' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.authorList(emlObject)
get.authorList<-function(emlObject){
  #get author names & affiliations
  authors<-arcticdatautils::eml_get_simple(emlObject, "creator")
  authors<-unlist(authors)

  #extract givenName; should handle middle names too!
  FirstName<-NULL
  first<-NULL
  for(i in 1:length(authors)){
    if(stringr::str_detect(names(authors)[i], "givenName\\b")){
      FirstName<-append(FirstName, authors[i][[1]])
    }
    else if(stringr::str_detect(names(authors)[i], "givenName1\\b")){
      first_middle<-paste0(authors[i], " ", authors[i+1])
      FirstName<-append(FirstName, first_middle)
    }

    if(length(first>0)){
      first_middle<-paste0(first, " ", middle)
      FirstName<-append(FirstName, first_middle)
    }
  }

  #extract surName
  LastName<-NULL
  for(i in 1:length(authors)){
    if(stringr::str_detect(names(authors)[i], "surName")){
      LastName<-append(LastName, authors[i][[1]])
    }
  }

  #create a single object that is a string consisting of the ith author, formatted according to the Chicago manual of style, Journal article:

  author<-NULL
  Last.First<-NULL
  if(length(LastName)>0){
    #single author:
    if(length(LastName)==1){
      author<-paste0(LastName, ", ", FirstName, ".")
      Last.First<-author
    }

    #multi-author:
    else{
      for(i in 1:length(LastName)){
        if(i==1){
        }
        if(i>1 && i<length(LastName)){
          author<-paste0(FirstName[i], " ", LastName[i])
        }
        if(i>1 && i==length(LastName)){
          author<-paste0("and ", FirstName[i], " ", LastName[i], ".")
        }
        Last.First<-append(Last.First, author)
      }

      #make it a string, not a list:
      Last.First<-toString(Last.First)
    }
  }


}

#' returns the DOI
#'
#' @description returns a text string that is the DOI for the datapackage
#'
#' @details accesses the contents of the<alternateIdentifier> tag and does some text manipulation to return a string with the DOI including the URL and prefaced by 'doi: '.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.DOI(emlObject)
get.DOI<-function(emlObject){
  #where EMLassemblyline stores DOIs.
  pid<-arcticdatautils::eml_get_simple(emlObject, "alternateIdentifier")
  if(is.null(pid)){
    stop("Your EML lacks a DOI in the \"alternateIdentifier\" tag.\n Please use the set.DOI() function to add your DOI")
  }
  #if a DOI exists, report that it already exists and prompt to edit:
  mylist<-NULL
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
  doi<-gsub('doi:', 'https://doi.org/', doi)
}

#' returns the park unit connections
#'
#' @description returns a string with the park unit codes where the data were collected
#'
#' @details accesses the contents of the <geographicDescription> tags and returns the contents of the tag that containst the text "NPS Unit Connections". If there is no <geographicDescription>, it alerts the user and suggests adding park unit connections using the set.parkUnits() function.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.parkUnits(emlObject)
get.parkUnits<-function(emlObject){
  units<-arcticdatautils::eml_get_simple(emlObject, "geographicDescription")
  if(is.null(units)){
    warning("No Park Unit Connections specified. Use the set.parkUnits() function to add Park Unit Connections.")
    punits<-NA #to do: test whether NA needs quotes for write.README.
  }
  else{
  punits<-NULL
    for(i in 1:length(units)){
      if(stringr::str_detect(units[i], "NPS Unit Connections:")){
        punits<-units[i]
      }
    }
    if(is.null(punits)){
      warning("No Park Unit Connections specified. Use the set.parkUnits() function to add Park Unit Connections.")
      punits<-NA #to do: test whether NA needs quotes for write.README.
    }
  }
  return(punits[[1]])
}

#' returns a CUI statement
#'
#' @description get.CUI returns an english-language translation of the CUI codes
#'
#' @details get.CUI accesses the contents of the Controlled Unclassified Information (CUI) tag, <CUI> and returns an appropriate string of english-language text based on the properties of the CUI code. If thee <CUI> tag is empty or does not exist, get.CUI alerts the user and suggests specifying CUI using the set.CUI() funciton.
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.CUI(emlObject)
get.CUI<-function(emlObject){
  cui<-arcticdatautils::eml_get_simple(emlObject, "CUI")
  if(is.null(cui)){
    stop("No CUI specified. Use the set.CUI() function to add a properly formatted CUI code.")
  }
  if(cui=="FED ONLY"){
    cui<-"Contains CUI. Only federal employees should have access (similar to \"internal only\" in DataStore)"
  }
  if(cui=="FEDCON"){
    cui<-"Contains CUI. Only federal employees and federal contractors should have access (also very much like current \"internal only\" setting in DataStore)"
  }
  if(cui=="DL ONLY"){
    cui<-"Contains CUI. Should only be available to a names list of individuals (where and how to list those individuals TBD)"
  }
  if(cui=="NOCON"){
    cui<-"Contains  CUI. Federal, state, local, or tribal employees may have access, but contractors cannot."
  }
  if(cui=="PUBVER"){
    cui<-"Does NOT contain CUI. The original data contained CUI, but in this datapackage CUI have been obscured so that it no longer contains CUI."
  }
  if(cui=="PUBFUL"){
    cui<-"Does NOT contain CUI. The original data contained no CUI. No data were obscured or altered to generate the data package."
  }
  return(cui)
}


#' displays file names, sizes, and descriptions
#'
#' @description returns a plain-text table containing file names, file sizes, and short descriptions of the files.
#'
#' @details returns the file names (listed in the <objectName> tag), the size of the files (listed in the <size> tag) and converts it from bytes (B) to a more easily interpretable unit (KB, MB, GB, etc). Technically this uses powers of 2^10 so that KB is actually a kibibyte (1024 bytes) and not a kilobyte (1000 bytes). Similarly MB is a mebibyte not a megabyte, GB is a gibibyte not a gigabyte, etc. But for most practical purposes this is probably irrelevant. Finally, a short description is provided for each file (from the <entityDescription> tag).
#'
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.fileInfo(emlObject)
#gets & displays the filenames (from objectName) and file descriptions (from entityDescription):
get.fileInfo<-function(emlObject){
  #get file names
  file.name<-arcticdatautils::eml_get_simple(emlObject, "objectName")

  #get file sizes (assumes in bytes)
  filesize<-arcticdatautils::eml_get_simple(emlObject, "size")
  filesize<-suppressWarnings(as.numeric(filesize))
  filebyte<-unique(filesize)
  filebyte<-filebyte[!is.na(filebyte)]
  readable<-gdata::humanReadable(filebyte, standard="Unix") %>%
    paste0("B")

  #get file descriptions
  file.descript<-arcticdatautils::eml_get_simple(emlObject, "entityDescription")

  #generate dataframe for display:
  dat<-data.frame(file.name, readable, file.descript)
  colnames(dat)<-c("FileName", "Size", "Description")

  print("Current filenames and file descriptions:")
  print(dat)
}

#' returns the DOI of the associated DRR
#'
#' @description get.DRRdoi returns a text string with the associated Data Release Report (DRR)'s DOI.
#'
#' @details get.DRRdoi accesses the <useageCitation> tag(s) and searches for the string "DRR: https://doi.org/". If that string is found, the contenst of that tag are returned. If the <useageCitation> tag is empty or not present, the user is informed and pointed to the set.DRRdoi() function to add the DOI of an associated DRR.
#' @param emlObject is an R object imported (typically from an EML-formatted .xml file) using EmL::read_eml(<filename>, from="xml").
#' @return a text string
#' @export
#' @example
#' get.DRRdoi(emlObject)
get.DRRdoi<-function(emlObject){
  doi<-arcticdatautils::eml_get_simple(emlObject, "useageCitation")
  if(is.null(doi)){
    stop("You have not specified a DRR associated with this datapackage. To specify a DRR, use set.DRRdoi().")
  }
  for(i in 1:length(doi)){
    if(stringr::str_detect(doi[i], "DRR: https://doi.org/")){
      DRRdoi<-doi[i]
    }
  }
  return(DRRdoi)
}
