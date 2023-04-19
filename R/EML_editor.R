# set.project<-function(emlObject, ProtocolRef){
# use rest API to fill out all the fields needed for a project. The project is where we hold PROTOCOL information.

# What info is needed? NPS needs just the ref# (user supplied)
# EML schema requires:
# title
# personell
#-role
#-organization name (producing unit?)
# onlineUrl



# requires VPN:
# https://irmaservices.nps.gov/datastore-secure/v4/rest/Profile/2250047




#  if (toupper(Secure)=="TRUE") {
# get HoldingID from the ReferenceID - defaults to the first holding
# RestHoldingInfoURL <- paste0('https://irmaservices.nps.gov/datastore-secure/v4/rest/reference/',ReferenceID,'/DigitalFiles')
# xml <- httr::content(httr::GET(RestHoldingInfoURL, httr::authenticate(":", ":", "ntlm")))
# RestDownloadURL <- paste0("https://irmaservices.nps.gov/datastore-secure/v4/rest/DownloadFile/",xml[[1]]$resourceId)
# HoldingID <- xml[[1]]$resourceId
# DigitalFileType <- xml[[1]]$extension#Qs: can relatedProjects be added later or do they need to be added at the same time?
#  }


# }
