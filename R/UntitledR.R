#FetchElevationAspectSlope <- function(data, lat_col, long_col, wkid_col, agol_username = rstudioapi::showPrompt("Username", "Please enter your AGOL username", default = ""), agol_password = rstudioapi::askForPassword("Please enter your AGOL password"), spatial_res = "FINEST") {
  # Check for missing data

#  row_count <- nrow(data)

#  na_row_count <- nrow(dplyr::filter(data, is.na({{long_col}}) | is.na({{lat_col}})))

#  if (na_row_count > 0) {
#    data %<>% dplyr::filter(!is.na({{long_col}}) & !is.na({{lat_col}}))
#    warning(paste(na_row_count, "rows were omitted due to missing lat/long data"))
#    }

  # Get a token with a headless account

#  token_resp <- httr::POST("https://nps.maps.arcgis.com/sharing/rest/generateToken",
#                           body = list(username = agol_username,
#                                       password = agol_password,
#                                       referer = 'https://irma.nps.gov',
#                                       f = 'json'),
#                           encode = "form")
#  agol_token <- jsonlite::fromJSON(httr::content(token_resp, type="text", encoding = "UTF-8"))


  # On the off chance that the data have multiple wkid's, loop through them and get elevation/aspect/slope for each, then bind results back into a single dataframe

#  data_w_covariates <- tibble::tibble()
#  message("Using AGOL to calculate elevation, aspect, and slope. If you have a large dataset, this could take a while.")
#  for (wkid in unique(data[[rlang::ensym(wkid_col)]])) {
#    message(paste("Fetching data for WKID", wkid))
#    temporary <- ArcGISSummarizeElevation(data, {{lat_col}},
#                                          {{long_col}},
#                                          wkid,
#                                          spatial_res,
#                                          agol_token)
#    data_w_covariates <- rbind(data_w_covariates, temporary) } return(data_w_covariates) }
#}



