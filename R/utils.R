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
