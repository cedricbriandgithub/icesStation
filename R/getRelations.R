#' Get all relations from a vocab object returned by icesVocab::getCodeDetail
#' compatibility version 1.2.0
#' @param st. an object returned by getCodeDetail
#' @return A vector with unique key of relations (as returned by
#' the api in the parents element of the list)
#' @examples
#' \dontrun{
#' tt <- icesVocab::getCodeDetail("Station", 1000)
#' getRelations(st. = tt)
#' # [1] "Station_DTYPE" "PURPM"         "Datasets"      "ISO_3166"
#' # [5] "WLTYP"         "PRGOV"         "EDMO"
#' }
#' @export
getRelations <- function(st.) {
  unique(st.$parents$code_types$Key)
}


#' Get attributes from a vocab object returned by icesVocab::getCodeDetail
#' compatibility icesVocab >= version 1.3.3
#' @param st. an object returned by getCodeDetail
#' @return A list with description and value for attributes
#' @examples
#' \dontrun{
#' tt <- icesVocab::getCodeDetail("Station", 1000)
#' getAttribute(st. = tt)
#' }
#' @export
getAttribute <- function(st.) {
  attribute <- st.$attribute
  stopifnot (length(attribute$description) == length(attribute$value))
  return(attribute)
}

#' Get a vector of values for each relation
#' compatibility version 1.2.0
#' @param st. an object returned by getCodeDetail
#' @param relation_key the parent key returned by getRelations()
#' @return A vector of relation values
#' @examples
#' \dontrun{
#' tt <- icesVocab::getCodeDetail("Station", 1000)
#' relation_key <- getRelations(tt)
#' getListRelations(st. = tt, relation_key. = "ISO_3166") # FI
#' getListRelations(st. = tt, relation_key. = "Station_DTYPE") # EW~EU~NU~CW
#' }
#' @export
getListRelations <- function(st., relation_key.) {
  pos <- which(st.$parents$code_types$Key == relation_key.)
  values <- st.$parents$codes[pos, "Key"]
  if (length(values) > 1) values <- paste0(values, collapse = "~")
  return(values)
}


