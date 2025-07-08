#' Get a table of stations.
#' @param code a vector of station codes
#' @param deprecated logical choosing TRUE or FALSE will only return the
#' deprecated or not deprecated records.
#' @param date A date or character like '2025-01-01' to choose only the station
#' modified after that date
#' @return A data frame of stations.
#' @examples
#' \dontrun{
#' # get all stations entered in the last month
#' station <- getListStation(date=Sys.Date()-as.difftime(4,units="weeks"))
#' # stations 286 and 287 are deprecated, they will not be returned by the following
#' station <- station <- getListStation(code = c(1,286,287), deprecated = FALSE)
#' station <- getListStation(code = 1:10)
#' }
#' @export
getListStation <- function(code = NULL, deprecated = NULL, date = NULL) {
  if (is.null(code)){
  st0 <- icesVocab::getCodeList(code_type ="Station", date = date)
  } else {
    # vectorize getCodeList over code
    list_st0 <- mapply(icesVocab::getCodeList, code, MoreArgs=list(code_type ="Station", date=date), SIMPLIFY = FALSE)
    st0 <- do.call("rbind", list_st0)
  }
  if (!is.null(deprecated)) {
    if (!is.logical(deprecated)) stop("deprecated should be a logical")
    st0 <- st0[st0$Deprecated == deprecated,]
  }
  ll <- mapply(getStationDetail, st0$Key, SIMPLIFY = FALSE)
  station <- do.call("rbind", ll)
  return(station)
}
