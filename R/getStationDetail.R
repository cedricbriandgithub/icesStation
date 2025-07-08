#' Get a station record
#' @param .code The code of the station
#' @return A table with station values filled. In fields with
#' external relations are collated together for instance if a station
#' returns association with EW, EU and NU for Station_DTYPE these character
#' vectors will be collated together with a `~` returning EW~EU~NU
#' @examples
#' \dontrun{
#' stdetail <- getStationDetail(.code = 1000)
#' }
getStationDetail <- function(.code) {
  st <- icesVocab::getCodeDetail(code_type = "Station", code = .code)
  # getlistparent(st, "Station_DTYPE") # "CS~ES~PB~ZB"
  vec_relation_names <- c("PURPM", "WLTYP", "ISO_3166", "EDMO", "Station_DTYPE", "PRGOV")
  relations <- mapply(FUN = getListRelations, vec_relation_names, MoreArgs = list(st. = st))
  relations <- unlist(relations)
  attribute <- getAttribute(st.=st)

  getvalueattribute <- function(attr, data = attribute){
    pos <- match(attr, data$description)
    if (is.na(pos)) {
      return(NA)
    } else {
      return(data$value[pos])
    }

  }
  Station <- data.frame(
    "Definition" = "Station",
    "HeaderRecord" = "record",
    "Station_Code" = st$detail$Key,
    "Station_Country" = relations["ISO_3166"],
    "Station_Name" = st$detail$Description,
    "Station_LongName" = st$detail$LongDescription,
    "Station_ActiveFromDate" = getvalueattribute("Active From"),
    "Station_ActiveUntilDate" = getvalueattribute("Active Until"),
    "Station_ProgramGovernance" = relations["PRGOV"],
    "Station_StationGovernance" = relations["EDMO"],
    "Station_PURPM" = relations["PURPM"],
    "Station_Geometry" = getvalueattribute("Position"),
    "Station_DataType" = relations["Station_DTYPE"],
    "Station_WLTYP" = relations["WLTYP"],
    "Station_MSTAT" = relations["MSTAT"],
    "Station_Notes" = getvalueattribute("Station Notes"),
    "Station_Deprecated" = st$deprecated,# problem
    "modified" = st$detail$Modified
  )
  return(Station)
}
