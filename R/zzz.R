#' @export
 .onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to icesStation\n",
    "Please read the full disclaimer here:\n",
    "  https://github.com/ices-tools-prod/disclaimers/blob/master/Disclaimer_icesStation.txt\n",
    "Please note that this package is under development.\n"
  )
}
