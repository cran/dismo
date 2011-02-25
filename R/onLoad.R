
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname)
  .maxentRemoveTmpFiles()
  options('rasterExtractWarningGiven' = 10)
}  
