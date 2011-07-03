
.onLoad <- function(libname, pkgname) { 
  .maxentRemoveTmpFiles()
  options('rasterExtractWarningGiven' = 10)
}  
