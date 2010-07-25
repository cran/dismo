
.onLoad <- function(libname, pkgname) {
  .jpackage(pkgname)
  .maxentRemoveTmpFiles()
}  
