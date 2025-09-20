.onLoad <- function(libname, pkgname) {
  # check package version; based on `rvcheck`
  tryCatch({
    url <- paste0("https://raw.githubusercontent.com/NCD-RisC/ncdrisc/master/DESCRIPTION")
    x <- readLines(url)
    remote_version <- gsub("Version:\\s*", "", x[grep('Version:', x)])
    installed_version <- tryCatch(packageVersion('ncdrisc'), error=function(e) NA)
    if (installed_version != remote_version) {
      print_it('Please install the latest version `ncdrisc` package.', 'br_red')
      print_it(paste('Installed version:', installed_version), 'grey')
      print_it(paste('Latest version on GitHub:', remote_version), 'grey')
      print_it('Run devtools::install_github("NCD-RisC/ncdrisc")')
    }
  }, error = function(e) {
    print_it("WARNING - Cannot verify package version: are you offline?", "br_red")}
  )
}

.onAttach <- function(libname, pkgname) {

}
