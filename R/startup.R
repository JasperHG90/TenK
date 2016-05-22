.onAttach <- function(libname, pkgname) {
  # Starts when package is loaded
  if (interactive()) {
    packageStartupMessage('For documentation, see: https://github.com/JasperHG90/TenK/wiki or execute "vignette("TenK")". For information on how to cite this package execute "citation("TenK")"')
  }
}
