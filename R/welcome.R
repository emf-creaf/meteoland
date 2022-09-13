.onAttach <- function(lib, pkg)  {
  packageStartupMessage("Package 'meteoland' [ver. ",
                        utils::packageDescription("meteoland",
                                                  fields="Version"),"]",
                        appendLF = TRUE)
}
