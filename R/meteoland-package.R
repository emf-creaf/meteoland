#' meteoland: Landscape Meteorology Tools
#' 
#' Functions to estimate weather variables at any position of a landscape [De
#' Caceres et al. (2018)](https://doi.org/10.1016/j.envsoft.2018.08.003).
#' 
#' @name meteoland-package
#' @aliases meteoland meteoland-package
#' @docType package
#' @author \strong{Maintainer}: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#' 
#' Authors: \itemize{
#' \item{ Nicolas Martin
#' [ORCID](https://orcid.org/0000-0001-7574-0108)}
#' \item{ Víctor Granda
#' [ORCID](https://orcid.org/0000-0002-0469-1991)}
#' \item{ Antoine Cabon
#' [ORCID](https://orcid.org/0000-0001-6426-1726)}
#' }
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/meteoland/index.html}} }
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @useDynLib meteoland, .registration = TRUE
## usethis namespace: end
NULL
