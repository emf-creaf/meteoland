#' meteoland: Landscape Meteorology Tools
#'
#' Functions to estimate weather variables at any position of a landscape
#'
#' @name meteoland-package
#' @aliases meteoland meteoland-package
#' @docType package
#' @author \strong{Maintainer}: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' [ORCID](https://orcid.org/0000-0001-7132-2080)
#'
#' Authors: \itemize{
#' \item{ Víctor Granda
#' [ORCID](https://orcid.org/0000-0002-0469-1991)}
#' \item{ Nicolas Martin
#' [ORCID](https://orcid.org/0000-0001-7574-0108)}
#' \item{ Antoine Cabon
#' [ORCID](https://orcid.org/0000-0001-6426-1726)}
#' }
#' @seealso Useful links: \itemize{ \item{
#' \url{https://emf-creaf.github.io/meteoland/index.html}} }
#'
#' @references De Caceres et al., 2018 (\doi{10.1016/j.envsoft.2018.08.003}))
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @importFrom cubelyr tbl_cube
#' @importFrom stats aggregate lm complete.cases cor approx na.omit quantile sd dist var rlnorm
#' @importFrom utils setTxtProgressBar txtProgressBar write.table read.table head tail
#' @importFrom methods as new is slot .valueClassTest coerce
#' @importFrom grDevices topo.colors colorRampPalette
#' @importFrom graphics lines par hist abline
#' @importFrom dplyr .data
#' @importFrom rlang :=
#' @useDynLib meteoland, .registration = TRUE
#' @import sp stars sf
## usethis namespace: end
NULL
