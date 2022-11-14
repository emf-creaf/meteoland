#' meteoland: Landscape Meteorology Tools
#' 
#' Functions to estimate weather variables at any position of a landscape [De
#' Caceres et al. (2018) c("\\Sexpr[results=rd]{tools:::Rd_expr_doi(\"#1\")}",
#' "10.1016/j.envsoft.2018.08.003")\Sexpr{tools:::Rd_expr_doi("10.1016/j.envsoft.2018.08.003")}].
#' 
#' 
#' @name meteoland-package
#' @aliases meteoland meteoland-package
#' @docType package
#' @author \strong{Maintainer}: Miquel De Cáceres
#' \email{miquelcaceres@@gmail.com}
#' (\href{https://orcid.org/0000-0001-7132-2080ORCID})
#' 
#' Authors: \itemize{ \item Nicolas Martin
#' (\href{https://orcid.org/0000-0001-7574-0108ORCID}) \item Víctor Granda
#' (\href{https://orcid.org/0000-0002-0469-1991ORCID}) \item Antoine Cabon
#' (\href{https://orcid.org/0000-0001-6426-1726ORCID}) }
#' @seealso Useful links: \itemize{ \item
#' \url{https://emf-creaf.github.io/meteoland/index.html} }
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
#' @importFrom Rcpp evalCpp
#' @importFrom Rcpp sourceCpp
#' @useDynLib meteoland, .registration = TRUE
## usethis namespace: end
NULL
