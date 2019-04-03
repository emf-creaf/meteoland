\encoding{UTF-8}
\name{correction_lowlevel}
\alias{correction_series}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Low-level correction functions
}
\description{
Low-level function to perform bias correction. 
}
\usage{
correction_series(obs, mod, proj = NULL, method = "unbias", isPrec=TRUE, qstep=0.01)
}

\arguments{
  \item{obs}{Observed series for the reference (historical) period.}
  \item{mod}{Modelled series for the reference (historical) period.}
  \item{proj}{Modelled series for the projected period. If missing, the reference (historical) period is corrected.}
  \item{method}{Correction method, either \code{"unbias"}, \code{"scaling"}, \code{"quantmap"}}
  \item{isPrec}{A flag to indicate that variable is precipitation (only relevant for quantile mapping).}
  \item{qstep}{Probability step for quantile mapping (see \code{\link{defaultCorrectionParams}}).}
}

\value{
Returns a vector with corrected values.
}
\references{
De Caceres M, Martin-StPaul N, Turco M, Cabon A, Granda V (2018) Estimating daily meteorological data and downscaling climate models over landscapes. Environmental Modelling and Software 108: 186-196.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{correctionpoints}}, \code{\link{defaultCorrectionParams}}}
