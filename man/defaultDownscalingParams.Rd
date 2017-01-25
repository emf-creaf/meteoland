\encoding{UTF-8}
\name{defaultDownscalingParams}
\alias{defaultDownscalingParams}
\title{
Default downscaling parameters
}
\description{
Returns a list with the default parameterization for staistical downscaling.
}
\usage{
defaultDownscalingParams()
}
\value{
A list with the following items (default values in brackets):
\itemize{
\item{\code{fill_wind [= TRUE]}: A logical flag to fill wind speed values with coarse-scale values when fine-scale data is missing.}
\item{\code{wind_height  [= 10]}: Wind measurement height (in m).}
}
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, Biodiversity and Landscape Ecology Laboratory, Centre Tecnologic Forestal de Catalunya
}
\seealso{\code{\link{MeteorologyInterpolationData}}}

