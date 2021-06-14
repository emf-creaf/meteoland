\encoding{UTF-8}
\name{extractNetCDF}
\alias{extractNetCDF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extraction of climatic data from NetCDF files (deprecated)
}
\description{
This function reads a set of NetCDF files (one per variable) and extracts data for a set of NetCDF cells that are specified using a boundary box (in lon/lat format) or a set of (x,y) grid indices.
}
\usage{
extractNetCDF(ncdf_files, bbox = NULL, offset = 0, cells = NULL, export = TRUE,
              exportDir = getwd(), exportFormat = "meteoland/txt", mpfilename = "MP.txt")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{ncdf_files}{
Character vector containing files to read
}
  \item{bbox}{
Boundary box (2 x 2 matrix) specifying the limit coordinates of a study area (in lon/lat format).
}
  \item{offset}{A buffer to include NetCDF cells that are at a certain distance around the boundary box.}
  \item{cells}{
A (n x 2) matrix specifying the x and y indices of n cells in a grid.
}
  \item{export}{If \code{export = FALSE} the extracted data is stored in memory. Otherwise the result is written in the disk (using the format specified in \code{exportFormat}).}
  \item{exportFormat}{Export format for meteorological data (see \code{\link{writemeteorologypoint}}).}
  \item{exportDir}{Output directory for extracted meteorology.}
  \item{mpfilename}{The name of the file that will store the meta data describing all written files.}

}
\details{
Function \code{extractNetCDF} first identifies which cells in NetCDF data should be extracted according to \code{bbox} (or the cells are indicated by the user using \code{cells}), and the overall period (days). If neither \code{bbox} or \code{cells} is supplied, then all NetCDF cells will be processed. For each cell to be processed, the function loops over all files (which can describe different variables and time periods) and extracts the corresponding data. The function transforms units to the units used in \code{meteoland}. If specific humidity and mean temperature are available, the function calculates mean relative humidity.

Extracted meteorological data (a data frame with days in rows and meteorological variables in columns) can be stored in an object \code{\link{SpatialPointsMeteorology-class}} or it can be written in the disk (one file per cell). In the latter case, the output format can be chosen and the function also writes a supplementary file containing the meta data (i.e. the coordinates and filename of each file).

Humidity in climate model files is given as specific humidity. This is converted to relative humidity and the conversion may produce values above saturation (>100\%) (see also \code{\link{defaultCorrectionParams}} for the same issue when performing bias correction).
}
\value{
If \code{export = FALSE}, the function returns an object of class \code{\link{SpatialPointsMeteorology-class}} with the meteorological series for each cell (represented by a spatial point). Otherwise the function returns an object of class \code{\link{SpatialPointsDataFrame-class}} containing the meta data of the files written in the disk.
}
\author{
Miquel De \enc{Cáceres}{Caceres} Ainsa, CREAF

Nicolas Martin, INRA-Avignon
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{correctionpoints}}, \code{\link{writemeteorologypointfiles}}, \code{\link{SpatialPointsMeteorology-class}}
}
