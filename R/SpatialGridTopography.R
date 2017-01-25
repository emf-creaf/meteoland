SpatialGridTopography<-function(grid, elevation, proj4string = CRS(as.character(NA))) {
  if (!is(grid, "SpatialGrid")) sg = SpatialGrid(grid, proj4string)
  else sg = grid
  nrows = sg@grid@cells.dim[1]
  ncols = sg@grid@cells.dim[2]
  cellWidth = sg@grid@cellsize[1]
  cellHeight = sg@grid@cellsize[2]
  slopeVec = .slope(elevation, nrows, ncols, cellWidth, cellHeight)
  aspectVec = .aspect(elevation, nrows, ncols, cellWidth, cellHeight)
  data = data.frame(elevation = elevation, slope = slopeVec, aspect = aspectVec)
  lt = new("SpatialGridTopography",
          grid = sg@grid,
          bbox = sg@bbox,
          proj4string = sg@proj4string,
          data = data)
  return(lt)
}
