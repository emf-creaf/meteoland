mergegrids<-function(..., verbose = TRUE) {
  l <- list(...)
  ng <- length(l)
  mg_1 = l[[1]]
  dates = mg_1@dates
  vars = names(mg_1@data[[1]])
  npx = nrow(mg_1@data[[1]])
  gt_1 = mg_1@grid
  for(i in 2:ng) {
    mg_i <- l[[i]]
    gt_i <- mg_i@grid
    vars_i = names(mg_i@data[[1]])
    if(sum(gt_i@cellcentre.offset==gt_1@cellcentre.offset)<2) stop("Grids have to be congruent.")
    if(sum(gt_i@cellsize==gt_1@cellsize)<2) stop("Grids have to be congruent.")
    if(sum(gt_i@cells.dim==gt_1@cells.dim)<2) stop("Grids have to be congruent.")
    if(!identicalCRS(mg_1,mg_i)) stop("CRSs have to be identical.")
    dates = c(dates, mg_i@dates)
    vars = c(vars, vars_i)
  }
  dates = sort(unique(dates))
  nt = length(dates)
  vars = unique(vars)
  data = vector("list", nt)
  names(data) = as.character(dates)
  if(verbose) pb = txtProgressBar(1, nt, style=3)
  for(i in 1:nt) {
    if(verbose) setTxtProgressBar(pb, i)
    m = matrix(NA, nrow = npx, ncol = length(vars))
    df = as.data.frame(m, row.names=1:npx)
    names(df) <-vars
    for(j in 1:ng) {
      mg_j = l[[j]]
      if(as.character(dates[j]) %in% as.character(mg_j@dates)) {
        df_j = mg_j@data[[as.character(dates[j])]]
        df[names(df_j)] = df_j
      }
    }
    data[[i]] = df
  }
  sgm = SpatialGridMeteorology(gt_1, proj4string = mg_1@proj4string, data = data, dates = dates)
  return(sgm)
}