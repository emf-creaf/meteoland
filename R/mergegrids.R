mergegrids<-function(..., verbose = TRUE) {
  l <- list(...)
  if(inherits(l[[1]], "list")) l = l[[1]]
  ng <- length(l)
  mg_1 = l[[1]]
  dates = mg_1@dates
  vars = names(mg_1@data[[1]])
  npx = nrow(mg_1@data[[1]])
  gt_1 = mg_1@grid
  pixels = inherits(mg_1, "SpatialPixelsMeteorology")
  for(i in 2:ng) {
    mg_i <- l[[i]]
    gt_i <- mg_i@grid
    vars_i = names(mg_i@data[[1]])
    if(sum(gt_i@cellcentre.offset==gt_1@cellcentre.offset)<2) stop("Grids have to be congruent.")
    if(sum(gt_i@cellsize==gt_1@cellsize)<2) stop("Grids have to be congruent.")
    if(sum(gt_i@cells.dim==gt_1@cells.dim)<2) stop("Grids have to be congruent.")
    if(!identicalCRS(mg_1,mg_i)) stop("CRSs have to be identical.")
    if(pixels) {
      gi_1 = gt_1@grid.index
      gi_i = gt_i@grid.index
      if(sum(gi_1==gi_i)<length(gi_1)) stop("Grid indices need to be the same.")
    }
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
  if(pixels) {
    points = SpatialPoints(coordinates(mg_1), proj4string = mg_1@proj4string)
    sgm = SpatialPixelsMeteorology(points = points, grid = gt_1, proj4string = mg_1@proj4string, data = data, dates = dates)
  } else {
    sgm = SpatialGridMeteorology(gt_1, proj4string = mg_1@proj4string, data = data, dates = dates)
  }
  return(sgm)
}

mergepoints<-function(..., verbose = TRUE) {
  l <- list(...)
  if(inherits(l[[1]], "list")) l = l[[1]]
  ng <- length(l)
  mg_1 = l[[1]]
  dates = mg_1@dates
  vars = names(mg_1@data[[1]])
  ccmax = mg_1@coords
  for(i in 2:ng) {
    mg_i <- l[[i]]
    vars_i = names(mg_i@data[[1]])
    if(!identicalCRS(mg_1,mg_i)) stop("CRSs have to be identical.")
    dates = c(dates, mg_i@dates)
    vars = c(vars, vars_i)
    ccmax = rbind(ccmax, mg_i@coords)
  }
  dates = sort(unique(dates))
  nt = length(dates)
  vars = unique(vars)
  nptsmax= nrow(ccmax)
  newPts = 0
  cc = matrix(nrow=0, ncol=2)
  colnames(cc) = colnames(ccmax)
  data = vector("list", nptsmax)
  if(verbose) pb = txtProgressBar(1, ng, style=3)
  for(j in 1:ng) {
    if(verbose) setTxtProgressBar(pb, j)
    mg_j = l[[j]]
    npt_j = nrow(mg_j@coords)
    for(i in 1:npt_j) {
      df_i = mg_j@data[[i]]
      #Check if coordinates are new
      cc_i = mg_j@coords[i,]
      if(nrow(cc)>0) {
        w = which(cc[,1]==cc_i[1] & cc[,2]==cc_i[2])
        isnew = (length(w)==0)
      } else {
        isnew = T
      }
      if(isnew) {
        newPts = newPts+1
        #Add coords
        cc = rbind(cc, cc_i)
        rownames(cc)[newPts] = rownames(mg_j@coords)[i]
        #New data frame
        m = matrix(NA, nrow = nt, ncol = length(vars))
        df = as.data.frame(m, row.names=1:nt)
        names(df) <-vars
        row.names(df)<-as.character(dates)
        df[row.names(df_i),names(df_i)] = df_i
        data[[newPts]] = df
      } else {
        #Old data frame
        df = data[[w]]
        df[row.names(df_i),names(df_i)] = df_i
        data[[w]] = df
      }
    }
  }
  data = data[1:newPts]
  rn = rownames(cc)
  if(length(unique(rn))<length(rn)) rownames(cc) = 1:newPts
  spm = SpatialPointsMeteorology(points = SpatialPoints(cc, proj4string = mg_1@proj4string),
                                 data = data, dates = dates)
  return(spm)
}