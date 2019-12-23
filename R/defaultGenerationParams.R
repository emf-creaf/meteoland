#Default parameters for weather generation
defaultGenerationParams<-function() {
  return(list(
    conditional = TRUE, 
    dry_wet_threshold = 0.3,
    wet_extreme_quantile_threshold = 0.8,
    n_knn_annual = 100,
    adjust_annual_precip = TRUE,
    min_ratio = 0.9,
    max_ratio = 1.2
  ))
}
