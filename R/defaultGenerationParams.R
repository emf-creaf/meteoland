#Default parameters for weather generation
defaultGenerationParams<-function() {
  return(list(
    conditional = "none", 
    dry_wet_threshold = 0.3,
    wet_extreme_quantile_threshold = 0.8,
    n_knn_annual = 100,
    adjust_annual_precip = TRUE,
    range_size_days = 5,
    range_size_years = 12,
    min_ratio = 0.7,
    max_ratio = 1.3
  ))
}
