#Default parameters for weather generation
defaultGenerationParams<-function() {
  return(list(
    dry_wet_threshold = 0.3,
    wet_extreme_quantile_threshold = 0.8
  ))
}
