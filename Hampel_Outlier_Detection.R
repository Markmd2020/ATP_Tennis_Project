hampel_outlier_detection <- function(x, k = 3) {
  med <- median(x, na.rm = TRUE)
  mad_val <- mad(x, na.rm = TRUE)  
  
  threshold <- k * mad_val
  
  outliers <- abs(x - med) > threshold
  return(outliers)
}