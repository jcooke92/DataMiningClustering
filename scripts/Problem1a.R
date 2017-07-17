purity_kmeans <- function(clusterassignment, classes, numclusters)
{
  require(matrixStats)
  
  # Create table of cluster assignment versus classes
  t <- table(clusterassignment, classes)
  pure = 0
  total = 0
  
  # Pick the top objects with largest distance from centroid
  outliers = 25
  
  # Count total number of objects
  for(x in t)
    total = total+x
  
  # Calculate purity on all clusters
  for(x in apply(t[1:numclusters,], 1, max))
    pure = pure+x
  
  pure = pure*100/(total-outliers)
  outliers = outliers/total*100
  
  results <- c(pure, outliers)
  return(results)
}