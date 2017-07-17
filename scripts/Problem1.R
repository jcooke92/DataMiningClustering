purity <- function(clusterassignment, classes, numclusters)
{
  require(matrixStats)
  
  # Create table of cluster assignment versus classes
  t <- table(clusterassignment, classes)
  pure = 0
  total = 0
  outliers = 0
  
  # Count total number of objects
  for(x in t)
    total = total+x
  
  # Count outliers in cluster 0
  for(x in t[1,])
    outliers = outliers+x
  
  # Calculate purity on all clusters except 0
  for(x in apply(t[2:numclusters,], 1, max))
    pure = pure+x
  
  pure = pure*100/(total-outliers)
  outliers = outliers/total*100
  
  results <- c(pure, outliers)
  return(results)
}