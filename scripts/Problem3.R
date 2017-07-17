variance <- function(clusterassignment, numerical) 
{
  # Get levels of the clusters
  clusterlevels <- levels(factor(clusterassignment))
  n <- length(clusterlevels)
  var_weight <- array(dim=c(n, 2))
  population <- 0
  b <- 1
  outliers <- 0
  
  # Check if we have a 0 cluster
  if (clusterlevels[1] == "0") 
  {
    b <- 2
    var_weight[1,] <- c(0, 0)
    outliers <- length(numerical[clusterassignment == "0"])
  }
  else
  {
    outliers = 25
  }
  
  # Calculate variance
  for (i in b:n) 
  {
    cluster <- numerical[clusterassignment==clusterlevels[i]]
    population <- population + length(cluster)
    if (length(cluster) == 1) 
    {
      var_weight[i,] <- c(0, length(cluster))
    } else 
    {
      var_weight[i,] <- c(var(cluster), length(cluster))
    }
  }
  
  percentage <- outliers / (outliers + population)
  
  totalv <- 0
  for (i in 1:n) 
    totalv <- totalv + var_weight[i, 1] * var_weight[i, 2] / population
  
  return (c(totalv, percentage))
}