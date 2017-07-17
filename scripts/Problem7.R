find_best_clustering <- function(data)
{ 
  acceptanceProbability <- function(neighbor, newNeighbor, temp, startTemp)
  {
    result = 0.0
    
    # If the new neighbor is better than the old one, accept it
    if(newNeighbor < neighbor)
      result = 1.0
    # Otherwise we accept the new neighbor with some probability
    else
      result = ((neighbor - newNeighbor) + (temp/startTemp^2))*(temp/startTemp)
    
    return (result)
  }

  # Transform the data by modifying a column with a random weight
  transformData <- function(data, column, weight)
  {
    data[, i] = data[, i] * weight

    return (data)
  }
  
  seed = sample(1:10000, 1, TRUE)
  set.seed(seed)
  currentData <- data
  transformedData <- data
  k <- kmeans(data, 6)
  kNew <- k
  cooldownRate = 0.7
  weights = c(1, 1, 1, 1, 1, 1, 1, 1)
  variances = c()

  # Simulated annealing on each attribute in the dataset
  for(i in 1:ncol(data))
  {
    temp = 50
    startTemp = temp
    while(temp > 1)
    {
      w = runif(1, 0.0, 1.0)
      transformedData <- transformData(currentData, i, w)
      # Have to use MacQueen algorithm here or else too much memory is used and program fails to complete
      # Also have to increase max iterations and sometimes with only 10 iterations kmeans does not converge
      kNew <- kmeans(transformedData, 6, algorithm="MacQueen", iter.max=100)
      neighbor = variance(k$cl, HAbalone$rings)
      newNeighbor = variance(kNew$cl, HAbalone$rings)
      if(acceptanceProbability(neighbor[1], newNeighbor[1], temp, startTemp) >= runif(1, 0.0, 1.0))
      {
        k <- kNew
        currentData <- transformedData
        weights[i] = w
        variances = c(variances, newNeighbor[1])
      }
      
      temp = temp - cooldownRate
      
      gc()
    }
  }
  
  print(c("Seed: ", seed))
  print(c("Weight vector: ", weights))
  print(c("Variance: ", variance(k$cl, HAbalone$rings)[1]))
  plot(variances)
  return (k)
}