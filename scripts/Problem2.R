ordinal_agreement <- function(clusterassignment, classes) 
{
  # Get levels of the clusters
  clusterlevels <- levels(factor(clusterassignment))
  n <- length(clusterlevels)
  b <- 1
  totalOrdinalAgreement <- 0
  populationSize = length(classes)

  # Check if we have a 0 cluster 
  if (clusterlevels[1] == "0") 
  {
    b <- 2 
    populationSize = populationSize - length(classes[clusterassignment==clusterlevels[1]])
  }
  
  
  # Calculate ordinal agreement
  for (i in b:n) 
  {
    cluster <- classes[clusterassignment==clusterlevels[i]]
    clusterSize <- length(cluster)
    clusterOrdinalAgreement <- 0
    if(clusterSize != 1)
      clusterOrdinalAgreement <- 0
    for(j in 1:clusterSize)
    {
      for(k in 1:clusterSize)
      {
        if(j != k)
        {
          individualAgreement = abs(cluster[j] - cluster[k])
          clusterOrdinalAgreement = clusterOrdinalAgreement + individualAgreement
        }
      }
      clusterOrdinalAgreement = clusterOrdinalAgreement / (clusterSize^2 - clusterSize)
      clusterOrdinalAgreement = clusterOrdinalAgreement * (clusterSize / populationSize)
      totalOrdinalAgreement = totalOrdinalAgreement + clusterOrdinalAgreement
    }
  }
  
  return (totalOrdinalAgreement)
}