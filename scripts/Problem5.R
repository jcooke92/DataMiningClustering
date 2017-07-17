set.seed(4335)
lowestSSE = .Machine$integer.max 
lowestSSECluster <- 0
numclusters = 0

for(i in 1:20)
{
  k1 <- kmeans(HAbaloneNumeric, 5)
  sse1 = 0 
  for(j in length(k1$withinss))
    sse1 = sse1 + k1$withinss[j]
  
  k2 <- kmeans(HAbaloneNumeric, 10)
  sse2 = 0
  for(j in length(k2$withinss))
    sse2 = sse2 + k2$withinss[j]
  
  if(sse1 < lowestSSE)
  {
    lowestSSE = sse1
    lowestSSECluster <- k1
    numclusters = 5
  }
  
  if(sse2 < lowestSSE)
  {
    lowestSSE = sse2
    lowestSSECluster <- k2
    numclusters = 10
  }
}

print(lowestSSE)

purity_kmeans(lowestSSECluster$cl, HAbalone$rings, numclusters)
ordinal_agreement(lowestSSECluster$cl, HAbalone$rings)
variance(lowestSSECluster$cl, HAbalone$rings)

purity_kmeans(lowestSSECluster$cl, transformedClass, numclusters)
ordinal_agreement(lowestSSECluster$cl, transformedClass)
variance(lowestSSECluster$cl, transformedClass)