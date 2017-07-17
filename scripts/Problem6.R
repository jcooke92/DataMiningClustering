library(dbscan)

# Not enough clusters
db <- dbscan(complex9[1:2], 5, 8)

# Too many clusters
db <- dbscan(complex9[1:2], 5, 6)

# Too many clusters. Obviously not going to get good clustering from eps=5
db <- dbscan(complex9[1:2], 5, 7)

# Too many clusters
db <- dbscan(complex9[1:2], 8, 10)

# Number of clusters is ok but clustering itself is terrible. Not going to get good clustering from eps=8
db <- dbscan(complex9[1:2], 8, 12)
plot(complex9[,1:2], xlab = c("Purity: ", purity(db$cl, complex9[,3], 7)) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[db$cl], main = "DBSCAN with 7 clusters")

# Again, number of clusters is fine but clustering itself is awful. Not going to get good clustering from high minpts
db <- dbscan(complex9[1:2], 10, 15)
plot(complex9[,1:2], xlab = c("Purity: ", purity(db$cl, complex9[,3], 12)) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[db$cl], main = "DBSCAN with 12 clusters")

# Very good clustering results except for one circle object. 
db <- dbscan(complex9[1:2], 12, 4)
plot(complex9[,1:2], xlab = c("Purity: ", purity(db$cl, complex9[,3], 10)) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[db$cl], main = "DBSCAN with 10 clusters")

# Ok results but has lower purity than previous clustering and does not cluster the oval well
db <- dbscan(complex9[1:2], 14, 10)
plot(complex9[,1:2], xlab = c("Purity: ", purity(db$cl, complex9[,3], 10)) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[db$cl], main = "DBSCAN with 10 clusters")

# Only 1 cluster
db <- dbscan(HAbaloneNumeric, 12, 4)

# Good amount of clusters once eps < 1
db <- dbscan(HAbaloneNumeric, 0.4, 4)

db <- dbscan(HAbaloneNumeric, 0.7, 4)
ordinal_agreement(db$cl, transformedClass)

db <- dbscan(HAbaloneNumeric, 0.7, 8)
ordinal_agreement(db$cl, transformedClass)

db <- dbscan(HAbaloneNumeric, 0.7, 12)
ordinal_agreement(db$cl, transformedClass)

db <- dbscan(HAbaloneNumeric, 0.7, 3)
ordinal_agreement(db$cl, transformedClass)