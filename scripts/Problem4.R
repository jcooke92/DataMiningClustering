library(readr)
library(MASS)

complex9 <- read_csv(file.choose(), col_names = FALSE)

k <- kmeans(complex9[,1:2], 9)
purity <- purity_kmeans(k$cl, complex9[,3], 9)
print(purity)
plot(complex9[,1:2], xlab = c("Purity: ", purity[1]) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange")[k$cl], main = "First Kmeans with 9 clusters")

k <- kmeans(complex9[,1:2], 9)
purity <- purity_kmeans(k$cl, complex9[,3], 9)
print(purity)
plot(complex9[,1:2], xlab = c("Purity: ", purity[1]) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange")[k$cl], main = "Second Kmeans with 9 clusters")

k <- kmeans(complex9[,1:2], 13)
purity <- purity_kmeans(k$cl, complex9[,3], 9)
print(purity)
plot(complex9[,1:2], xlab = c("Purity: ", purity[1]) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[k$cl], main = "First Kmeans with 13 clusters")

k <- kmeans(complex9[,1:2], 13)
purity <- purity_kmeans(k$cl, complex9[,3], 9)
print(purity)
plot(complex9[,1:2], xlab = c("Purity: ", purity[1]) , col = c("yellow","green","blue", "red","forestgreen", "brown", "black", "purple", "orange", "gold", "magenta", "yellowgreen", "cyan")[k$cl], main = "Second Kmeans with 13 clusters")
