# Read in original data
library(readr)
data <- read_csv(file.choose(), col_names = FALSE)

# Create empty data frame for transformed data
sex = c(1)
length = c(1)
diameter = c(1)
height = c(1)
wholeWeight = c(1)
shuckedWeight = c(1)
visceraWeight = c(1)
shellWeight = c(1)
rings = data$X9
class = c('A')
HAbalone <- data.frame(sex, length, diameter, height, wholeWeight, shuckedWeight, visceraWeight, shellWeight, rings, class, stringsAsFactors=FALSE)
transformedClass = c()

# Transform the original data 
for(i in 1:nrow(data))
{
  # Convert sex attribute to integer
  if(data[i,1] == 'M')
    HAbalone[i,1] = 2
  else if(data[i,1] == 'F')
    HAbalone[i,1] = 1
  else if(data[i,1] == 'I')
    HAbalone[i,1] = 0
  
  # Calculate z-scores for attributes 2-8
  for(j in 2:8)
  {
    HAbalone[i,j] = ((data[i,j] - mean(data[,j])) /sd(data[,j]))
  }
  
  # Create class attribute based on ring attribute
  # Also create a numeric based class attribute for use with clustering functions
  if(data[i,9] <= 5)
  {
    HAbalone[i,10] = 'A'
    transformedClass[i] = 4
  }
  else if(data[i,9] <= 8)
  {
    HAbalone[i,10] = 'B'
    transformedClass[i] = 3
  }
  else if(data[i,9] <= 11)
  {
    HAbalone[i,10] = 'C'
    transformedClass[i] = 2
  }
  else if(data[i,9] <= 17)
  {
    HAbalone[i,10] = 'D'
    transformedClass[i] = 1
  }
  else
  {
    HAbalone[i,10] = 'E'
    transformedClass[i] = 0
  }
}

# Create numeric only version of HAbalone to be used with clustering algorithms
HAbaloneNumeric = data.frame(HAbalone[1:8], stringsAsFactors=FALSE)