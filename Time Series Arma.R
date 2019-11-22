# Time Series ARMA models for Cherry BLossom Website
library(mosaic)

library(xts)

library(tswge)
data("airline")




# Read in the data
Walmart = read.csv(file.choose(),header = TRUE)
# Load the Data
Stor9Item50 = Walmart 

aic5.wge(airline)
