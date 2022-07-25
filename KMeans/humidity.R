# Andrew Zalesak
# July 25th 2022
# with tutorial from https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/

# Load packages
library(stats)
library(factoextra)

# Load data
df <- read.csv("geoMap.csv")

# Clean data
colnames(df)[2] <- "humidifier"
colnames(df)[3] <- "dehumidifier"
colnames(df)[4] <- "black.mold"
rownames(df) <- df$Region
df <- subset(df, select = -Region)

# Scale data
dd <- scale(df)

# Perform k-means clustering
set.seed(123)
km.res <- kmeans(dd, 7, nstart = 25)
print(km.res)

# Visualize clusters
fviz_cluster(km.res, data=df)

# Problem: labels overlap each other and become illegible