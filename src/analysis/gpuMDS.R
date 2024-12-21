################################################################################
#             Multidimensional Scaling (MDS) with GPU acceleration             #
################################################################################

library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(cluster)
library(gpuR)

# Load data already cleansed and scaled
load("src/data/cleansed_data_scaled.RData")
df <- all_players # For commodity, rename the dataset


# Remove Name, which is of character type
factonum <- subset(df, select = -c(Name))
dist_matrix <- daisy(factonum, metric = "gower") # We have mixed data types
gpu_dist_matrix <- gpuMatrix(dist_matrix, type = "float") # Convert to GPU matrix
