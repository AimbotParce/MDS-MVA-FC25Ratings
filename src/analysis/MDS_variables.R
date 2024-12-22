################################################################################
#                        Multidimensional Scaling (MDS)                        #
################################################################################

library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
library(cluster)

# Load data already cleansed and scaled
load("src/data/cleansed_data_scaled.RData")
df <- all_players # For commodity, rename the dataset


# Keep only the numeric variables
numeric <- Filter(is.numeric, df)

# I want to plot an MDS for the variables, instead of the points.
corr_matrix <- abs(cor(numeric, use = "pairwise.complete.obs"))
# This will be considered a similarity measure, let's get the dissimilarity
# To do this, we use formula d^2(x, y) = s(x, x) + s(y, y) - 2s(x, y)
# We know that the diagonal of correlation is necessarily 1, so we can simplify
# the formula to d^2(x, y) = 2(1 - s(x, y))
diss_matrix = sqrt(2 * (1 - corr_matrix))


# Now compute the MDS
mds_result <- cmdscale(diss_matrix, eig = TRUE) # Perform MDS with k=2

# Check whether the MDS is representative of the data
print(mds_result$eig)
print(mds_result$GOF)
# This is not a good GOF, but we're reducing a lot of variables to 2 dimensions


# Compute the STRESS for the report
diss_original <- as.matrix(diss_matrix)
diss_mds <- as.matrix(dist(mds_result$points))
stress <- sqrt(sum((diss_original - diss_mds)^2) /sum(diss_original^2))
print(stress)

# Create a dataframe with the MDS results
df <- data.frame(x = mds_result$points[,1], y = mds_result$points[,2], variable = colnames(numeric))

# From this point on, we'll plot the results, coloring by different factors
# such as Position or League, to see if there are any patterns.

# Plot the MDS with Position as color
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Coordinate 1", y = "Coordinate 2", color = "Position"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  # Add labels
  geom_text(aes(label = variable), nudge_x = 0, nudge_y = 0.025)

ggsave("src/img/MDS_Variables.png")


