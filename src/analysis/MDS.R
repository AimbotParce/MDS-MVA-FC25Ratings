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


# Remove Name, which is of character type
factonum <- subset(df, select = -c(Name))
dist_matrix <- daisy(factonum, metric = "gower") # We have mixed data types
mds_result <- cmdscale(dist_matrix, eig = TRUE) # Perform MDS with k=2

# Check whether the MDS is representative of the data
print(mds_result$eig)
print(mds_result$GOF) # We expect a low GOF, as there are a lot of variables and we're reducing them to 2

# Extract x and y points and add them to the dataset
df$mds.x <- mds_result$points[, 1]
df$mds.y <- mds_result$points[, 2]


# From this point on, we'll plot the results, coloring by different factors
# such as Position or League, to see if there are any patterns.

# Plot the MDS with Position as color
color_palette <- brewer.pal(length(unique(df$Position)), "Set3")
ggplot(df, aes(x = mds.x, y = mds.y, color = Position)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Coordinate 1", y = "Coordinate 2", color = "Position"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette)
ggsave("src/img/MDS_Position.png")


# Plot the MDS with League as color
ggplot(df, aes(x = mds.x, y = mds.y, color = League)) +
  geom_point(alpha = 0.7, show.legend=FALSE) + # Add points with transparency
  labs(
    x = "Coordinate 1", y = "Coordinate 2"
  ) +
  theme_minimal()
ggsave("src/img/MDS_League.png")

# Plot MDS with Countries as color
ggplot(df, aes(x = mds.x, y = mds.y, color = Nation)) +
  geom_point(alpha = 0.7, show.legend=FALSE) +
  labs(
    x = "Coordinate 1", y = "Coordinate 2", color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right") 
ggsave("src/img/MDS_Countries.png")

# Plot MDS with Sex as color
color_palette <- brewer.pal(2, "Dark2")
ggplot(df, aes(x = mds.x, y = mds.y, color = Sex)) +
  geom_point(alpha = 0.7) +
  labs(
    x = "Coordinate 1", y = "Coordinate 2", color = "Gender"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette)
ggsave("src/img/MDS_Gender.png")
