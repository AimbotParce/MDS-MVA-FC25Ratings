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
ggplot(df, aes(x = x, y = y, color = Position)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "Metric MDS of Football Players",
    x = "Coordinate 1", y = "Coordinate 2", color = "Position"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette)

## LEAGUE

# Filter the top 5 leagues based on the number of players
top_leagues <- names(sort(table(all_players$League), decreasing = TRUE))[1:10]

# Filter the dataset for the top 5 leagues
league_data <- sample_data[sample_data$League %in% top_leagues, ]

# Select relevant numeric variables for MDS analysis (PAC, SHO, PAS, DRI, DEF, PHY)
numeric_vars_league <- league_data[, c("PAC", "SHO", "PAS", "DRI", "DEF", "PHY")]

# Scale the numeric data
numeric_vars_scaled_league <- scale(numeric_vars_league)

# Compute the distance matrix (Euclidean distance)
dist_matrix_league <- dist(numeric_vars_scaled_league, method = "euclidean")

# Perform MDS
mds_result_league <- cmdscale(dist_matrix_league, eig = TRUE)

# Print MDS eigenvalues and goodness-of-fit
print(mds_result_league$eig)
print(mds_result_league$GOF)

# Prepare data frame for ggplot
mds_df_league <- data.frame(
  x = mds_result_league$points[, 1],
  y = mds_result_league$points[, 2],
  League = league_data$League
)

# Load ggplot2 for visualization


# Plot MDS for Leagues
ggplot(mds_df_league, aes(x = x, y = y, color = League)) +
  geom_point(alpha = 0.7) + # Add points with transparency
  labs(
    title = "MDS of Football Players by League",
    x = "Coordinate 1", y = "Coordinate 2", color = "League"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette)


## COUNTRIES
# Filter the top 10 countries based on the number of players
top_countries <- names(sort(table(all_players$Nation), decreasing = TRUE))[1:10]

# Filter the dataset for the top 10 countries
country_data <- sample_data[sample_data$Nation %in% top_countries, ]

# Select relevant numeric variables for MDS analysis (PAC, SHO, PAS, DRI, DEF, PHY)
numeric_vars_country <- country_data[, c("PAC", "SHO", "PAS", "DRI", "DEF", "PHY")]

# Scale the numeric data
numeric_vars_scaled_country <- scale(numeric_vars_country)

# Compute the distance matrix (Euclidean distance)
dist_matrix_country <- dist(numeric_vars_scaled_country, method = "euclidean")

# Perform MDS
mds_result_country <- cmdscale(dist_matrix_country, eig = TRUE)

# Print MDS eigenvalues and goodness-of-fit
print(mds_result_country$eig)
print(mds_result_country$GOF)

# Prepare data frame for ggplot
mds_df_country <- data.frame(
  x = mds_result_country$points[, 1],
  y = mds_result_country$points[, 2],
  Nation = country_data$Nation
)

# Plot MDS for Countries
ggplot(mds_df_country, aes(x = x, y = y, color = Nation)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "MDS of Football Players by Country",
    x = "Coordinate 1", y = "Coordinate 2", color = "Country"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette)


## SEX
# Filter the dataset for Male and Female players
sex_data <- sample_data

# Select relevant numeric variables for MDS analysis (PAC, SHO, PAS, DRI, DEF, PHY)
numeric_vars_sex <- sex_data[, c("PAC", "SHO", "PAS", "DRI", "DEF", "PHY")]

# Scale the numeric data
numeric_vars_scaled_sex <- scale(numeric_vars_sex)

# Compute the distance matrix (Euclidean distance)
dist_matrix_sex <- dist(numeric_vars_scaled_sex, method = "euclidean")

# Perform MDS
mds_result_sex <- cmdscale(dist_matrix_sex, eig = TRUE)

# Print MDS eigenvalues and goodness-of-fit
print(mds_result_sex$eig)
print(mds_result_sex$GOF)

# Prepare data frame for ggplot
mds_df_sex <- data.frame(
  x = mds_result_sex$points[, 1],
  y = mds_result_sex$points[, 2],
  Sex = sex_data$Sex
)

# Define a color palette for Male and Female
color_palette_sex <- c("Male" = "blue", "Female" = "red")

# Plot MDS for Sex
ggplot(mds_df_sex, aes(x = x, y = y, color = Sex)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "MDS of Football Players by Sex",
    x = "Coordinate 1", y = "Coordinate 2", color = "Sex"
  ) +
  theme_minimal() +
  theme(legend.position = "right") +
  scale_color_manual(values = color_palette_sex)
