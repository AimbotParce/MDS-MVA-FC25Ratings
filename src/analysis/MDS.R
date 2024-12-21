################################################################################
#                        Multidimensional Scaling (MDS)                        #
################################################################################

library(ggplot2)
library(RColorBrewer)
library(FactoMineR)
library(factoextra)
load("src/data/cleansed_data.RData")
summary(all_players)


sample_data <- all_players[sample(nrow(all_players), size = floor(0.1 * nrow(all_players))), ]

# POSITIONS ANALYSIS
numeric_vars <- sample_data[, c("PAC", "SHO", "PAS", "DRI", "DEF", "PHY")]

numeric_vars_scaled <- scale(numeric_vars)

dist_matrix <- dist(numeric_vars_scaled, method = "euclidean")

mds_result <- cmdscale(dist_matrix, eig = TRUE)

x <- mds_result$points[, 1]
y <- mds_result$points[, 2]

print(mds_result$eig)
print(mds_result$GOF)

unique_positions <- unique(sample_data$Position)
num_positions <- length(unique_positions)

color_palette <- brewer.pal(num_positions, "Set3")

sample_data$Position <- factor(sample_data$Position)

mds_df <- data.frame(x = x, y = y, Position = sample_data$Position)

ggplot(mds_df, aes(x = x, y = y, color = Position)) +
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
