################################################################################

################################PCA#############################################

################################################################################

library(FactoMineR)
library(factoextra)
library(ggplot2)

load("~/Desktop/MVA PROJECT/MDS-MVA-FC25Ratings/src/data/cleansed_data_scaled.RData")

#Filter DATA to obtain meaning full results:
#1- Remove outliers
outlier_rows <- which(univariate_outlier_count >= 5)

all_players <- all_players[-outlier_rows, ]

# Filter out rows where Position is GK
#all_players <- all_players %>%
#  filter(Position != "GK")

# Perform PCA with supplementary variables
active_data <- all_players[, setdiff(names(all_players), c("Name"))]

active_data <- active_data[, -c(44,45,46)]

# Find indices of supplementary qualitative variables (excluding 'Name')
quali_sup_indices <- which(names(active_data) %in% c("Position", "Weak foot", "Skill moves", 
                                                     "Preferred foot", "Sex"))

# Perform PCA
res.pca <- PCA(active_data, 
                  quali.sup = quali_sup_indices, 
                  scale.unit = TRUE, 
                  graph = FALSE)

# View PCA results
summary(res.pca)

res.pca$var

eig_values <- res.pca$eig
explained_variance <- eig_values[, "percentage of variance"]
cumulative_variance <- eig_values[, "cumulative percentage of variance"]

# Create a data frame for the first 10 components
df <- data.frame(x = 1:10, y = cumulative_variance[1:10])

# Plot the explained variance with fviz_eig, limiting to first 10 components
p <- fviz_eig(res.pca,
              addlabels = FALSE,     
              choice = "variance",
              ncp = 10,
              title= "Cumulative Proportion of Variance Explained")  # Set to show only the first 10 components

# Add the cumulative variance line to the plot
p <- p + 
  geom_point(data = df, aes(x, y), size = 2, color = "#00AFBB") +  # Points for cumulative variance
  geom_line(data = df, aes(x, y), color = "#00AFBB") +            # Line for cumulative variance
  scale_y_continuous(
    sec.axis = sec_axis(~ . )  # Add secondary axis for cumulative variance
  )
  
# Print the final plot
print(p)

fviz_cos2(res.pca, choice = "var", axes = 1:2)

var <- get_pca_var(res.pca) 
head(var$coord) 
head(var$cos2)
head(var$contrib) 

fviz_pca_var(res.pca, col.var ="black")
library("corrplot")

corrplot(var$cos2, is.corr=FALSE)

fviz_contrib(res.pca, choice = "var", axes = 1, top = 25)

fviz_contrib(res.pca, choice = "var", axes = 2, top = 25)

fviz_contrib(res.pca, choice = "var", axes = 3, top = 25)

# Plot PCA variable contributions, showing top 10 variables
create_pca_plot <- function(pca_res, comp1, comp2, title_text) {
  # Create the PCA plot
  p <- fviz_pca_var(pca_res, 
                    col.var = "contrib", 
                    gradient.cols = c("white", "blue", "red"),
                    axes = c(comp1, comp2),        # Specify which components to plot
                    ggtheme = theme_minimal(),
                    col.quali.sup = "blue",
                    repel = TRUE,
                    select.var = list(contrib = 20))  # Select top 20 contributing variables
  p <- p + ggtitle(title_text)
  
  return(p)
}


plot_pc1_pc2 <- create_pca_plot(res.pca, 1, 2, "Component 1 vs Component 2")
print(plot_pc1_pc2)

plot_pc1_pc3 <- create_pca_plot(res.pca, 1, 3, "Component 1 vs Component 3")
print(plot_pc1_pc3)

plot_pc2_pc3 <- create_pca_plot(res.pca, 2, 3, "Component 2 vs Component 3")
print(plot_pc2_pc3)

#POSITIONS
fviz_pca_ind(res.pca,
             geom = "point", 
             habillage = active_data$Position,  # Group by Position
             addEllipses = TRUE,      # Add ellipses for each group
             ellipse.level = 0.95,    # Confidence level for ellipses
             repel = TRUE)            

# Grouping the Position variable into Defensive, Midfield, and Attack
library(dplyr)

# Create a new categorical variable based on Position
active_data1 <- active_data %>%
  mutate(Position_group = factor(case_when(
    Position %in% c("CB", "CDM", "LB", "RB") ~ "Defensive",
    Position %in% c("CAM", "CM", "LM", "RM", "LW", "RW") ~ "Midfield",
    Position %in% c("ST") ~ "Attack",
    Position %in% c("GK") ~ "GoalKeeper",
    TRUE ~ NA_character_  # For any positions not covered
  ), levels = c("Defensive", "Midfield", "Attack", "GoalKeeper")))


active_data1 <- active_data1[, -c(37)]
# Check the summary to see the distribution of the new categorical variable
summary(active_data1$Position_group)

# Find indices of supplementary qualitative variables (excluding 'Name')
quali_sup_indices <- which(names(active_data1) %in% c( "Weak foot", "Skill moves", 
                                                     "Preferred foot", "Sex", "Position_group"))

# Perform PCA
#SEX

res.pca <- PCA(active_data1, 
               quali.sup = quali_sup_indices, 
               scale.unit = TRUE, 
               graph = TRUE)

fviz_pca_ind(res.pca,
             geom = "point", 
             habillage = active_data1$Sex,  # Group by Position
             addEllipses = TRUE,      # Add ellipses for each group
             ellipse.level = 0.95,    # Confidence level for ellipses
             repel = TRUE)       

fviz_pca_ind(res.pca,
             geom = "point", 
             habillage = active_data1$Position_group,  # Group by Position
             addEllipses = TRUE,      # Add ellipses for each group
             ellipse.level = 0.95,    # Confidence level for ellipses
             repel = TRUE)            # Avoid label overlap

# Plot by Position Group with Title
fviz_pca_ind(res.pca,
             geom = "point", 
             habillage = active_data1$Position_group,  # Group by Position
             addEllipses = TRUE,      # Add ellipses for each group
             ellipse.level = 0.95,    # Confidence level for ellipses
             repel = TRUE) +          # Avoid label overlap
  ggtitle("PCA by Position Group (1 & 3)")  # Add a title for this plot

# Plot by Position Group with Title for another component combination


# Skill moves group plot with Title
fviz_pca_ind(res.pca,
             geom = "point", 
             habillage = active_data1$`Skill moves`,  # Group by Skill moves
             addEllipses = TRUE,      # Add ellipses for each group
             ellipse.level = 0.95,    # Confidence level for ellipses
             repel = TRUE) +          # Avoid label overlap
  ggtitle("PCA by Skill Moves")  # Add title for this plot



