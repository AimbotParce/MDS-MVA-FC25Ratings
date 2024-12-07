################################################################################

################################PCA#############################################

################################################################################

library(FactoMineR)
library(factoextra)

load("~/Desktop/MVA PROJECT/MDS-MVA-FC25Ratings/src/data/cleansed_data.RData")

# Perform PCA with supplementary variables
active_data <- all_players[, setdiff(names(all_players), c("Name"))]

# Find indices of supplementary qualitative variables (excluding 'Name')
quali_sup_indices <- which(names(active_data) %in% c("Position", "Weak foot", "Skill moves", 
                                                     "Preferred foot", "Nation", "League", "Team", "Sex"))

# Perform PCA
pca_result <- PCA(active_data, 
                  quali.sup = quali_sup_indices, 
                  scale.unit = TRUE, 
                  graph = FALSE)

# View PCA results
summary(pca_result)

# Plot individuals with supplementary variable contributions
fviz_pca_ind(pca_result, habillage = which(sapply(active_data, is.factor)))

# Plot supplementary variables
fviz_pca_var(pca_result, repel = TRUE)


####POSITION####

# Perform PCA with supplementary variables
active_data <- all_players[, setdiff(names(all_players), c("Name", "Weak foot", "Skill moves", 
                                                           "Preferred foot", "Nation", "League", "Team", "Sex"))]

# Find indices of supplementary qualitative variables (excluding 'Name')
quali_sup_indices <- which(names(active_data) == c("Position"))

# Perform PCA
pca_result <- PCA(active_data, 
                  quali.sup = quali_sup_indices, 
                  scale.unit = TRUE, 
                  graph = FALSE)

# Plot individuals with supplementary variable contributions
fviz_pca_ind(pca_result, 
             habillage = quali_sup_indices,  # Color points by the categorical variable
             label = "none",                 # Hide labels (numbers)
             addEllipses = TRUE,             # Optionally add ellipses for groups
             palette = "jco",                # Custom color palette (optional)
             legend.title = "Category") 


####WEAK FOOT####
# Filter active data by excluding categorical variables like 'Name' and 'Weak foot'
active_data_weakfoot <- all_players[, setdiff(names(all_players), c("Name", "Position", "Skill moves", 
                                                                    "Preferred foot", "Nation", "League", "Team", "Sex"))]

# Find index for the 'Weak foot' supplementary variable
quali_sup_weakfoot <- which(names(active_data_weakfoot) == "Weak foot")

# Perform PCA
pca_result_weakfoot <- PCA(active_data_weakfoot, 
                           quali.sup = quali_sup_weakfoot, 
                           scale.unit = TRUE, 
                           graph = FALSE)

# Visualize individuals (with color based on 'Weak foot')
fviz_pca_ind(pca_result_weakfoot, 
             habillage = quali_sup_weakfoot,  # Color points by 'Weak foot'
             label = "none",                  # Hide labels (numbers)
             addEllipses = TRUE,              # Optionally add ellipses for groups
             palette = "jco",                 # Custom color palette
             legend.title = "Weak foot")      # Customize legend title

####SKILL MOVES####
# Filter active data by excluding categorical variables like 'Name' and 'Skill moves'
active_data_skillmoves <- all_players[, setdiff(names(all_players), c("Name", "Position", "Weak foot", 
                                                                      "Preferred foot", "Nation", "League", "Team", "Sex"))]

# Find index for the 'Skill moves' supplementary variable
quali_sup_skillmoves <- which(names(active_data_skillmoves) == "Skill moves")

# Perform PCA
pca_result_skillmoves <- PCA(active_data_skillmoves, 
                             quali.sup = quali_sup_skillmoves, 
                             scale.unit = TRUE, 
                             graph = FALSE)

# Visualize individuals (with color based on 'Skill moves')
fviz_pca_ind(pca_result_skillmoves, 
             habillage = quali_sup_skillmoves,  # Color points by 'Skill moves'
             label = "none",                   # Hide labels (numbers)
             addEllipses = TRUE,               # Optionally add ellipses for groups
             palette = "jco",                  # Custom color palette
             legend.title = "Skill moves")     # Customize legend title

####LEAGUE####
# Filter active data by excluding categorical variables like 'Name' and 'League'
active_data_league <- all_players[, setdiff(names(all_players), c("Name", "Position", "Weak foot", 
                                                                  "Skill moves", "Preferred foot", "Nation", 
                                                                  "Team", "Sex"))]

# Find index for the 'League' supplementary variable
quali_sup_league <- which(names(active_data_league) == "League")

# Perform PCA
pca_result_league <- PCA(active_data_league, 
                         quali.sup = quali_sup_league, 
                         scale.unit = TRUE, 
                         graph = FALSE)

# Visualize individuals (with color based on 'League')
fviz_pca_ind(pca_result_league, 
             habillage = quali_sup_league,  # Color points by 'League'
             label = "none",                # Hide labels (numbers)
             addEllipses = TRUE,            # Optionally add ellipses for groups
             palette = "jco",               # Custom color palette
             legend.title = "League")       # Customize legend title

#### TEAM ####
# Filter active data by excluding categorical variables like 'Name' and 'Team'
active_data_team <- all_players[, setdiff(names(all_players), c("Name", "Position", "Weak foot", 
                                                                "Skill moves", "Preferred foot", "Nation", 
                                                                "League", "Sex"))]

# Perform PCA
pca_result_team <- PCA(active_data_team, 
                       quali.sup = quali_sup_team, 
                       scale.unit = TRUE, 
                       graph = FALSE)

# Visualize individuals (with color based on 'Team')
fviz_pca_ind(pca_result_team, 
             habillage = quali_sup_team,  # Color points by 'Team'
             label = "none",              # Hide labels (numbers)
             addEllipses = TRUE,          # Optionally add ellipses for groups
             palette = "jco",             # Custom color palette
             legend.title = "Team")       # Customize legend title


#### SEX ####
# Filter active data by excluding categorical variables like 'Name' and 'Sex'
active_data_sex <- all_players[, setdiff(names(all_players), c("Name", "Position", "Weak foot", 
                                                               "Skill moves", "Preferred foot", "Nation", 
                                                               "League", "Team"))]

# Find index for the 'Sex' supplementary variable
quali_sup_sex <- which(names(active_data_sex) == "Sex")

# Perform PCA
pca_result_sex <- PCA(active_data_sex, 
                      quali.sup = quali_sup_sex, 
                      scale.unit = TRUE, 
                      graph = FALSE)

# Visualize individuals (with color based on 'Sex')
fviz_pca_ind(pca_result_sex, 
             habillage = quali_sup_sex,  # Color points by 'Sex'
             label = "none",             # Hide labels (numbers)
             addEllipses = TRUE,         # Optionally add ellipses for groups
             palette = "jco",            # Custom color palette
             legend.title = "Sex")       # Customize legend title












