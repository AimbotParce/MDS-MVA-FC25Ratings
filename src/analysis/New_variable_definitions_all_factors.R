load("~/Desktop/MVA PROJECT/MDS-MVA-FC25Ratings/src/data/cleansed_data.RData")

all_players$'Skill moves'<- as.numeric(all_players$'Skill moves')
all_players$'Weak foot'<- as.numeric(all_players$'Weak foot')

# Create new variables by calculating the mean of related variables manually
all_players$Speed_Movement <- rowMeans(all_players[, c("Acceleration", "Sprint Speed", "Agility", "Dribbling", "Balance")], na.rm = TRUE)
all_players$Shooting <- rowMeans(all_players[, c("Finishing", "Shot Power", "Long Shots", "Volleys", "Penalties")], na.rm = TRUE)
all_players$Passing <- rowMeans(all_players[, c("Short Passing", "Long Passing", "Crossing", "Free Kick Accuracy")], na.rm = TRUE)
all_players$Tactical_Play <- rowMeans(all_players[, c("Positioning", "Vision", "Curve")], na.rm = TRUE)
all_players$Defense <- rowMeans(all_players[, c("Interceptions", "Def Awareness", "Standing Tackle", "Sliding Tackle", "Heading Accuracy")], na.rm = TRUE)
all_players$Overall_Rating <- rowMeans(all_players[, c("OVR", "PAC", "SHO", "PAS", "DEF", "DRI", "PHY")], na.rm = TRUE)
all_players$Physical_Profile <- rowMeans(all_players[, c("Height", "Weight", "Age")], na.rm = TRUE)
all_players$Pos <- ifelse(all_players$Position %in% c("CB", "LB", "RB", "CDM", "GK"), 
                          "Defense", 
                          ifelse(all_players$Position %in% c("CM", "CDM", "CAM", "LM", "RM"), 
                                 "Midfield", 
                                 "Attack"))

# Function to quantize a variable into three categories (Low, Mid, High)
quantize_variable <- function(x) {
  quantiles <- quantile(x, probs = c(1/3, 2/3), na.rm = TRUE)
  
  # Handle cases where quantiles might be identical
  if (quantiles[1] == quantiles[2]) {
    quantiles[2] <- quantiles[1] + 1 # Add a small value to separate categories
  }
  
  cut(x, breaks = c(-Inf, quantiles[1], quantiles[2], Inf),
      labels = c("Low", "Mid", "High"), right = FALSE, include.lowest = TRUE)
}

# Apply the function to each variable and create new columns
all_players$Speed_Movement <- quantize_variable(all_players$Speed_Movement)
all_players$Shooting <- quantize_variable(all_players$Shooting)
all_players$Passing <- quantize_variable(all_players$Passing)
all_players$Tactical_Play <- quantize_variable(all_players$Tactical_Play)
all_players$Defense <- quantize_variable(all_players$Defense)
all_players$Overall_Rating <- quantize_variable(all_players$Overall_Rating)
all_players$Physical_Profile <- quantize_variable(all_players$Physical_Profile)

all_players$Reactions_ <- quantize_variable(all_players$Reactions)
all_players$"Ball Control" <- quantize_variable(all_players$"Ball Control")
all_players$Composure <- quantize_variable(all_players$Composure)
all_players$Jumping <- quantize_variable(all_players$Jumping)
all_players$Stamina <- quantize_variable(all_players$Stamina)
all_players$Strength <- quantize_variable(all_players$Strength)
all_players$Aggression <- quantize_variable(all_players$Aggression)

all_players$Pos <- factor(all_players$Pos)

# Remove the old individual variables that are part of the groups
all_players <- all_players[, !(names(all_players) %in% c("Name", "Acceleration", "Sprint Speed", "Dribbling", "Agility", "Balance",
                                                         "Finishing", "Shot Power", "Long Shots", "Volleys", "Penalties",
                                                         "Short Passing", "Long Passing", "Crossing", "Free Kick Accuracy",
                                                         "Positioning", "Vision", "Curve",
                                                         "Interceptions", "Def Awareness", "Standing Tackle", "Sliding Tackle", "Heading Accuracy",
                                                         "OVR", "PAC", "SHO", "PAS", "DEF", "PHY", "DRI",
                                                         "Height", "Weight", "Age", "Weak foot", "Skill moves", "Position",
                                                         "Rank"))]

str(all_players)

save(all_players, file = file.path("src/data/new_vars_all_factors.RData"))

