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
# Remove the old individual variables that are part of the groups
all_players <- all_players[, !(names(all_players) %in% c("Acceleration", "Sprint Speed", "Dribbling", "Agility", "Balance",
                                                         "Finishing", "Shot Power", "Long Shots", "Volleys", "Penalties",
                                                         "Short Passing", "Long Passing", "Crossing", "Free Kick Accuracy",
                                                         "Positioning", "Vision", "Curve",
                                                         "Interceptions", "Def Awareness", "Standing Tackle", "Sliding Tackle", "Heading Accuracy",
                                                         "OVR", "PAC", "SHO", "PAS", "DEF", "PHY", "DRI",
                                                         "Height", "Weight", "Age", "Weak foot", "Skill moves", "Position"))]

str(all_players)

save(all_players, file = file.path("src/data/new_vars.RData"))