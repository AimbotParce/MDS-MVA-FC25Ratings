#### PROFILING  ####
library(FactoMineR)
load("src/data/cleansed_data_scaled.RData")

profiling <- all_players[, c("OVR", "PAC", "SHO", "PAS", "DRI", "DEF", "PHY", "Age", "Height", "Weight", "Position", "Preferred foot", "Sex")]
#only grouped variables and for categoricals only position and the ones with a low number of factors

res1 <- FAMD(profiling)
summary(res1)

### Hierarchical Clustering on FAMD scores ###
hc_famd<-HCPC(res1, 8)
#Even though the highest relative loss of inertia is achieved at 3 clusters, the high number of variables and the plot of the hierarchical tree makes us choose to get 8 clusters 


## Cluster Descriptions ##
hc_famd$desc.var

