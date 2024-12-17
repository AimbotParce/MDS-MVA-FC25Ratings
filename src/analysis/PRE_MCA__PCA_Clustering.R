################################################################################

##########################Clustering###################################################

################################################################################

library(ggplot2)
library(RColorBrewer) 
library(FactoMineR)
library(factoextra)
load("~/Desktop/MVA PROJECT/MDS-MVA-FC25Ratings/src/data/cleansed_data.RData")

sample_data <- all_players[sample(nrow(all_players), size = floor(0.1 * nrow(all_players))), ]

################MCA##################

mca_df <- Filter(is.factor, sample_data)
res.mca <- MCA(mca_df, graph = FALSE)

barplot(res.mca$eig[, "percentage of variance"], names.arg = 1:nrow(res.mca$eig),
        ylab = "Percentage of variance", xlab = "Dimension")

plot(res.mca, choix = "var", title = "Cloud of variables")
plot(res.mca, invisible = c("ind"), title = "Graph of the active categories")

res.hcpc.mca <- HCPC(res.mca, graph = FALSE)

res.hcpc.mca $desc.var

fviz_cluster(res.hcpc.mca, geom = "point", main = "Clustered Factor Map")

################PCA##################

pca_df <- Filter(is.numeric, sample_data)  

names(pca_df)

# Perform PCA
res.pca <- PCA(pca_df, 
                  scale.unit = TRUE, 
                  graph = FALSE)

summary(res.pca)

# Plot individuals with supplementary variable contributions
fviz_pca_ind(res.pca, habillage = which(sapply(sample_data, is.factor)))

# Plot supplementary variables
fviz_pca_var(res.pca, repel = TRUE)

res.hcpc.pca <- HCPC(res.pca, graph = FALSE)

res.hcpc.pca $desc.var

fviz_cluster(res.hcpc.pca, geom = "point", main = "Clustered Factor Map")

