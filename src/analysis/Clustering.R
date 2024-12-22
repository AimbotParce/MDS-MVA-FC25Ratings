
###########Clustering###############

load("src/data/cleansed_data_scaled.RData")

#We will be using all normal quantitative variables
numerical_vars <- names(all_players)[sapply(all_players, is.numeric)]
df_scaled_numeric <- all_players[, numerical_vars]

#Calculating distance matrix
d <- dist(df_scaled_numeric, method = "euclidean") 

#Dendograms (we choose ward method)

fit <- hclust(d, method="ward.D2") 
plot(fit,main="Dendrogram of Ward's method", cex = 0.01)
abline(h = 300, col = "red", lty = 2)
#chosen because is less sensitive to outliers and noise and tends to form equally sized clusters.


#######################################
### Determining Number of Clusters ####
#######################################

####### 1.TWSS Elbow Graph #########

## Applying kmeans algorithm for different number of clusters ##

aux<-c()
for (i in 2:8){
  k<-kmeans(df_scaled_numeric,centers=i,nstart=25)
  aux[i-1]<-k$tot.withinss
}
plot(2:8, aux, xlab="Number of Clusters", ylab="TWSS", type="l", main="TWSS vs. number of clusters")
#3 or 4 seems like a good elbow point

k3 <- kmeans(df_scaled_numeric, centers = 3, nstart = 25)
k4 <- kmeans(df_scaled_numeric, centers = 4, nstart = 25)


### Sum of Squares ####
k3$betweenss/k3$totss # as it gets closer to 1 better. 
k4$betweenss/k4$totss # as it gets closer to 1 better. 

#### 2. Pseudo F Index #########
aux<-c()
for (i in 2:8){
  k<-kmeans(df_scaled_numeric,centers=i,nstart=25)
  aux[i-1]<-((k$betweenss)*(nrow(df_scaled_numeric)-i))/((k$tot.withinss)*(i-1))
}
plot(2:8,aux, xlab="Number of Clusters", ylab="Pseudo-F", type="l", main="Pseudo F Index")
which.max(aux)

#### 3. Silhoutte Index ######
## 3a. silhouette function from cluster library
library(cluster)


## Using plot function
si <- silhouette(k3$cluster,d)
plot(si)
si4 <- silhouette(k4$cluster,d)
plot(si4)


## 3b. sil function from kmed library

library(kmed)


## k-medoid algorithm 

res <- fastkmed(d, 3)
res4 <- fastkmed(d, 4)

silhouette <- sil(d, res$medoid, res$cluster)
silhouette$plot

silhouette4 <- sil(d, res4$medoid, res4$cluster)
silhouette4$plot
#low average scores but it's the best we can do, low number of negative scores in 3 of 4 clusters which is good
#We choose 4 clusters in the end :D

## Q6. Try to interpret the clusters by looking at mean values of the variables at each cluster.
# Add the cluster indicators to the data frame

df_scaled_numeric_test_4 <- data.frame(df_scaled_numeric, k4$cluster)

### Group Means ###

aggregate(df_scaled_numeric_test_4,by=list(k4$cluster),FUN=mean)
#comment differences in means
#use of radar plot to visualize differences

