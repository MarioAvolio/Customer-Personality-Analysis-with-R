# execute DataPreprocessing
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 
library("factoextra")

trainingSet <- trainingSet[, c(getNumberOfColFromName("Total_spent", trainingSet), 
                                    getNumberOfColFromName("MntMeatProducts", trainingSet), 
                                    getNumberOfColFromName("NumCatalogPurchases", trainingSet), 
                                    getNumberOfColFromName("MntWines", trainingSet), 
                                    getNumberOfColFromName("MntFishProducts", trainingSet))]









########################################################################
#                                                                      #
#                             ELBOW METHOD                             #
#                                                                      #
########################################################################
set.seed(6)
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(trainingSet, i)$withinss)
}
plot(1:10, wcss, type="b", main = paste('Clusters'), xlab='Number of clusters', ylab="WCSS")


# OR

fviz_nbclust(trainingSet,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2)
########################################################################




########################################################################
#                                                                      #
#                              SILHOUETTE                              #
#                                                                      #
########################################################################
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
avg_sil # <------ 0.6468039 0.6021632

# OR
fviz_nbclust(trainingSet, kmeans, method="silhouette")
########################################################################





########################################################################
#                                                                      #
#                               K-MEANS                                #
#                                                                      #
########################################################################
set.seed(29)
kmeans <- kmeans(trainingSet, 2, nstart = 10)
print(km.res$centers)
fviz_cluster(kmeans, trainingSet, geom = "point",ellipse.type = "norm",repel = TRUE)
########################################################################









