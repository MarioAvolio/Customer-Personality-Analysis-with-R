########################################################################
#                                                                      #
#                             ELBOW METHOD                             #
#                                                                      #
########################################################################

# execute DataPreprocessing
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

trainingSet_pca <- trainingSet[, c(getNumberOfColFromName("Total_spent", trainingSet), 
                                    getNumberOfColFromName("MntMeatProducts", trainingSet), 
                                    getNumberOfColFromName("NumCatalogPurchases", trainingSet), 
                                    getNumberOfColFromName("MntWines", trainingSet), 
                                    getNumberOfColFromName("MntFishProducts", trainingSet))]



set.seed(6)
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(trainingSet_pca, i)$withinss)
  plot(1:10, wcss, type="b", main= paste('Clusters'), xlab='Number of clusters', ylab="WCSS")
}