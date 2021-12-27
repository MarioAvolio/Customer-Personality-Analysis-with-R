# execute DataPreprocessing
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 


########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library("seriation")
library("factoextra")
library("cluster")
########################################################################





########################################################################
#                                                                      #
#                             ELBOW METHOD                             #
#                                                                      #
########################################################################
set.seed(6)
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(trainingSet_scaled, i)$withinss)
}
plot(1:10, wcss, type="b", main = paste('Clusters'), xlab='Number of clusters', ylab="WCSS")


# OR

fviz_nbclust(trainingSet_scaled,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2)
########################################################################













########################################################################
#                                                                      #
#                              SILHOUETTE                              #
#                                                                      #
########################################################################
k <- 2:10
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters', ylab='Average Silhouette Scores', frame=FALSE)
avg_sil # <<<- important

# OR
fviz_nbclust(trainingSet_scaled, kmeans, method="silhouette")
########################################################################
















########################################################################
#                                                                      #
#                               K-MEANS                                #
#                                                                      #
########################################################################
set.seed(29)
km <- kmeans(trainingSet_scaled, 2, nstart = 10)
print(km$centers)
fviz_cluster(km, trainingSet_scaled, geom = "point",ellipse.type = "norm",repel = TRUE)
cl <- km$cluster
########################################################################










########################################################################
#                                                                      #
#                         DISSIMILARITY MATRIX                         #
#                                                                      #
########################################################################
dissplot(dist(trainingSet_scaled), labels=cl,options=list(main="Kmeans Clustering With k=2"))

# clusters similar to each
# other are plotted
# darker, and dissimilar
# combinations are
# plotted lighter.

########################################################################












########################################################################
#                                                                      #
#                               ANALISYS                               #
#                                                                      #
########################################################################
trainingSet_scaled <- mutate(trainingSet_scaled, cluster = cl)
#Calculating the mean for each category
count(trainingSet_scaled, cluster)

trainingSet <- mutate(trainingSet, cluster = cl)
count(trainingSet, cluster)



#visualizing wines
ggplot(trainingSet, aes(MntWines)) + geom_histogram(color = "black", fill = "red") + 
  facet_wrap(vars(cluster)) 

#visualizing Income variable
ggplot(trainingSet, aes(Income)) + geom_histogram(color = "black", fill = "green") + 
  facet_wrap(vars(cluster)) + 
  geom_vline(aes(xintercept=mean(Income)),color="blue", linetype="dashed", size = 1) +
  xlim(0,200000)

#visualizing Total_spent
ggplot(trainingSet, aes(Total_spent)) + geom_histogram(color = "black", fill = "purple") + facet_wrap(vars(cluster))


#visualizing NumCatalogPurchases
ggplot(trainingSet, aes(NumCatalogPurchases)) + geom_histogram(color = "black", fill = "blue") + facet_wrap(vars(cluster)) 


#visualizing meat variable
ggplot(trainingSet, aes(MntMeatProducts)) + geom_histogram(color = "black", fill = "brown") + facet_wrap(vars(cluster))
########################################################################
