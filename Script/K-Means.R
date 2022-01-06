# execute DataPreprocessing
source(paste(getwd(),"/Script/PCA.R",sep = "")) 


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
  wcss[i] <- sum(kmeans(trainingSet_input, i)$withinss)
}
plot(1:10, wcss, type="b", main = paste('Clusters'), xlab='Number of clusters', ylab="WCSS")


# OR

fviz_nbclust(trainingSet_input,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2)
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
fviz_nbclust(trainingSet_input, kmeans, method="silhouette")
########################################################################
















########################################################################
#                                                                      #
#                               K-MEANS                                #
#                                                                      #
########################################################################
set.seed(29)
km <- kmeans(trainingSet_input, 2, nstart = 10)
print(km$centers)
fviz_cluster(km, trainingSet_input, geom = "point",ellipse.type = "norm",repel = TRUE)
cl <- km$cluster
########################################################################










########################################################################
#                                                                      #
#                         DISSIMILARITY MATRIX                         #
#                                                                      #
########################################################################
dissplot(dist(trainingSet_input), labels=cl,options=list(main="Kmeans Clustering With k=2"))

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
trainingSet_input <- mutate(trainingSet_input, cluster = cl)
#Calculating the mean for each category
count(trainingSet_input, cluster)

trainingSet <- mutate(trainingSet, cluster = cl)
count(trainingSet, cluster)

trainingSet


#visualizing wines
wines <- ggplot(trainingSet, aes(MntWines)) + 
   facet_grid(cluster~.) 

wines + geom_histogram(color = "black", fill = "red") 
wines + geom_density(fill="red", position = "Stack")



#visualizing Income variable
income <- ggplot(trainingSet, aes(Income))+ 
  facet_grid(cluster~.) + 
  xlim(0,200000)

income +  geom_histogram(color = "black", fill = "green")
income + geom_density(fill="green", position = "Stack")



#visualizing Total_spent
ts <- ggplot(trainingSet, aes(Total_spent), colour=cluster) + facet_grid(cluster~.)
ts + geom_histogram(color = "black", fill = "purple") 
ts + geom_density(fill="purple", position = "Stack")

spentplot <- ggplot(trainingSet, aes(x=cluster,y=Total_spent,fill=cluster))+geom_boxplot(outlier.colour="black", outlier.shape=16,outlier.size=2, notch=T)
spentplot



#visualizing NumCatalogPurchases
numCatalogPurchases <- ggplot(trainingSet, aes(NumCatalogPurchases)) +  facet_grid(cluster~.)
numCatalogPurchases + geom_histogram(color = "black", fill = "blue") 
numCatalogPurchases + geom_density(fill="blue", position = "Stack")



#visualizing meat variable
meat <- ggplot(trainingSet, aes(MntMeatProducts)) +  facet_grid(cluster~.)
meat + geom_histogram(color = "black", fill = "brown") 
meat + geom_density(fill="brown", position = "Stack")
########################################################################
