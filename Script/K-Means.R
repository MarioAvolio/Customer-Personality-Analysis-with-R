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


#SAVE
#  png(file=paste(getwd(),"/Output/imgs/KMEANS/KMEANS%03d.png",sep = ""), width = 800, height = 800)



########################################################################
#                                                                      #
#                             ELBOW METHOD                             #
#                                                                      #
########################################################################
set.seed(6)
wcss <- vector()
for (i in 1:10) {
  wcss[i] <- sum(kmeans(dataSet_scaled, i)$withinss)
}
plot(1:10, wcss, type="b", main = paste('Clusters'), xlab='Number of clusters', ylab="WCSS")


# OR

fviz_nbclust(dataSet_scaled,kmeans,method="wss")+geom_vline(xintercept=2,linetype=2)
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
fviz_nbclust(dataSet_scaled, kmeans, method="silhouette")
########################################################################
















########################################################################
#                                                                      #
#                               K-MEANS                                #
#                                                                      #
########################################################################
set.seed(29)
km <- kmeans(dataSet_scaled, 2, nstart = 10)
print(km$centers)
fviz_cluster(km, dataSet_scaled, geom = "point",ellipse.type = "norm",repel = TRUE)
cl <- km$cluster
########################################################################










########################################################################
#                                                                      #
#                         DISSIMILARITY MATRIX                         #
#                                                                      #
########################################################################
dissplot(dist(dataSet_scaled), labels=cl,options=list(main="Kmeans Clustering With k=2"))

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
dataSet <- mutate(dataSet, cluster = cl)
dataSet$cluster <- as.factor(dataSet$cluster)
count(dataSet, cluster)


#visualizing wines
wines <- ggplot(dataSet, aes(MntWines)) + 
   facet_grid(cluster~.) 

wines + geom_histogram(color = "black", fill = "red") 
wines + geom_density(fill="red", position = "Stack")
ggplot(dataSet, aes(x=cluster,y=MntWines,fill=cluster))+geom_boxplot(outlier.colour="black")


#visualizing Income variable
income <- ggplot(dataSet, aes(Income))+ 
  facet_grid(cluster~.) + 
  xlim(0,200000)

income +  geom_histogram(color = "black", fill = "green") + geom_vline(aes(xintercept=mean(Income)),color="blue", linetype="dashed", size = 1)
income + geom_density(fill="green", position = "Stack")
ggplot(dataSet, aes(x=cluster,y=Income,fill=cluster))+geom_boxplot(outlier.colour="black") + ylim(0,200000)



#visualizing Total_spent
ts <- ggplot(dataSet, aes(Total_spent), colour=cluster) + facet_grid(cluster~.)
ts + geom_histogram(color = "black", fill = "purple") 
ts + geom_density(fill="purple", position = "Stack")
ggplot(dataSet, aes(x=cluster,y=Total_spent,fill=cluster))+geom_boxplot(outlier.colour="black")


#visualizing NumCatalogPurchases
numCatalogPurchases <- ggplot(dataSet, aes(NumCatalogPurchases)) +  facet_grid(cluster~.)
numCatalogPurchases + geom_histogram(color = "black", fill = "blue") 
numCatalogPurchases + geom_density(fill="blue", position = "Stack")
ggplot(dataSet, aes(x=cluster,y=NumCatalogPurchases,fill=cluster))+geom_boxplot(outlier.colour="black") + ylim(0,10)


#visualizing meat variable
meat <- ggplot(dataSet, aes(MntMeatProducts)) +  facet_grid(cluster~.)
meat + geom_histogram(color = "black", fill = "brown") 
meat + geom_density(fill="brown", position = "Stack")
ggplot(dataSet, aes(x=cluster,y=MntMeatProducts,fill=cluster))+geom_boxplot(outlier.colour="black")
########################################################################





#turn off png plotting
dev.off() 

