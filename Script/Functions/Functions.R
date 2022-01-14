converterCharacterToFactor <- function(column) {
  
  uniqueValue <- levels(column) # take unique value of column
  length(uniqueValue)
  column <- factor(column,
                   levels = uniqueValue,
                   labels = 1:length(uniqueValue)) # Convert character column to factor 
  
  return(column)
  
}












multiple.func <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x), sd=sd(x), var=var(x),
    median=median(x), range=range(x))
}











calculateBreaksFromSummary <- function(col){
  breaks = c(summary(col)["Min."],
             summary(col)["1st Qu."], 
             summary(col)["Mean"],
             summary(col)["3rd Qu."],
             summary(col)["Max."])
  
  return(breaks)
}








getNumberOfColFromName <- function(name, dataset=customers)
{
  return(match(name,names(dataset)))
}








getIndipendentNumbersOfCol <- function(data=dataSet)
{
  return(c(- getNumberOfColFromName("Education", data), - getNumberOfColFromName("Marital_Status", data), 
           - getNumberOfColFromName("AcceptedCmp3", data), - getNumberOfColFromName("AcceptedCmp4", data), - getNumberOfColFromName("AcceptedCmp5", data),
           - getNumberOfColFromName("AcceptedCmp1", data), - getNumberOfColFromName("AcceptedCmp2", data), - getNumberOfColFromName("Complain", data),
           - getNumberOfColFromName("Response", data), - getNumberOfColFromName("Kidhome", data), - getNumberOfColFromName("Teenhome", data) )) 
}








########################################################################
#                                                                      #
#                                 PCA                                  #
#                                                                      #
########################################################################
analyzeVariablesPCA <- function(set.PCA){
  # • var$coord: coordinates of variables to create a scatter plot
  # • var$cos2: represents the quality of representation for variables on the factor map.
  # • var$contrib: contains the contributions (in percentage) of the variables to the principal
  # components.
  var <- get_pca_var(set.PCA)
  cat("\n ---------------------- Coord ---------------------- \n")
  print(var$coord)
  cat("\n ---------------------- Cos2 ---------------------- \n")
  print(var$cos2)
  cat("\n ---------------------- Contrib ---------------------- \n")
  print(var$contrib)
}

analyzeIndividuals <- function(set.PCA){
  ind <- get_pca_ind(set.PCA)
  cat("\n ---------------------- Coord ---------------------- \n")
  print(ind$coord)
  cat("\n ---------------------- Cos2 ---------------------- \n")
  print(ind$cos2)
  cat("\n ---------------------- Contrib ---------------------- \n")
  print(ind$contrib)
}


########################################################################
#                                                                      #
#                              SILHOUETTE                              #
#                                                                      #
########################################################################
silhouette_score <- function(k, df=trainingSet){
  km <- kmeans(df, centers = k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[, 3])
}








########################################################################
#                                                                      #
#                 Relationship Income and Consumption                  #
#                                                                      #
########################################################################

plotRelationship <- function(product){
  plot = trainingSet %>%
    ggplot(aes_string(x='Income', y=product)) + 
    geom_point() +
    geom_smooth(aes_string(x='Income', y=product), method='gam', formula=y ~ s(x, bs = "cs")) +
    ggtitle(paste('Scatterplot of Income and ', product)) 
  
  return(plot)
}












# Print
library(xtable)
printToLatex <- function(dataSetToPrint)
{
  print(xtable(as.data.frame(dataSetToPrint), type = "latex"), file = "filename2.tex")
}