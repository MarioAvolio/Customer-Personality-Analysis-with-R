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








getNumberOfColFromName <- function(name)
{
  return(match(name,names(customers)))
}








getIndipendentNumbersOfCol <- function()
{
   return(c(- getNumberOfColFromName("Education"), - getNumberOfColFromName("Marital_Status"), 
           - getNumberOfColFromName("AcceptedCmp3"), - getNumberOfColFromName("AcceptedCmp4"), - getNumberOfColFromName("AcceptedCmp5"),
           - getNumberOfColFromName("AcceptedCmp1"), - getNumberOfColFromName("AcceptedCmp2"), - getNumberOfColFromName("Complain"),
           - getNumberOfColFromName("Response"), - getNumberOfColFromName("Kidhome"), - getNumberOfColFromName("Teenhome") )) 
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