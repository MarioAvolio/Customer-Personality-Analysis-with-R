# PCA feature extraction

# execute DataPreprocessing
source(paste(getwd(),"/Model/DataPreprocessing.R",sep = "")) 

# Packages required
# install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR")
library("factoextra")

# ---------------------------------- PCA START
executePCA <- function(set, numberOfDimensions){
  set.PCA = PCA(set, scale.unit = TRUE, ncp = numberOfDimensions, graph = FALSE)
  eig.val <- get_eigenvalue(set.PCA)
  print(eig.val)   
  return(set.PCA)
}

trainingSet.PCA <- executePCA(trainingSet, 9) # 9 or 12 dimensions? TODO
fviz_eig(trainingSet.PCA, addlabels = TRUE, ylim = c(0, 50))


# ---------------------------------- VARIABLES ANALYSIS
analyzeVariables <- function(set.PCA){
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
 
# - Positively correlated variables are grouped
# together.
# - Negatively correlated variables are
# positioned on opposite sides of the plot
# origin (opposed quadrants).
# - The distance between variables and the
# origin measures the quality of the variables.
# Variables that are away from the origin are
# well represented.
fviz_pca_var(trainingSet.PCA, col.var = "black") 
analyzeVariables(trainingSet.PCA)


# ---------------------------------- INDIVIDUALS ANALYSIS
analyzeIndividuals <- function(set.PCA){
  ind <- get_pca_ind(set.PCA)
  cat("\n ---------------------- Coord ---------------------- \n")
  print(ind$coord)
  cat("\n ---------------------- Cos2 ---------------------- \n")
  print(ind$cos2)
  cat("\n ---------------------- Contrib ---------------------- \n")
  print(ind$contrib)
}


# - A high cos2 indicates a good
# representation of the individual on the
# principal component.
# - A low cos2 indicates that the individual is
# not perfectly represented by the PCs.

fviz_pca_ind(trainingSet.PCA, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                                repel = TRUE # Avoid text overlapping (slow if many points)
             )



