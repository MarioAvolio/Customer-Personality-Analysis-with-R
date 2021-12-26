
# execute DataPreprocessing
source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) 

########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library("FactoMineR")
library("factoextra")
library("caret")
########################################################################












########################################################################
#                                                                      #
#                           FEATURE SCALING                            #
#                                                                      #
########################################################################

trainingSet_copy_pre <- preProcess(trainingSet[,getIndipendentNumbersOfCol()], method = c("center", "scale"), thresh = 0.70)
trainingSet_copy <- predict(trainingSet_copy_pre, trainingSet[,getIndipendentNumbersOfCol()])
summary(trainingSet_copy)
remove(trainingSet_copy_pre)
?preProcess

# trainingSet[, getIndipendentNumbersOfCol()] <- scale(trainingSet[, getIndipendentNumbersOfCol()])
# testSet[, getIndipendentNumbersOfCol()] <- scale(testSet[, getIndipendentNumbersOfCol()])
########################################################################








########################################################################
#                                                                      #
#                                 PCA                                  #
#                                                                      #
########################################################################
#Running a PCA.
trainingSet_pca <- PCA(trainingSet_copy, graph = FALSE)

?PCA
#----------------------------------------------------- Exploring PCA
# Getting the summary of the pca
summary(trainingSet_pca)

#Getting the variance of the first 9 new dimensions
trainingSet_pca$eig[,2][1:9]

#Getting the cummulative variance
trainingSet_pca$eig[,3][1:5]

#Getting the most correlated variables
dimdesc(trainingSet_pca, axes = 1:2)

# get eigenvalue
get_eigenvalue(trainingSet_pca)

# get variables and indivisuals information
# • var$coord: coordinates of variables to create a scatter plot
# • var$cos2: represents the quality of representation for variables on the factor map.
# • var$contrib: contains the contributions (in percentage) of the variables to the principal
# components.
analyzeVariablesPCA(trainingSet_pca)
analyzeIndividuals(trainingSet_pca)


#Tracing variable contributions in customers_pca
trainingSet_pca$var$contrib
########################################################################













########################################################################
#                                                                      #
#                            VISUALIZE PCA                             #
#                                                                      #
########################################################################
fviz_eig(trainingSet_pca, addlabels = TRUE, ylim = c(0, 50))
fviz_contrib(trainingSet_pca, choice = "var", axes = 1, top = 5)
fviz_pca_biplot(trainingSet_pca)


#Creating a factor map for the variable contributions
fviz_pca_var(trainingSet_pca, col.var = "contrib", repel = TRUE)

fviz_pca_var(trainingSet_pca, select.var = list(contrib = 5), col.var = "contrib", repel = TRUE)

# NOTE:
# - Positively correlated variables are grouped
  # together.
# - Negatively correlated variables are
  # positioned on opposite sides of the plot
  # origin (opposed quadrants).
# - The distance between variables and the
  # origin measures the quality of the variables.
  # Variables that are away from the origin are
  # well represented.





# ---------------------------------- INDIVIDUALS ANALYSIS



# - A high cos2 indicates a good
# representation of the individual on the
# principal component.
# - A low cos2 indicates that the individual is
# not perfectly represented by the PCs.

fviz_pca_ind(trainingSet_pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

########################################################################



