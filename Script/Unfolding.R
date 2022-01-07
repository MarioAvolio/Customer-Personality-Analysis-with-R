source(paste(getwd(),"/Script/DataPreprocessing.R",sep = "")) # execute DataPreprocessing



library(tidyverse) 
library(caret)

install.packages("dplyr")    # alternative installation of the %>%

library(dplyr)    # alternatively, this also loads %>%

library(ggplot2)


hist(trainingSet$Response,col="coral")
prop.table(table(trainingSet$Response))


set.seed(100)
trctrl <- trainControl(method = "cv", number = 10, savePredictions=TRUE)
nb_fit <- train(factor(Response) ~., data = trainingSet, method = "naive_bayes", trControl=trctrl, tuneLength = 0)
nb_fit


pred <- nb_fit$pred
pred$equal <- ifelse(pred$pred == pred$obs, 1,0)


eachfold <- pred %>%                                        
  group_by(Resample) %>%                         
  summarise_at(vars(equal),                     
               list(Accuracy = mean))              
eachfold

ggplot(data=eachfold, aes(x=Resample, y=Accuracy, group=1)) +
  geom_boxplot(color="maroon") +
  geom_point() +
  theme_minimal()

