

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

