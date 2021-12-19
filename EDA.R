
# Read DataSet
dataSet <- read.csv(paste(getwd(),"/Model//marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=T) # use TAB as separator!

dim(dataSet)
#funzione per eseguire media, minimo etc
multiple.func <- function(x) {
  c(min = min(x), mean = mean(x), max = max(x), sd=sd(x), var=var(x),
    median=median(x), range=range(x))
}
#visualizzazione classi di ogni colonna 
sapply(dataSet, class)

#multi.func su ogni colonna 
sapply(dataSet, multiple.func)


#stampa di tutti gli istogrammi
library(Hmisc)
hist.data.frame(dataSet)


#tentativo di boxplot
x = dataSet[,5:7]
par(mfrow=c(5,7))
for(i in 5:7) {
  boxplot(x[,i], main=names(dataSet)[i]) }

plot(x=dataSet$Year_Birth, y=dataSet$Recency, col=dataSet$AcceptedCmp3)
