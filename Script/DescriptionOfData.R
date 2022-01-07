########################################################################
#                                                                      #
#                               LIBRARY                                #
#                                                                      #
########################################################################
library(dplyr) # glimpse
library(ggplot2)
library(naniar)
library(tidyr)
library(lubridate)
library(ggcorrplot)
library(caTools)
library(DataExplorer)
########################################################################




# Open file
png(file=paste(getwd(),"/Output/imgs/DESCRIPTION/DESCRIPTION%03d.png",sep = ""), width = 800, height = 800)




########################################################################
#                                                                      #
#                           Read Dataset                               #
#                                                                      #
######################################################################## 
customers <- read.csv(paste(getwd(),"/Data/marketing_campaign.csv",sep = ""), header=TRUE, sep="\t",  stringsAsFactors=F) # use TAB as separator!
########################################################################







########################################################################
#                                                                      #
#                               SUMMARY                                #
#                                                                      #
########################################################################
dim(customers)
head(customers, n=6)
glimpse(customers)


summary(customers) # most important!

 #        ID          Year_Birth    Education         Marital_Status         Income          Kidhome          Teenhome      Dt_Customer       
 #  Min.   :    0   Min.   :1893   Length:2240        Length:2240        Min.   :  1730   Min.   :0.0000   Min.   :0.0000   Length:2240       
 #  1st Qu.: 2828   1st Qu.:1959   Class :character   Class :character   1st Qu.: 35303   1st Qu.:0.0000   1st Qu.:0.0000   Class :character  
 #  Median : 5458   Median :1970   Mode  :character   Mode  :character   Median : 51382   Median :0.0000   Median :0.0000   Mode  :character  
 #  Mean   : 5592   Mean   :1969                                         Mean   : 52247   Mean   :0.4442   Mean   :0.5062                     
 #  3rd Qu.: 8428   3rd Qu.:1977                                         3rd Qu.: 68522   3rd Qu.:1.0000   3rd Qu.:1.0000                     
 #  Max.   :11191   Max.   :1996                                         Max.   :666666   Max.   :2.0000   Max.   :2.0000                     
 #  NA's   :24                                                           
 # 
 # 
 #    Recency         MntWines         MntFruits     MntMeatProducts  MntFishProducts  MntSweetProducts  MntGoldProds    NumDealsPurchases
 # Min.   : 0.00   Min.   :   0.00   Min.   :  0.0   Min.   :   0.0   Min.   :  0.00   Min.   :  0.00   Min.   :  0.00   Min.   : 0.000   
 # 1st Qu.:24.00   1st Qu.:  23.75   1st Qu.:  1.0   1st Qu.:  16.0   1st Qu.:  3.00   1st Qu.:  1.00   1st Qu.:  9.00   1st Qu.: 1.000   
 # Median :49.00   Median : 173.50   Median :  8.0   Median :  67.0   Median : 12.00   Median :  8.00   Median : 24.00   Median : 2.000   
 # Mean   :49.11   Mean   : 303.94   Mean   : 26.3   Mean   : 166.9   Mean   : 37.53   Mean   : 27.06   Mean   : 44.02   Mean   : 2.325   
 # 3rd Qu.:74.00   3rd Qu.: 504.25   3rd Qu.: 33.0   3rd Qu.: 232.0   3rd Qu.: 50.00   3rd Qu.: 33.00   3rd Qu.: 56.00   3rd Qu.: 3.000   
 # Max.   :99.00   Max.   :1493.00   Max.   :199.0   Max.   :1725.0   Max.   :259.00   Max.   :263.00   Max.   :362.00   Max.   :15.000   
 #                                                                                                                                        
 # 
 # 
 # NumWebPurchases  NumCatalogPurchases NumStorePurchases NumWebVisitsMonth  AcceptedCmp3      AcceptedCmp4      AcceptedCmp5    
 # Min.   : 0.000   Min.   : 0.000      Min.   : 0.00     Min.   : 0.000    Min.   :0.00000   Min.   :0.00000   Min.   :0.00000  
 # 1st Qu.: 2.000   1st Qu.: 0.000      1st Qu.: 3.00     1st Qu.: 3.000    1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.00000  
 # Median : 4.000   Median : 2.000      Median : 5.00     Median : 6.000    Median :0.00000   Median :0.00000   Median :0.00000  
 # Mean   : 4.085   Mean   : 2.662      Mean   : 5.79     Mean   : 5.317    Mean   :0.07277   Mean   :0.07455   Mean   :0.07277  
 # 3rd Qu.: 6.000   3rd Qu.: 4.000      3rd Qu.: 8.00     3rd Qu.: 7.000    3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.00000  
 # Max.   :27.000   Max.   :28.000      Max.   :13.00     Max.   :20.000    Max.   :1.00000   Max.   :1.00000   Max.   :1.00000  
 #                                                                                                                               
 # 
 # 
 #  AcceptedCmp1      AcceptedCmp2        Complain        Z_CostContact   Z_Revenue     Response     
 # Min.   :0.00000   Min.   :0.00000   Min.   :0.000000   Min.   :3     Min.   :11   Min.   :0.0000  
 # 1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.000000   1st Qu.:3     1st Qu.:11   1st Qu.:0.0000  
 # Median :0.00000   Median :0.00000   Median :0.000000   Median :3     Median :11   Median :0.0000  
 # Mean   :0.06429   Mean   :0.01339   Mean   :0.009375   Mean   :3     Mean   :11   Mean   :0.1491  
 # 3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:0.000000   3rd Qu.:3     3rd Qu.:11   3rd Qu.:0.0000  
 # Max.   :1.00000   Max.   :1.00000   Max.   :1.000000   Max.   :3     Max.   :11   Max.   :1.0000  
                                                                                                   

#NOTE:
# - There are some categorical features, so we will need to encode into numeric form as we proceed.
# - Dt_Customer that indicates a cutomer joined the database is not parsed as Date object.

#########################################################################





#########################################################################
#                                                                       #
#                           SUMMARY BOXPLOTS                            #
#                                                                       #
#########################################################################

# Boxplot of the Year of birth variable
ggplot(customers, aes(Year_Birth)) + geom_boxplot() 

#NOTE:
# - 3 Outliers


# Boxplot of Income variable
ggplot(customers, aes(Income)) + geom_boxplot() 

#NOTE:
# - 24 rows containing non-finite values
# - Some outliers


# Boxplot of Income variable
ggplot(customers, aes(Income)) + geom_boxplot() 

#NOTE:
# - 24 rows containing non-finite values
# - Some outliers



# Kidhome
ggplot(customers, aes(Kidhome)) + geom_boxplot() 

#NOTE:
# - No outliers
# - First quantile is equal to Avg





# Kidhome
ggplot(customers, aes(Teenhome)) + geom_boxplot() 

#NOTE:
# - No outliers
# - First quantile is equal to Avg
# - Similar to Kidhome boxplot



# Boxplot of Recency variable
ggplot(customers, aes(Recency)) + geom_boxplot() 

#NOTE:
# - No outliers




# -------------------------------------------------------------- MNT VARIABLES BOXPLOT

# Boxplot of the Mnt Wines variable
ggplot(customers, aes(MntWines)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 1493. The third quantile is 504.25.
# - There are not outliers.


# Boxplot of the Mnt Fruits variable
ggplot(customers, aes(MntFruits)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 199 The third quantile is 33.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fruit is higher compared to the quantiles.


# Boxplot of the Mnt Meat Products variable
ggplot(customers, aes(MntMeatProducts)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 1725 The third quantile is 232.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of meat is higher compared to the quantiles.


# Boxplot of the Mnt Fish Products variable
ggplot(customers, aes(MntFishProducts)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 259 The third quantile is 50.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.



# Boxplot of the Mnt Sweet Products variable
ggplot(customers, aes(MntSweetProducts)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 259 The third quantile is 50.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.


# Boxplot of the Mnt Gold Prods variable
ggplot(customers, aes(MntGoldProds)) + geom_boxplot() 

#NOTE:
# - We also noted from looking at the summary statistics, the minimum amount is 0 and max is 362 The third quantile is 56.0.
# - All theese became clear when we plotted a boxplot. There are also many cases where the mnt of fish  is higher compared to the quantiles.


#- the boxplots of theese variables are similiar.  
# -------------------------------------------------------------- 










# -------------------------------------------------------------- PURCHASES VARIABLES BOXPLOT

# NumDealsPurchases
ggplot(customers, aes(NumDealsPurchases)) + geom_boxplot() 

#NOTE:
# - Some outliers


# NumWebPurchases  
ggplot(customers, aes(NumWebPurchases)) + geom_boxplot() 

#NOTE:
# - Some outliers


# NumCatalogPurchases 
ggplot(customers, aes(NumCatalogPurchases)) + geom_boxplot()

#NOTE:
# - Some outliers



# NumStorePurchases 
ggplot(customers, aes(NumStorePurchases)) + geom_boxplot() 

#NOTE:
# - No outliers

# -------------------------------------------------------------- 




# NumWebVisitsMonth
ggplot(customers, aes(NumDealsPurchases)) + geom_boxplot() 

#NOTE:
# - Some outliers


# Z_CostContact and Z_Revenue
ggplot(customers, aes(Z_CostContact)) + geom_boxplot() 
multiple.func(customers$Z_CostContact)
ggplot(customers, aes(Z_Revenue)) + geom_boxplot() 
multiple.func(customers$Z_Revenue)

# NOTE:
# - The features Z_CostContact and Z_Revenue show no variation.

########################################################################






########################################################################
#                                                                      #
#                             MISSING VALUE                            #
#                                                                      #
########################################################################
hist(customers$Income,40,col="#adcae6")
ggplot(customers, aes(y = Income)) + geom_boxplot()
n_miss(customers) # counting the total number of missing values in the data
miss_var_summary(customers) # Summarizing missingness in each variable 


# NOTE:
#   
# - There are 24 missing values in the income variable.
# - Also, we can see that the maximum value of the income variable is larger than the 3rd quantile.
########################################################################



########################################################################
#                                                                      #
#                                 MREPORT                              #
#                                                                      #
########################################################################

# DataExplorer::create_report(customers, output_dir = paste(getwd(),"/Output/Data/",sep = ""))
########################################################################



dev.off() 


















