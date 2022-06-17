#installing & loading packages
install.packages("janitor")
install.packages("skimr")
install.packages("GGally")
install.packages("gridExtra")
install.packages("here")
library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(GGally)
library(gridExtra)
library(here)
getwd()
#reading the data set & cleaning it
wineDT <- read.csv(here("data/winequality-red.csv"))
wine <- as_tibble(wineDT)
wine <- wine %>%
  clean_names()
colnames(wine)[9] <- "pH"
wine$quality <- as.factor(wine$quality)
 
#an abstract look into the data set and its properties 
glimpse(wine)
summary(wine)
str(wine)
skim(wine)


#first we will take a look at the first and last rows of the data set
head(wine)
tail(wine)

#Distribution of the wine quality ratings
wine %>% 
  ggplot(aes(x=quality, fill=quality)) +
  geom_bar()


#Distribution of the wine variables 
g1 <- wine %>% ggplot(aes(x=fixed_acidity)) + geom_histogram(bins = 20)
g2 <- wine %>% ggplot(aes(x=volatile_acidity)) + geom_histogram(bins = 20)
g3 <- wine %>% ggplot(aes(x=citric_acid)) + geom_histogram(bins = 20)
g4 <- wine %>% ggplot(aes(x=residual_sugar)) + geom_histogram(bins = 20)
g5 <- wine %>% ggplot(aes(x=chlorides)) + geom_histogram(bins = 20)
g6 <- wine %>% ggplot(aes(x=free_sulfur_dioxide)) + geom_histogram(bins = 20)
g7 <- wine %>% ggplot(aes(x=total_sulfur_dioxide)) + geom_histogram(bins = 20)
g8 <- wine %>% ggplot(aes(x=density)) + geom_histogram(bins = 20)
g9 <- wine %>% ggplot(aes(x=pH)) + geom_histogram(bins = 20)
g10 <- wine %>% ggplot(aes(x=sulphates)) + geom_histogram(bins = 20)
g11 <- wine %>% ggplot(aes(x=alcohol)) + geom_histogram(bins = 20)

grid.arrange(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11)



#lets take a look on how each attribute effect the quality of the wine 

#quality & fixed_acidity


#it seems fixed_acidity has no effect on quality 

#quality & volatile_acidity
ggplot(wine,aes(x=quality, y= volatile_acidity, color = quality)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems volatile_acidity has a negative effect on quality "downward trend"

#quality & citric_acid
 ggplot(wine,aes(x=quality, y= citric_acid, color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems citric_acid has a positive effect on quality "upward trend"


#quality & residual_sugar
ggplot(wine,aes(x=quality, y= residual_sugar, color = quality)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems residual_sugar has no effect on quality 

#quality & chlorides
 ggplot(wine,aes(x=quality, y= chlorides, color = quality)) +
   geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#chlorides seems to have no effect on the quality but we can't be sure,
#the box plot is not clear due to the high variance of its data so lets make bigger!
 ggplot(wine,aes(x=quality, y= chlorides, color = quality)) + 
   geom_boxplot(outlier.shape = NA) +
   coord_cartesian(ylim=c(0, 0.2))
 #it seems chlorides has no effect on quality
 
#quality & free_sulfur_dioxide
ggplot(wine,aes(x=quality, y= free_sulfur_dioxide, color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems free_sulfur_dioxide has no effect on quality


#quality & total_sulfur_dioxide
ggplot(wine,aes(x=quality, y= total_sulfur_dioxide,color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#similar to free_sulfur_dioxide variable so seems to have no effect on quality

#quality & density
ggplot(wine,aes(x=quality, y= density,color = quality)) +
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems density has a negative effect on quality "downward trend"


#quality & pH
ggplot(wine,aes(x=quality, y= pH,color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems pH has no effect on quality


#quality & sulphates
ggplot(wine,aes(x=quality, y= sulphates,color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems sulphates has a positive effect on quality "upward trend"


#quality & alcohol
ggplot(wine,aes(x=quality, y= alcohol,color = quality)) + 
  geom_jitter(width = 0.15, shape = 16, alpha = 0.75) +
  geom_boxplot() 
#it seems alcohol has a positive effect on quality "upward trend"



