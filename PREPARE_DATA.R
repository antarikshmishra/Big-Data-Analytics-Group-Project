#Load packages
install.packages("caret")
library(caret)

#Remove variables (before PCA)
#After looking at the plots, we removed all the variables which had little differences between y=="yes" and y=="no" respondents.
#2 exceptions: campaign, education, as they make business sense (e.g. the more educated you are, the more likeky you know about deposits, the more likely you are going to subscribe).
#Duration is also removed as it cannot be used to predict beforehand whether the consumer will subscribe.
#pdays is removed as pdaysinv has been computed instead.
rm.var<-c(
  "age",
  "duration",
  "marital",
  "default",
  "housing",
  "loan",
  "pdays",
  "day_of_week"
)
data<-data[,!names(data) %in% rm.var]

#Create balanced datasets (after PCA, before running logistic regression and boosted trees)
##Create a dataset with 50% y=="yes" and 50% y=="no"
data.no<-data[data$y=="no",]
set.seed(3546)
data.50sample<-rbind(
  data.no[sample(nrow(data.no), 4640), ],
  data[data$y=="yes",])
rm(data.no)
##Divide into training and test datasets.
set.seed(3546)
index<-createDataPartition(
  data.50sample$y,
  p = 0.8,
  list = FALSE,
  times = 1)
data.train<-data.50sample[index,]
data.test<-data.50sample[-index,]
rm(index)