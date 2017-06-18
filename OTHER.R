#Load packages
install.packages("corrplot")
library(corrplot)

#Correlation matrix (for numerical variables only)
corrplot(
  cor(bank_additional_full),
  method="shade",
  shade.col=NA,
  tl.col="black",
  tl.srt=45,
  addCoef.col="black",
  type = "lower")

#Transform pdays into pdaysinv and add it to the dataset
pdaysinv<-1/(data$pdays+1)
pdaysinv[pdaysinv == 1/1000] <- 0
describe(pdaysinv)
data<-cbind(data,pdaysinv)
rm(pdaysinv)