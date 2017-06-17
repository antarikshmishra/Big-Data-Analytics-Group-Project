#Load packages
install.packages("gbm")
install.packages("e1071", dependencies=TRUE)
install.packages("pROC")
library(gbm)
library(caret)
library(pROC)

#Convert y to numeric binary values
datab<-data
levels(datab$y)[levels(datab$y)=="yes"]<-1
levels(datab$y)[levels(datab$y)=="no"]<-0
datab$y<-as.numeric(datab$y)-1

#Run boosted trees model
bst<-gbm(formula = y~.,
         data = datab,
         distribution = "bernoulli",
         n.trees = 200,
         cv.folds = 4,
         interaction.depth = 2,
         n.minobsinnode = 10,
         shrinkage = 0.05,
         bag.fraction = 0.5,
         train.fraction = 0.5,
         keep.data = FALSE,
         verbose = TRUE,
         n.cores = 1)
pred<-predict(bst,data,type = "response")

#Draw ROC curve
ROCC<-roc(datab$y,pred)
plot(ROCC,print.thres="best")

#Generate confusion matrix
pred.binary<-as.numeric(pred>coords(ROCC,x="best",ret="threshold"))
table(datab$y,pred.binary)
#or (better, require caret package)
confusionMatrix(datab$y,pred.binary,positive = "1")