# Make sure that you have all the libraries installed
library(tidyverse)
library(caret)
library(data.table)
library(pROC)
library(doMC)
library(e1071)
#library(ggfortify)

bank_data <- fread("raw_data/bank-additional-full.csv", sep = ";", header = T, stringsAsFactors = F) %>% drop_na()

#We have to convert the "pdays" column into categorical to deal with the 999 value issue and we remove duration as we cannot use it in prediction
bank_data$pdays <- cut(bank_data$pdays, breaks = c(-1,10,20,30,1000), labels=c("1","2","3","4"))
bank_data_clean <- subset(bank_data, select = -c(duration))

#Summary of the cleaned dataset
summary(bank_data_clean)

str(bank_data_clean)

# Extract all the numeric features of the full bank data to do feature reduction using pca
bank_subset_all_numeric <- bank_data_clean %>% 
  select_if(is.numeric) %>% 
  drop_na()

# This preprocessing step will create a preProcess object that will center, scale and apply pca model on all the numerical variables
preProc <- preProcess(x = bank_subset_all_numeric, method = c("center", "scale", "pca"))

# This step returns the data set with all the categorical features and the principal components of the numerical features
preprocessed_data_full <- predict(preProc, bank_data_clean) %>% 
  mutate(y = ifelse(y == "yes", 1, 0))
str(preprocessed_data_full)

# There is no preprocessing done on the categorical data. 
# If needed you can convert month and day_of_week from character vectors to ordered factors. Check ?factor

# If you want to create dummy variables of your categorical features you can run the following lines of code

# dummy_vars <- dummyVars(y~. - contact -poutcome, preprocessed_data_full, sep = ".", fullRank = T)
# preprocessed_data_full_with_dummies <- predict(dummy_vars, preprocessed_data_full)

# Modelling ----

# Splitting the dataset into train and test (9:1)
indexes <- createDataPartition(y = preprocessed_data_full$y, times = 1, p = .9)

train_set <- preprocessed_data_full[indexes$Resample1,]
test_set <- preprocessed_data_full[-indexes$Resample1,]

ml_formula <- formula(y~.)

# Logistic Regression Model (Generalized Linear Model)
glm_model <- glm(formula = ml_formula, family = binomial(link = "logit"), data = train_set)

predictions <- predict.glm(glm_model, test_set, type = "response")

# Take the threshold of .5 to predict the classes based on the probabilities
pred_class <- ifelse(predictions > .5, 1, 0)
# Confusion Matrix of the predicted classes
confusionMatrix(table(pred_class, test_set$y))

# ROC curve
roc(response = test_set$y, predictor = predictions) %>% plot.roc()


# 10 fold repeated cross validation gradient boosting model

fitControl <- trainControl(## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 4, classProbs = T,summaryFunction = twoClassSummary, allowParallel = T)

set.seed(825)
# Register your computer cores to do parallel processing
n_threads <- detectCores()
registerDoMC(cores = n_threads)

gbm_model <- train(ml_formula, data = {train_set %>% mutate(y = as.factor(ifelse(y == 1, "X1", "X0")))}, 
                 method = "gbm", 
                 trControl = fitControl,
                 ## This last option is actually one
                 ## for gbm() that passes through
                 verbose = FALSE)


gbm_model

# Predict classes of the test data
pred_gbm <- predict(gbm_model, test_set, type = "prob") %>% mutate(y = test_set$y)

# Confusion Matrix
pred_gbm %>% 
  mutate(pred = ifelse(X1 > .5, 1, 0)) %>% 
  select(y, pred) %>%
  table %>% 
confusionMatrix

# ROC curve
roc(response = pred_gbm$y, predictor = pred_gbm$X1) %>% plot.roc()

# Logit model with cross-validation
glm_model_cv <- train(ml_formula, data = {train_set %>% mutate(y = as.factor(ifelse(y == 1, "X1", "X0")))}, 
                   method = "glm", 
                   trControl = fitControl)


glm_model_cv

# Predict classes of the test data for logit with cross-validation
pred_glm_cv <- predict(glm_model_cv, test_set, type = "prob") %>% mutate(y = test_set$y)

# Confusion Matrix of the logit model with cross validation
pred_glm_cv %>% 
  mutate(pred = ifelse(X1 > .35, 1, 0)) %>% 
  select(y, pred) %>%
  table %>% 
  confusionMatrix

# ROC curve of the new logit model
roc(response = pred_glm_cv$y, predictor = pred_glm_cv$X1) %>% plot.roc()

