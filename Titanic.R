library(ISLR)
library(car)
library(DataExplorer)
library(tidyverse)
library(polycor)
library(rsample)
library(glmnet)
library(elasticnet)
library(recipes)
library(MASS)
#library(mice)
library(glmnet)
#library(gbm)
library(caret)
require(randomForest)
library(tidyverse)

#make sure you change the directory if you want to use the data in your own computer
TC = read.csv(file="puth here you own directory that you save the file", header=TRUE, sep=",")
glimpse(TC)


titanic_adj <- TC%>%
  dplyr:: select(-name)%>%
  dplyr:: select(-cabin)%>%
  dplyr:: select(-body)%>%
  dplyr:: select(-ticket)%>%
  dplyr:: select(-home.dest)%>%
  dplyr:: select(-boat)%>%
  dplyr:: select(survived, everything())



# STEP 1: check the dimension and structure
glimpse(titanic_adj)
# STEP 2: check for missing values by row and column in the data
plot_missing(titanic_adj)

###################Exploratory Data Analysis#####################

glimpse(titanic_adj)

plot_density(titanic_adj)

#distribution of survival
g <- ggplot(titanic_adj,aes(survived))
g + geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_y_continuous(labels=scales::percent)+theme_minimal()+ylab("Percent")

#Survival by age
g <-ggplot(titanic_adj,aes(age))+theme_bw()+facet_wrap(~survived)
g + geom_density()


#sex distribution #survival
g <-ggplot(titanic_adj,aes(sex,fill=sex))+theme_bw()+facet_wrap(~survived)+labs(x="",y="")
g+geom_bar()


#p-class distribution survival
titanic_adj$embarked <- as.factor(titanic_adj$embarked)
g <-ggplot(titanic_adj,aes(survived,fill=survived))+theme_bw()+facet_wrap(~embarked)
g+geom_bar()
g <-ggplot(titanic_adj,aes(survived,fill=survived))+theme_bw()+facet_wrap(~pclass)
g+geom_bar()

#embark distribution survival

datanu$embarked <- as.factor(datanu$embarked)
g <-ggplot(datanu,aes(survived,fill=survived))+theme_bw()+facet_wrap(~embarked)
g+geom_bar()

########Split and recipe####################################################

set.seed(200)
samp <- sample(nrow(titanic_adj), .6 * nrow(titanic_adj))
train <- titanic_adj[samp,]
test <- titanic_adj[-samp,]

dim(train)
dim(test)


sushi = recipe (survived~., data = train)%>%
  #step_center(all_numeric(), -all_outcomes()) %>%
  #step_scale(all_numeric(), -all_outcomes()) %>%
  step_knnimpute(all_predictors(), -all_outcomes()) %>%
  step_BoxCox(all_numeric(), -all_outcomes()) %>%
  step_mutate(solo = (parch==0)+(sibsp==0)) %>%
  step_mutate(crew = (sex=="male")+(fare==0))%>%
  step_dummy(all_nominal(),-all_outcomes(), one_hot = TRUE) %>%
  prep(data = train)

sushi
#view (train)

train_clean <- bake(sushi, new_data = train)
test_clean <- bake(sushi, new_data = test)

train_clean <- na.omit(train_clean)

test_clean <- na.omit(test_clean)



# Logistic model for comparison#############################################

TR.log <- glm(survived ~ . , data = train_clean, family = "binomial")
summary(TR.log)


TR.log.prob <- predict(TR.log, newdata=test_clean, type='response')
TR.log.pred <- ifelse(TR.log.prob > 0.5,"yes","no")
table(TR.log.pred, test_clean$survived)
mean(TR.log.pred==test_clean$survived)

view(train_clean)

# LogisticCaretModel##############GLM#########################################

ctrl <- trainControl(method = "repeatedcv",
                     number=10, repeats=3,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     savePredictions = "final")



HR.redmod.log <- train(survived ~ .,
                       data = train_clean,
                       method = "glm",
                       metric = "ROC",
                       family="binomial",
                       trControl = ctrl)
HR.redmod.log


print(HR.redmod.log)

HR.logpred = predict(HR.redmod.log, newdata=test_clean)
confusionMatrix(data=HR.logpred, test_clean$survived)

################LDA#####################################################

control <- trainControl(method = "repeatedcv", number = 10,
                        repeats = 3, classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions= "final")


HR.log.cv <- train(survived ~ . ,
                   data = train_clean,
                   method = "lda",
                   
                   metric="ROC",
                   trControl = control)

HR.log.cv
print(HR.log.cv)



HR.log.pred <- predict(HR.log.cv, newdata=test_clean)
confusionMatrix(data=HR.log.pred, test_clean$survived)









#####qda########################

control <- trainControl(method = "repeatedcv", number = 10,
                        repeats = 3, classProbs = TRUE,
                        summaryFunction = twoClassSummary,
                        savePredictions= "final")


qda.cv <- train(survived ~ . ,
                   data = train_clean,
                   method = "qda",
                   
                   metric="ROC",
                   trControl = control)

qda.cv
print(qda.)



HR.log.pred <- predict(HR.log.cv, newdata=test_clean)
confusionMatrix(data=HR.log.pred, test_clean$survived)


# Random Forest Standard Model#################################################

ti.rf <- randomForest(survived ~ . ,
                      data = train_clean)

ti.rf

plot(ti.rf)


importance(ti.rf)
varImpPlot(ti.rf)



# Random forest model with Train
control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

ti.rf.cv <- train(survived ~ . ,
                  method = "rf",
                  data = train_clean,
                  trControl = control,
                  metric = "ROC",
                  ntree = 200,
                  tuneLength = 10)

ti.rf.cv
ti.rf.cv.pred <- predict(ti.rf.cv, newdata=test_clean)
confusionMatrix(data=ti.rf.cv.pred, test_clean$survived)

###############GBM#############################################################

control <- trainControl(method = "repeatedcv",
                        number = 10,
                        repeats = 3,
                        classProbs = TRUE,
                        summaryFunction = twoClassSummary)

HR.gbm.cv <- train(survived ~ . ,
                   method = "gbm",
                   data = train_clean,
                   trControl = control,
                   metric = "ROC",
                   verbose = FALSE,
                   tuneLength = 5)

HR.gbm.cv

HR.gbm.pred <- predict(HR.gbm.cv, newdata=test_clean)
confusionMatrix(data=HR.gbm.pred, test_clean$survived)
#################################################################




# XGBoost Model
library(xgboost)
#Training with xgboost 
trctrl <- trainControl(method = "cv", number = 5)


tune_grid <- expand.grid(nrounds = 400,
                         max_depth = 3,
                         eta = c(0.3, 0.2, 0.1, 0.05),
                         gamma = 1,
                         colsample_bytree = 1,
                         min_child_weight = 1,
                         subsample = 0.5)

rf_fit <- train(survived~., data = train_clean, method = "xgbTree",
                trControl=trctrl,
                tuneGrid = tune_grid,
                tuneLength = 20)
rf_fit

rf.pred = predict(rf_fit, newdata=test_clean)
confusionMatrix(data=rf.pred, test_clean$survived)


























