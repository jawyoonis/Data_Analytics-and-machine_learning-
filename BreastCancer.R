library(ISLR)
library(mlbench)
library(caret)
library(MASS)
library(DataExplorer)
library(tidyverse)
library(polycor)
library(car)
library(recipes)
library(ROCR)
library(broom)

data(BreastCancer)
summary(BreastCancer)
Bc<-BreastCancer%>%
select(-Id)%>%

select(Class, everything())

for(i in 1:9) {
  Bc[, i] <- as.numeric(as.character(Bc[, i]))
}

# Change Y values to 1's and 0's
c$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

#Bc$Class= as.factor(Bc$Class)

glimpse(Bc)

dim(Bc)



plot_missing(Bc)
hetcor(Bc)
cor(Bc, method = "pearson")


set.seed(1234)
samp <- sample(nrow(Bc), .80*nrow(Bc))
train <- Bc[samp,]
test <- Bc[-samp,]

rec_object= recipe(Class ~., data=train)%>%
step_bagimpute(all_predictors(), -all_outcomes())%>%
step_bagimpute(all_predictors(), -all_outcomes())%>%
prep(data=train)
rec_object



train_clean = bake(rec_object, new_data=train)
test_clean = bake(rec_object, new_data=test)

plot_missing(train_clean)

# logistic model 

glm_1 = glm(Class ~ ., data=train_clean,family=binomial(link="logit"), maxit=100)
summary(glm_1)
anova(glm_1,test="Chisq")

glm_2= glm(Class ~ Cl.thickness + Cell.size + Cell.shape, 
           data=train_clean,family=binomial(), maxit=100)
anova(glm_2,test="Chisq")



#Knn 

library(class)
train_x<-as.matrix(train_clean[,-1])
train_y<-train_clean$Loan_Status

test_x<-as.matrix(test_clean[,-1])
test_y<-test_clean$Loan_Status



knn.pred= knn(train_x, test_x, train_y, k=50)
table(knn.pred, test_y)
mean(test_y==knn.pred)
anova(knn.pred, glm_2)
