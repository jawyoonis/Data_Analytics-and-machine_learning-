library(mlbench)
library(ISLR)
library(caret)
library(DataExplorer)
library(tidyverse)
library(car)
library(MASS)
library(ROCR)
library(polycor)
library(corrplot)

data("PimaIndiansDiabetes2")
pima<-PimaIndiansDiabetes2
dim(pima)
glimpse(pima)
plot_missing(pima)
glimpse(pima)
#correlations <- cor(pima)
#corrplot(correlations, method="circle")
#correlation Check 
cor(pima, method="person")
hetcor(pima)

set.seed(2020)
train_test_split<-initial_split(pima, prob=0.5)
train_test_split

train_tb1 <-training(train_test_split)
train_tb1
test_tb1 <-testing(train_test_split)
test_tb1

#recipe
rec_obj <-recipe(outcome~,  data= train_tb1) %>%
  
#step_bagimute( all_predictors(), -all_outcomes())

train_clean<- bake(rec_obj, new_data, train_tb1)
test_clean<-bake(rec_obj, new_data, test_tb1)
#logicstic modal 
log.fit<- glm( outcome, ~., data=train_clean, 
               family = binomial(link="logit"), maxit=100)

summary(log.fit)

anova(log.fit, test="chisq")# comparing the standard statistics validity 
#variance inflation vector 

vif(log.fit) # fector inflation to check colinearity not larger than 10
durbinwatsonTest(log.fit) # we fail to reject the null hypothesis because P-value >0.05
log.prob<- predict(log.fit, newdata= test_clean, type="response")
# to build the confusion matrix 
log.pred<- infelse(log.prob >.05, "pos", "neg") 
# if it is log.prob> 0.5 set that to positive otherwise negative

table(log.pred, test_clean$outcome) #confusion Matrix 
mean(log.pred==test_clean$outcome) # accuracy of the coeffiences

#plot ROc chart and compute area under curve 
p<- predict( log.fit, newdata=test_clean, response="response")
pr <- prediction( p, test_clean$outcome)
prf <- performance( pr, measure="tnr",  x.measure = "fnr")
 plot(prf)
 auc<- performance(pr, measure = "auc") # 
 auc<- auc @y.values[[1]]
 auc 
# tnr= true negative rate 
 # fnr= false negative rate 
 #auc= area under the curve 
## logistic diagnostic Linearity 

mydata <-test_tb1%>%
dplyr::select_if(is.numeric())
predictors<- colnames(mydata)

#bind the logit and tidying the data for plot 
mydata<- mydata %>%
mutate(logit=log(log.prob)/(l-log.prob)) %>%
gather(key = 'predictors', value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+ 
  geom_point(size=0.5, alpha=.5)+
  geom_smooth(method = "loess")+
  theme_bw()+ facet_wrap(~predictors, scales="free_y")


#outliers and fluences points 
plot(log.fit, which=4, id.n=3)

#extract model results 


model.data <- augment(log.fit) %>%
  mutate(index= 1:n())


model.data %>% top_n(3, cooksd)

ggplot(model.data, eas(index, .std.resid))
geom_point(aes(color=outcome), alpha=.5) +
  theme_bw()



model.data %>%
  filter(abs(.std.resid)) #shows true outliers here 














  
  
  
  
  







