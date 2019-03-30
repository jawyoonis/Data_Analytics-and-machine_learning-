library(ISLR)
library(MASS)
library(car)
library(tidyverse)
library(DataExplorer)
attach(Advertising)

# import dataset here
#Ad <- read.csv("Advertising.csv",header = TRUE,sep = ",")

# take a look at data
#glimpse(Ad)


# Remove X variable and move Sales to first column
Ad_clean <- Advertising %>%
  select(-X) %>%  
  glimpse(Ad_clean)
  select(Sales, everything())          

# Check for missing values
plot_missing(Ad_clean)
plot_histogram(Ad_clean)
plot_density(Ad_clean)


# Run a Pearson correlation on all variables
cor(Ad, method="pearson")


# create training/test datasets for Advertising using sample function
set.seed(2019)
samp <- sample(nrow(Ad_clean), .75*nrow(Ad_clean))
train <- Ad_clean[samp,]
test <- Ad_clean[-samp,]


# use rsample to split into training and test sets
set.seed(2019)
train_test_split <- initial_split(Ad_clean, prop = 0.75)
train_test_split


# Retrieve train and test sets
train <- training(train_test_split)
test  <- testing(train_test_split)


# Check dimensions of train and test data
dim(train)
dim(test)


# generate a fitted LR model using all predictors
Ad.fit <- lm(Sales ~ ., data=train)
summary(Ad.fit)

# generate a fitted LR model using two predictors
Ad.fit1 <- lm(Sales ~ TV + Radio, data=train)
summary(Ad.fit1)

# generate a prediction on a single sample
new <- data.frame(TV=c(10, 50, 100), Radio=c(5,10,20))
Ad.pred <- predict(Ad.fit1, data = new, interval="prediction")
Ad.pred

# generate a new fitted model using square of TV
Ad.fit2 <- lm(Sales ~ TV + Radio + I(TV^2), data=train)
summary(Ad.fit2)

# generate a new fitted model using TV and Radio interaction term
Ad.fit3 <- lm(Sales ~ TV + Radio + I(TV*Radio), data=train)
summary(Ad.fit3)

# determine best model using ANOVA
anova(Ad.fit1, Ad.fit2, Ad.fit3)


# LET THE REGRESSION DIAGNOSTICS BEGIN!

#divide plot window into four sections
par(mfrow=c(2,2))
plot(Ad.fit2) #generates four plots (residuals vs fitted, QQ, residuals vs leverage)

#Non-Linearity of the Data
crPlots(Ad.fit2)
#ceresPlots(Ad.fit2)

# Normality of Residuals
# qq plot for studentized resid
qqPlot(Ad.fit2, main="QQ Plot")

# normal distribution of predictors test
shapiro.test(Advertising$TV)
shapiro.test(Advertising$Radio)

# distribution of studentized residuals
sresid <- studres(Ad.fit2) 

hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")

xfit<-seq(min(sresid),max(sresid),length=40) 

yfit<-dnorm(xfit) 
lines(xfit, yfit)

# Autocorrelation of Error Terms
durbinWatsonTest(Ad.fit2)

#Non-Constant Variance of Error Terms (Heteroscedasticity)
ncvTest(Ad.fit2)
spreadLevelPlot(Ad.fit2)

#Outliers
outlierTest(Ad.fit2) # Bonferonni p-value for most extreme obs

leveragePlots(Ad.fit2) # leverage plots

cutoff <- 4/((nrow(Advertising)-length(fit$coefficients)-2)) 
plot(Ad.fit2, which=4, cook.levels=cutoff)
influencePlot(Ad.fit2,	id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

#Collinearity
vif(Ad.fit2) # variance inflation factors 


# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(Ad.fit2) 
summary(gvmodel)