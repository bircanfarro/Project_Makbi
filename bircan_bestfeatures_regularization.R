library(car)
library(psych)
train = read.csv('train.csv') 



numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later
train_numVar <- train[, numericVars]
data1 = na.fail(train_numVar) #Error in na.fail.default(train_numVar) : missing values in object
data2 = na.omit(train_numVar) #1121 obs
data3 = na.exclude(train_numVar) 
 


# creating full linear model and summarise to see the significance of each variables
model.1 = lm(SalePrice ~ ., data = data1)
summary(model.1)
vif(model.1)
alias(model.1)



# Model formed with the only variables that has the highest significany
model.2 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea, data = data1)
vif(model.2) #creates some colliniearity
AIC(model.2) #26768.64 ,28531.87



# extract the variables that creates colliniearity
model.3 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + 
               WoodDeckSF + ScreenPorch + PoolArea, data = data1)
vif(model.3) #no colliniearity
AIC(model.3) #26987.86 - AIC got higher, model.2 is better, 28784.8



# adding X1stFlrSF that created multiclliniearity in model.2
model.4 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + 
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = data1)
vif(model.4) # no colliniearity
AIC(model.4) #26966.64 - AIC is still higher than model.2 , 28755.32



# adding X1stFlrSF, X2ndFlrSF that created multiclliniearity in model.2
model.5 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = data1)
vif(model.5) # no colliniearity
AIC(model.5) #26780.55 - AIC is much better than model 3 little higher than model 2 but no colliniearity



# adding TotRmsAbvGrd that created multiclliniearity in model.2
model.6 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + TotRmsAbvGrd +
               WoodDeckSF + ScreenPorch + PoolArea, data = data1)
vif(model.6) #no colliniearity
AIC(model.6) #26872.29 - model 5 is the best so far



#The model was found based on AIC forward method
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
               GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = data1)
vif(model.7) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 
AIC(model.7) #26789.89

#difference 7 from 5 GrLivArea, TotalBsmtSF, YearRemodAdd, GarageArea, TotRmsAbvGrd, OpenPorchSF, BsmtHalfBath
#diffrence 5 from 7  LotFrontage, BsmtFullBath, X2ndFlrSF, X1stFlrSF, PoolArea, WoodDeckSF

# mutual features between model 5 and 7
model.7a = lm(SalePrice ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea +
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + ScreenPorch, data = data1)

vif(model.7a)
AIC(model.7a) #27028.79


# adding some features from the difference (YearRemodAdd, BsmtFullBath, PoolArea, X2ndFlrSF, X1stFlrSF, 
#OverallCond, WoodDeckSF  additionally Fireplaces
model.7b = lm(SalePrice ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                OverallCond + WoodDeckSF + Fireplaces, data = data1)

vif(model.7b) # no colliniearity
AIC(model.7b) #26781.16




# decided that the model includes some categorical variables like OverallQual, OverallCond. so now extracting those
model.8 = lm(SalePrice ~ BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                WoodDeckSF + Fireplaces, data = data1)
vif(model.8)
AIC(model.8) #26980.15 - AIC has increased as expected, but still pretty good

#this model matches with the model was discussed with marius

#Changing GrLivArea with X2ndFlrSF and X1stFlrSF and adding TotRmsAbvGrd 
model.8a = lm(SalePrice ~ GrLivArea + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
               PoolArea + YearRemodAdd + ScreenPorch + 
               WoodDeckSF + Fireplaces, data = data1)
vif(model.8a)
AIC(model.8a) #26978.12 the result is so close with the one above

 ---------------------------------------------------------------------------------------------------------------
#marius & bircan
  
# the model was decided the best with marius on Friday seems matched with my updated seperate features.
model.11 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
               YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
               MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = data1)
vif(model.11)
AIC(model.11) #26980.15
summary(model.11) #Adjusted R-squared:  0.7631 



# adding BsmtUnfSF features
model.12 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = data1)

vif(model.12) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.12) #26973.98 - better AIC
summary(model.12) #Adjusted R-squared:  0.7646



#extracting BsmtUnfSF and adding Full Bath
model.13 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = data1)
vif(model.13) # no colliniearity
AIC(model.13) #26974.69
summary(model.13) #Adjusted R-squared:  0.7644



#adding BsmtUnfSF again
model.14 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
              YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
              MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
              ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = data1)
vif(model.14) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.14) #26968.63
summary(model.14) #Adjusted R-squared:  0.7659



#switching  BsmtUnfSF  to BsmtFinSF2
model.15 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + 
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = data1)
vif(model.15) #no multicolliniearity
AIC(model.15) #26976.68
summary(model.15) #Adjusted R-squared:  0.7642



#Conclusion :  the model.11 to model.15 have very close results. at this moment we want the one has least colliniearity, AIC and 
#more features (?) which is model.13


influencePlot(model)
avPlots(model)
confint(model)



###################################################
###### REGULARIZATION AND CROSS  VALIDATION #######
###################################################

library(ISLR)
library(glmnet)
train = read.csv('./data/train_superclean.csv') 



#####Ridge Regression#####

x = model.matrix(SalePrice ~  OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                   BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                   WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                   Utilities +  MSZoning  + SaleCondition + Functional + Street + GarageQual +  
                   MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                   CentralAir)[, -1] #Dropping the intercept column.
y = train_train$SalePrice

grid = 10^seq(5, -2, length = 100)


ridge_model = glmnet(x, y, alpha = 0, lambda = grid)
plot(ridge_model, xvar = "lambda", label = TRUE, main = "Ridge Regression")


#Creating training and testing sets with 70-30 split 

set.seed(0)
train_train = sample(1:nrow(x), 7*nrow(x)/10)
train_test = (-train_train)
y.test = y[train_test]


#Running 5-fold cross validation.
set.seed(0)
cv.ridge = cv.glmnet(x[train_train, ], y[train_train],
                     lambda = grid, alpha = 0, nfolds = 5)
plot(cv.ridge, main = "Ridge Regression\n")
best.lambda.ridge = cv.ridge$lambda.min
best.lambda.ridge # 12045.04
log(best.lambda.ridge) # 9.396408

#What is the test MSE associated with this best value of lambda?
ridge.models.train = glmnet(x[train_train, ], y[train_train], alpha = 0, lambda = grid)
ridge.best.lambda.train = predict(ridge.models.train, s = best.lambda.ridge, newx = x[train_test, ])
mean((ridge.best.lambda.train - y.test)^2) # 2,294.467.104

ridge.best.lambda.train = predict.cv.glmnet(cv.ridge, s ="lambda.min", newx = x[train_test, ])
mean((ridge.best.lambda.train - y.test)^2)


#####Lasso Regression#####

lasso_model = glmnet(x, y, alpha = 1, lambda = grid)
plot(lasso_model, xvar = "lambda", label = TRUE, main = "Lasso Regression")

lasso_model$lambda1se

#Running 5-fold cross validation.
set.seed(0)
cv.lasso = cv.glmnet(x[train_train, ], y[train_train],
                     lambda = grid, alpha = 1, nfolds = 5)
plot(cv.lasso, main = "Lasso Regression\n")
best.lambda.lasso = cv.lasso$lambda.min
best.lambda.lasso #394.4206
log(best.lambda.lasso) #5.977418


#What is the test MSE associated with this best value of lambda?
lasso.model.train = glmnet(x[train_train, ], y[train_train], alpha = 1, lambda = grid)
lasso.best.lambda.train = predict(lasso.model.train, s = best.lambda.lasso, newx = x[train_test, ])
mean((lasso.best.lambda.train - y.test)^2) #2,324.445.074


