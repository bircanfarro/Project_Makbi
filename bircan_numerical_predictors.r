library(car)
library(psych)
train = read.csv('./data/train.csv') 


#bircan

numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later
train_numVar <- train[, numericVars]


# creating full linear model and summarise to see the significance of each variables
model.1 = lm(SalePrice ~ ., data = na.omit(train_numVar))
summary(model.1)
vif(model.1)
alias(model.1)



# Model formed with the only variables that has the highest significany
model.2 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))
vif(model.2) #creates some colliniearity
AIC(model.2) #26768.64



# extract the variables that creates colliniearity
model.3 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + 
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))
vif(model.3) #no colliniearity
AIC(model.3) #26987.86 - AIC got higher, model.2 is better



# adding X1stFlrSF that created multiclliniearity in model.2
model.4 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + 
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = na.omit(train_numVar))
vif(model.4) # no colliniearity
AIC(model.4) #26966.64 - AIC is better than model.2 



# adding X1stFlrSF, X2ndFlrSF that created multiclliniearity in model.2
model.5 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = na.omit(train_numVar))
vif(model.5) # no colliniearity
AIC(model.5) #26780.55 - AIC is much better than model.2 and model 3



# adding TotRmsAbvGrd that created multiclliniearity in model.2
model.6 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + TotRmsAbvGrd +
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))
vif(model.6) #no colliniearity
AIC(model.6) #26872.29 - model 5 is the best so far



#The model was found based on AIC forward method
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
               GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))
vif(model.7) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 
AIC(model.7) #26789.89

#difference 7 from 5 GrLivArea, TotalBsmtSF, YearRemodAdd, GarageArea, TotRmsAbvGrd, OpenPorchSF, BsmtHalfBath
#diffrence 5 from 7  LotFrontage, BsmtFullBath, X2ndFlrSF, X1stFlrSF, PoolArea, WoodDeckSF

# mutual features between model 5 and 7
model.7a = lm(SalePrice ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea +
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + ScreenPorch, data = na.omit(train_numVar))

vif(model.7a)
AIC(model.7a) #27028.79


# adding some features from the difference (YearRemodAdd, BsmtFullBath, PoolArea, X2ndFlrSF, X1stFlrSF, 
#OverallCond, WoodDeckSF  additionally Fireplaces
model.7b = lm(SalePrice ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                OverallCond + WoodDeckSF + Fireplaces, data = na.omit(train_numVar))

vif(model.7b) # no colliniearity
AIC(model.7b) #26781.16




# decided that the model includes some categorical variables like OverallQual, OverallCond. so now extracting those
model.8 = lm(SalePrice ~ BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                WoodDeckSF + Fireplaces, data = na.omit(train_numVar))

vif(model.8)
AIC(model.8) #26980.15 - AIC has increased as expected, but still pretty good

#this model matches with the model was discussed with marius
---------------------------------------------------------------------------------------------------------------
#marius & bircan
  
# the model was decided the best with marius on Friday seems matched with my updated seperate features.
model.11 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
               YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
               MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
vif(model.11)
AIC(model.11) #26980.15
summary(model.11) #Adjusted R-squared:  0.7631 



# adding BsmtUnfSF features
model.12 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = na.omit(train_numVar))

vif(model.12) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.12) #26973.98 - better AIC
summary(model.12) #Adjusted R-squared:  0.7646



#extracting BsmtUnfSF and adding Full Bath
model.13 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.13) # no colliniearity
AIC(model.13) #26974.69
summary(model.13) #Adjusted R-squared:  0.7644



#adding BsmtUnfSF again
model.14 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
              YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
              MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
              ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.14) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.14) #26968.63
summary(model.14) #Adjusted R-squared:  0.7659



#switching  BsmtUnfSF  to BsmtFinSF2
model.15 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + 
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.15) #no multicolliniearity
AIC(model.15) #26976.68
summary(model.15) #Adjusted R-squared:  0.7642



#Conclusion :  the model.11 to model.15 have very close results. at this moment we want the one has least colliniearity, AIC and 
#more features (?) which is model.13


influencePlot(model)
avPlots(model)
confint(model)



