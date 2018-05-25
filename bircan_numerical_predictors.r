library(MASS) 
library(car)
library(psych)
train = read.csv('./data/train.csv') 



numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later
train_numVar <- train[, numericVars]


model.1 = lm(SalePrice ~ ., data = na.omit(train_numVar))
summary(model.1)
vif(model.1)
alias(model.1)


model.2 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))

vif(model.2)



model.3 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))
vif(model.3)



model.4 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + GrLivArea, data = na.omit(train_numVar))
vif(model.4)


#marius' model
model.5 = lm(SalePrice ~ GrLivArea + GarageArea + TotalBsmtSF + X1stFlrSF + MasVnrArea + 
               BsmtFinSF1 + WoodDeckSF + LotFrontage + X2ndFlrSF + OpenPorchSF + LotArea + 
               BsmtUnfSF + PoolArea  + X3SsnPorch + LowQualFinSF + BsmtFinSF2, data = na.omit(train_numVar))

vif(model.5) #Error in vif.default(model.5) : there are aliased coefficients in the model
alias(model.5)

#The model was found based on AIC 
model.6 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
               GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))

vif(model.6) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 

# best model so far but includes some categorical values, so we alterate to the model 8
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr +  MSSubClass + YearBuilt + OverallCond + OpenPorchSF +
               ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))

vif(model.7)

model.8 = lm(SalePrice ~  GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr +  MSSubClass + YearBuilt  + OpenPorchSF +
               ScreenPorch + BsmtFullBath, data = na.omit(train_numVar))
AIC(model.8) #27040
summary(model.8)


model.9 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + TotalBsmtSF + WoodDeckSF +
               YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                 MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.9) #26972


model.11 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
               YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
               MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.11) #26980

vif(model.11)
summary(model.11)
influencePlot(model.11)
avPlots(model.11)
confint(model.11)


#better AIC but multicolliniarity
model.12 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = na.omit(train_numVar))

vif(model.12)
AIC(model.12)

model.13 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
  MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
  ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))

vif(model.13)
AIC(model.13) #26968.63



model.14 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))

vif(model.14)
AIC(model.14) #26968.63
 
model.15 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + 
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))


vif(model.15)
AIC(model.15) #26968.63

model.16 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))

vif(model.16)
AIC(model.16) #26968.63


#testing GarageCars vs GarageArea
model.10 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + TotalBsmtSF + WoodDeckSF +
               YearRemodAdd + LotArea + GarageCars + MasVnrArea + 
               MSSubClass + YearBuilt + X1stFlrSF + 
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))

summary(model.10)

model.10a = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + TotalBsmtSF + WoodDeckSF +
               YearRemodAdd + LotArea + GarageArea + MasVnrArea + 
               MSSubClass + YearBuilt + X1stFlrSF + 
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
summary(model.10a)





# AIC(model.1) #26789.4
# AIC(model.2) #26768.64
# AIC(model.3) #26987.86
# AIC(model.4) #26777.7
# AIC(model.5) #27332.17
# AIC(model.6) #26098.95 but there are multicollinierity
# AIC(model.7) #26128.47  
# 
# 
# AIC(model.8) #27041.71, 27014.39
# 
# #model.7 seems the best numerical model with the least multicollinieraity and the second lowest AIC
# 
# 
# summary(model.7)
# #Multiple R-squared:  0.871,
# #Adjusted R-squared:  0.8691
# #F-statistic: 464.1 on 16 and 1100 DF,  p-value: < 2.2e-16
# plot(model.7)
# influencePlot(model.7)
# vif(model.7)
# avPlots(model.7)
# confint(model.7) # 97.5% confidence
# 
# summary(model.8)
