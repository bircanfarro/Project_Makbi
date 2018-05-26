library(MASS) 
library(car)
library(psych)
#train = read.csv('./data/train.csv') 
train = read.csv('train.csv')


numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later
train_numVar <- train[, numericVars]

# How many rows are we left with after na.omit()
train_na_free = na.omit(train_numVar)
print(nrow(train_numVar))          #1460 rows
print(nrow(train_na_free))  #1121 rows


# replace YearBuilt, YearRemodAdd, GarageYrBlt by AgeBuilt, AgeRemodAdd, AgeGarageBlt respectively
train_numVar$AgeBuilt     = 2018 - train$YearBuilt
train_numVar$AgeRemodAdd  = 2018 - train$YearRemodAdd
train_numVar$AgeGarageBlt = 2018 - train$GarageYrBlt


# ALIASED!
# model.1 = lm(SalePrice ~ ., data = na.omit(train_numVar))
# summary(model.1)
# vif(model.1)
# alias(model.1)


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


# #marius' model
# model.5 = lm(SalePrice ~ GrLivArea + GarageArea + TotalBsmtSF + X1stFlrSF + MasVnrArea + 
#                BsmtFinSF1 + WoodDeckSF + LotFrontage + X2ndFlrSF + OpenPorchSF + LotArea + 
#                BsmtUnfSF + PoolArea  + X3SsnPorch + LowQualFinSF + BsmtFinSF2, data = na.omit(train_numVar))
# 
# vif(model.5) #Error in vif.default(model.5) : there are aliased coefficients in the model
# alias(model.5)

#The model was found based on AIC 
model.6 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
               GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))

vif(model.6) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 

# best model so far but includes some categorical values, so we alternate to the model 8
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               AgeRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr +  MSSubClass + AgeBuilt + OverallCond + OpenPorchSF +
               ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))
AIC(model.7) #26834
vif(model.7) #all under 3.53
summary(model.7) # BsmtHalfBath, OpenPorchSF, TotalBsmtSF are not significant. Anyways, model is flawed because of categorical vars e.g. MSSubClass

model.8 = lm(SalePrice ~  GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               AgeRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr +  MSSubClass + AgeBuilt  + OpenPorchSF +
               ScreenPorch + BsmtFullBath, data = na.omit(train_numVar))
AIC(model.8) #27040
summary(model.8) #BsmtFinSF1, OpenPorchSF are not statistically significant


model.9 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + TotalBsmtSF + WoodDeckSF +
               AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                 MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.9) #26972
vif(model.9) # all VIFs<5, highest VIF=4.18 (TotalBsmstSF)
summary(model.9) # all are statistically significant, BsmtFinSF1 marginally so

# so here we attempt to eliminate TotalBsmstSF, because previously we got highest VIF on it
model.11 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
               AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
               MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.11) #26980
vif(model.11) # all under 2.74
summary(model.11) # all are statistically significant, BsmtFinSF1 is **-signficant from marginally significant, so it was pushed into insignificance by TotalBsmtSF
influencePlot(model.11)
avPlots(model.11)
confint(model.11)

# Since we left MSSubClass in, I wonder how strongly is it correlated with SalePrice
cor(train_numVar$SalePrice, train_numVar$MSSubClass) #-0.084, not surprisingly weakly correlated
#                Estimate Std. Error t value Pr(>|t|)    
# MSSubClass   -1.575e+02  3.475e+01  -4.533 6.46e-06 ***
# Notice that MSSubClass was highly significant. So a randomly coded categorical variables is highly signficant. 
# What's going on in here ?
# From the description, we see MSSubClass coding may be aligned (correlated) with the number of floors, and that's a feature weakly correlated with anything else.
# So my guess is that it is adding an orthogonal component to the Span(X), so it must increase R^2 and reduce AIC.
# MSSubClass: Identifies the type of dwelling involved in the sale.
# 20	1-STORY 1946 & NEWER ALL STYLES
# 30	1-STORY 1945 & OLDER
# 40	1-STORY W/FINISHED ATTIC ALL AGES
# 45	1-1/2 STORY - UNFINISHED ALL AGES
# 50	1-1/2 STORY FINISHED ALL AGES
# 60	2-STORY 1946 & NEWER
# 70	2-STORY 1945 & OLDER
# 75	2-1/2 STORY ALL AGES
# 80	SPLIT OR MULTI-LEVEL
# 85	SPLIT FOYER
# 90	DUPLEX - ALL STYLES AND AGES
# 120	1-STORY PUD (Planned Unit Development) - 1946 & NEWER
# 150	1-1/2 STORY PUD - ALL AGES
# 160	2-STORY PUD - 1946 & NEWER
# 180	PUD - MULTILEVEL - INCL SPLIT LEV/FOYER
# 190	2 FAMILY CONVERSION - ALL STYLES AND AGES

#Added BsmtUnfSF but that introduced some collinearity with BsmtFinSF1. Better AIC but multicolliniarity
model.12 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = na.omit(train_numVar))

vif(model.12)
AIC(model.12) #26973

model.13 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
  Age RemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
  MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
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


# Best predictors from forward AIC (Marius):
bestFwdAIC = c("GarageCars", "X1stFlrSF", "X2ndFlrSF", "AgeBuilt", "KitchenAbvGr", "BsmtFinSF1", "AgeRemodAdd", "MasVnrArea", 
               "Fireplaces", "BedroomAbvGr", "TotRmsAbvGrd",  "LotArea", "ScreenPorch", "BsmtUnfSF", "BsmtFullBath", "PoolArea", 
               "WoodDeckSF", "FullBath", "BsmtFinSF2") 


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
