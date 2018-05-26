library(car)
library(psych)
train = read.csv('./data/train.csv') 
#train = read.csv('train.csv')

#bircan

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
AIC(model.4) #26966.64 - AIC is still higher than model.2 



# adding X1stFlrSF, X2ndFlrSF that created multiclliniearity in model.2
model.5 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = na.omit(train_numVar))
vif(model.5) # no colliniearity
AIC(model.5) #26780.55 - AIC is much better than model 3 little higher than model 2 but no colliniearity



# adding TotRmsAbvGrd that created multiclliniearity in model.2
model.6 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + TotRmsAbvGrd +
               WoodDeckSF + ScreenPorch + PoolArea, data = na.omit(train_numVar))
vif(model.6) #no colliniearity
AIC(model.6) #26872.29 - model 5 is the best so far

# #marius' model
# model.5 = lm(SalePrice ~ GrLivArea + GarageArea + TotalBsmtSF + X1stFlrSF + MasVnrArea + 
#                BsmtFinSF1 + WoodDeckSF + LotFrontage + X2ndFlrSF + OpenPorchSF + LotArea + 
#                BsmtUnfSF + PoolArea  + X3SsnPorch + LowQualFinSF + BsmtFinSF2, data = na.omit(train_numVar))
# 
# vif(model.5) #Error in vif.default(model.5) : there are aliased coefficients in the model
# alias(model.5)


#The model was found based on AIC forward method
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
               GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))
vif(model.7) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 
AIC(model.7) #26789.89

#difference 7 from 5 GrLivArea, TotalBsmtSF, YearRemodAdd, GarageArea, TotRmsAbvGrd, OpenPorchSF, BsmtHalfBath
#diffrence 5 from 7  LotFrontage, BsmtFullBath, X2ndFlrSF, X1stFlrSF, PoolArea, WoodDeckSF

# best model so far but includes some categorical values, so we alternate to the model 8
model.7 = lm(SalePrice ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
               AgeRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr +  MSSubClass + AgeBuilt + OverallCond + OpenPorchSF +
               ScreenPorch + BsmtHalfBath, data = na.omit(train_numVar))
AIC(model.7) #26834
vif(model.7) #all under 3.53
summary(model.7) # BsmtHalfBath, OpenPorchSF, TotalBsmtSF are not significant. Anyways, model is flawed because of categorical vars e.g. MSSubClass

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

#Changing GrLivArea with X2ndFlrSF and X1stFlrSF and adding TotRmsAbvGrd 
model.8a = lm(SalePrice ~ GrLivArea + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
               BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
               PoolArea + YearRemodAdd + ScreenPorch + 
               WoodDeckSF + Fireplaces, data = na.omit(train_numVar))
vif(model.8a)
AIC(model.8a) #26978.12 the result is so close with the one above


model.9 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + TotalBsmtSF + WoodDeckSF +
               AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                 MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.9) #26972
vif(model.9) # all VIFs<5, highest VIF=4.18 (TotalBsmstSF)
summary(model.9) # all are statistically significant, BsmtFinSF1 marginally so

# so here we attempt to eliminate TotalBsmstSF, because previously we got highest VIF on it
#marius & bircan
  
# the model was decided the best with marius on Friday seems matched with my updated seperate features.
model.11 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
               AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
               MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr +
               ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.11) #26980
vif(model.11) # all under 2.74
summary(model.11) # all are statistically significant, BsmtFinSF1 is **-signficant from marginally significant, so it was pushed into insignificance by TotalBsmtSF
#Adjusted R-squared:  0.7631
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

# WITHOUT MSSubClass!!
model.11.1 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                  AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                  AgeBuilt + X1stFlrSF + BedroomAbvGr +
                  ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = na.omit(train_numVar))
AIC(model.11.1) #26998, increased from 26980 when we dropped MSSubClass
vif(model.11.1) # all under 2.308, decreased from 2.74 when we dropped MSSubClass
summary(model.11.1) # all our 15 variables are statistically significant


# adding BsmtUnfSF features
model.12 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + AgeBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = na.omit(train_numVar))
vif(model.12) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.12) #26973.98 - better AIC
summary(model.12) #Adjusted R-squared:  0.7646

# WITHOUT MSSubClass!! adding BsmtUnfSF features
model.12.1 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                AgeRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                AgeBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = na.omit(train_numVar))
vif(model.12.1) # multicolliniearity X1stFlrSF, BsmtFinSF1. we know that cor(TotalBsmtSF, X1stFlrSF) = 0.82
AIC(model.12.1) # 26991, increased from 26973.98 when we dropped MSSubClass
summary(model.12.1) #Adjusted R-squared: 0.7607, decreased from 0.7646 when we dropped MSSubClass

#extracting BsmtUnfSF and adding Full Bath
model.13 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.13) # no colliniearity
AIC(model.13) #26974.69
summary(model.13) #Adjusted R-squared:  0.7644

# WITHOUT MSSubClass!! extracting BsmtUnfSF and adding Full Bath
model.13.1 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                YearBuilt + X1stFlrSF + BedroomAbvGr +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.13.1) # no colliniearity
AIC(model.13.1) #26995.82, increased from 26974.69 when we dropped MSSubClass
summary(model.13.1) #Adjusted R-squared:  0.7597, decreased from 0.7644 when we dropped MSSubClass

#adding BsmtUnfSF again
model.14 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
              YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
              MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
              ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.14) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.14) #26968.63
summary(model.14) #Adjusted R-squared:  0.7659

# WITHOUT MSSubClass!! adding BsmtUnfSF again
model.14.1 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.14.1) # multicolliniearity X1stFlrSF, BsmtFinSF1
AIC(model.14.1) # 26988, increased from 26968.63 when we dropped MSSubClass
summary(model.14.1) #Adjusted R-squared: 0.7616 decreased from 0.7659 when we dropped MSSubClass

#switching  BsmtUnfSF  to BsmtFinSF2
model.15 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + 
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.15) #no multicolliniearity
AIC(model.15) #26976.68
summary(model.15) #Adjusted R-squared:  0.7642

# WITHOUT MSSubClass!! switching  BsmtUnfSF  to BsmtFinSF2
model.15.1 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
                YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                YearBuilt + X1stFlrSF + BedroomAbvGr + 
                ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
vif(model.15.1) #no multicolliniearity
AIC(model.15.1) # 26997.8, increased from 26976.68 when we dropped MSSubClass
summary(model.15.1) #Adjusted R-squared:0.7595, decreased from 0.7642 when we dropped MSSubClass


#Conclusion :  the model.11 to model.15 have very close results. at this moment we want the one has least colliniearity, AIC and 
#more features (?) which is model.13

influencePlot(model)
avPlots(model)
confint(model)




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

