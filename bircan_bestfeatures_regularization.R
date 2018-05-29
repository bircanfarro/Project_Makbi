
###################################################
########## NUMERIC FEATURES SELECTION  ############
###################################################


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




library(car)
library(psych)
train = read.csv('./data/train_superclean.csv') 



sapply(train, class)



categoricalVars <- which(sapply(train, is.factor))
CategoricalVarNames <- names(categoricalVars)
train_catVar <- train[, categoricalVars]
train_catVar = cbind(train_catVar, SalePrice = train$SalePrice)


# model.15 = lm(SalePrice ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
#                 YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
#                 MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + 
#                 ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = na.omit(train_numVar))
# 
# 
# 
# for (c in CategoricalVarNames)
# {
#   if ( is.factor(train2[[c]]) && length(levels(train2[[c]]))<2) print(c)
# }
# 
# 
# 
# 
# a = lm(SalePrice ~ HouseStyle + PavedDrive + Street, data = train)
# summary(a)
# 
# CategoricalVarNames 
# 
# 
# impute_levels = function(fac){
#   if (is.factor(fac))
#   {
#     if (length(levels(fac))<=2)
#     {print(fac)
#       return (as.numeric(fac)-1)}
#   }
#   return(fac)
# }
#  
#  for (i in CategoricalVarNames)
#  {
#    if (is.factor(train$i)) train$i.ct = C(train$i.f, treatment)
#  }
# 
# 
# 
# for (i in CategoricalVarNames)
# {
#   if (is.factor(train$i)) print(attributes(train$i.ct))
# }
# 
# 
# train1 <- within(train, {
#   HouseStyle.ct <- C(HouseStyle.f, treatment)
#   print(attributes(HouseStyle.ct))
# })
# 
# 
# m2 = lm(SalePrice ~ HouseStyle.ct,  data = train1)
# summary(m2)





######  MODEL FOR CATEGORICAL DATA ######
model.empty.cat = lm(SalePrice ~ 1, data = train_catVar) #The model with an intercept ONLY.
model.full.cat = lm(SalePrice ~ ., data = train_catVar) #The model with ALL variables.
scope = list(lower = formula(model.empty.cat), upper = formula(model.full.cat))


#Forward AIC on categorical variables
forwardAICat = step(model.empty, scope, direction = "forward", k = 2)

# Step:  AIC=30429.52
# SalePrice ~ Neighborhood + KitchenQual + BsmtQual + FireplaceQu + 
#   BldgType + BsmtExposure + HouseStyle + RoofMatl + ExterQual + 
#   PoolQC + GarageFinish + Exterior1st + MasVnrType + Condition2 + 
#   BsmtFinType1 + LotConfig + SaleCondition + RoofStyle + HeatingQC + 
#   LotShape + CentralAir + Heating + GarageQual + GarageCond + 
#   Condition1 + MSZoning + Alley + SaleType + Utilities



#Forward BIC on categorical variables
forwardBICat = step(model.empty, scope, direction = "forward", k = log(1460))

# Step:  AIC=30969.78
# SalePrice ~ Neighborhood + KitchenQual + BsmtQual + FireplaceQu + 
#   BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
#   HouseStyle + GarageFinish + CentralAir

summary(forwardAICat)
summary(forwardBICat)
vif(forwardAICat)
alias(forwardAICat)
vif(forwardBICat)

summary(train_catVar)



###################################################
#######  CATEGORICAL FEATURES SELECTION  ##########
###################################################

# less important ; Alley + MSZoning, LotShape , MasVnrType,
# unneceseray variables :Condition2 + LotConfig + HeatingQC , Heating , Condition1, SaleType, 
# variables that creates multicolliniearity:
# Neighborhood, BsmtQual, BsmtFinType1, RoofStyle , GarageQual, GarageCond 

# baseline is BIC >> adding features one by one from AIC model to find the best predictors
model.0 = lm(SalePrice ~  KitchenQual +  FireplaceQu + 
               BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.0) # Adjusted R-squared:  0.7234, F-statistic: 96.42, p-value: < 2.2e-16
vif(model.0) # ExterQual,KitchenQual
AIC(model.0) # 35254.86


model.1 = lm(SalePrice ~  KitchenQual +  FireplaceQu + SaleCondition +
               BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.1) # Adjusted R-squared:  0.7289, F-statistic: 88.17, p-value: < 2.2e-16
vif(model.1) # salecondition is significant but reduces F ??
AIC(model.1) # 35230.71



model.2 = lm(SalePrice ~  KitchenQual +  FireplaceQu +  Utilities + 
               BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.2) # Adjusted R-squared:  0.7233 ,F-statistic: 94.04,  p-value: < 2.2e-16
vif(model.2) 
AIC(model.2) # 35256.4



model.3 = lm(SalePrice ~  KitchenQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
               LotShape + MasVnrType + BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.3) #Adjusted R-squared:  0.7406, F-statistic: 79.6
vif(model.3)
AIC(model.3) # 35173.91



#based on p values
model.4 = lm(SalePrice ~  MSZoning + LotConfig + BldgType + HouseStyle + RoofMatl + 
               ExterQual  + BsmtExposure + KitchenQual +   
               GarageFinish + CentralAir , data = train_catVar)

summary(model.4) #Adjusted R-squared:  0.6977 F-statistic:  85.2
vif(model.4)
AIC(model.4) # 35384.61



model.5 = lm(SalePrice ~  KitchenQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
               LotShape + MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.5) #Adjusted R-squared:  0.7093 F-statistic:  72.21
vif(model.5) # best
AIC(model.5)  # 35337.21



model.6 = lm(SalePrice ~  ExterQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
               LotShape + MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC + 
               HouseStyle + GarageFinish + CentralAir , data = train_catVar)

summary(model.6) #Adjusted R-squared:  0.7119 F-statistic:  73.1 
vif(model.6) # best
AIC(model.6)  # 35324.3



##########################################
#######  FULL MODEL SELECTION  ##########
#########################################


model.n = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
               YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
               BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
               WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF +
               ExterQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
               LotShape + MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC +
               HouseStyle + GarageFinish + CentralAir , data = train)
summary(model.n)
vif(model.n) # PoolQC , BldgType, PoolArea,MSSubClass, X2ndFlrSF , Fireplaces , FireplaceQu, RoofMatl 
AIC(model.n) #34156.28

# Step:  AIC=29401.39
# SalePrice ~ OverallQual  
#   RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
#   Condition2 + SaleCondition + OverallCond + YearBuilt + LotArea + 
#   PoolQC + ExterQual + GarageArea + TotalBsmtSF + BldgType + 
#   Functional + BedroomAbvGr + Condition1 + PoolArea + ScreenPorch + 
#   LowQualFinSF + LandContour + Street + LandSlope + KitchenAbvGr + 
#   GarageFinish + MasVnrArea + Exterior1st + TotRmsAbvGrd + 
#   LotConfig + MSZoning + GarageCars + Fireplaces + YearRemodAdd + 
#   GarageQual + GarageCond + WoodDeckSF + BsmtFullBath + X1stFlrSF + 
#   Fence + MoSold

# less important ; Alley + MSZoning, LotShape , MasVnrType,
# unneceseray variables :Condition2 + LotConfig + HeatingQC , Heating , Condition1, SaleType, 
# variables that creates multicolliniearity:
# Neighborhood, BsmtQual, BsmtFinType1, RoofStyle , GarageQual, GarageCond 

model.n1 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Exterior1st +
                WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual +
                Utilities +  MSZoning  + SaleCondition + Functional + TotRmsAbvGrd +
                LotShape + MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                GarageFinish + CentralAir , data = train)
summary(model.n1) #Adjusted R-squared:  0.8786 , F-statistic: 128.2
vif(model.n1) # TotRmsAbvGrd Exterior1st YearBuilt 
AIC(model.n1) # 34094.42

# Step:  BIC=29907.7
# SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtQual + 
#   RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
#   Condition2 + OverallCond + YearBuilt + LotArea + GarageArea + 
#   TotalBsmtSF + PoolArea + ExterQual + SaleCondition + KitchenAbvGr + 
#   X2ndFlrSF + BedroomAbvGr + PoolQC + ScreenPorch + TotRmsAbvGrd + 
#   MasVnrArea + Street + X1stFlrSF

model.n2 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                Utilities +  MSZoning  + SaleCondition + Functional + Street + GarageQual +  
                MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                CentralAir , data = train)
summary(model.n2) #Adjusted R-squared:  0.8925 , F-statistic: 141.8
vif(model.n2) # YearBuilt 
AIC(model.n2) # 33919.4


model.n3 = lm(SalePrice ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                Utilities +  MSZoning  + SaleCondition + Functional + Street + GarageQual +  
                MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                CentralAir, data = train)
summary(model.n3) #Adjusted R-squared:  0.8925 , F-statistic: 141.8
vif(model.n3) # GarageFinish YearBuilt 
AIC(model.n3) # 33919.4





# less important ; Alley + MSZoning, LotShape , MasVnrType,
#unneceseray variables :Condition2 + LotConfig + HeatingQC , Heating , Condition1, SaleType, 
# variables that creates multicolliniearity:
# Neighborhood, BsmtQual, BsmtFinType1, RoofStyle , GarageQual, GarageCond 

######  MODEL FOR FULL TRAIN DATA ######
model.empty = lm(SalePrice ~ 1, data = train) #The model with an intercept ONLY.
model.full = lm(SalePrice ~ ., data = train) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))


library(MASS) 

#Forward AIC for the entire data
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)


# Step:  AIC=29401.39
# SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtQual + 
#   RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
#   Condition2 + SaleCondition + OverallCond + YearBuilt + LotArea + 
#   PoolQC + ExterQual + GarageArea + TotalBsmtSF + BldgType + 
#   Functional + BedroomAbvGr + Condition1 + PoolArea + ScreenPorch + 
#   LowQualFinSF + LandContour + Street + LandSlope + KitchenAbvGr + 
#   GarageFinish + MasVnrArea + Exterior1st + TotRmsAbvGrd + 
#   LotConfig + MSZoning + GarageCars + Fireplaces + YearRemodAdd + 
#   GarageQual + GarageCond + WoodDeckSF + BsmtFullBath + X1stFlrSF + 
#   Fence + MoSold


#Forward BIC for the entire data
forwardBIC = step(model.empty, scope, direction = "forward", k = log(1460))

# Step:  AIC=29907.7
# SalePrice ~ OverallQual + GrLivArea + Neighborhood + BsmtQual + 
#   RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
#   Condition2 + OverallCond + YearBuilt + LotArea + GarageArea + 
#   TotalBsmtSF + PoolArea + ExterQual + SaleCondition + KitchenAbvGr + 
#   X2ndFlrSF + BedroomAbvGr + PoolQC + ScreenPorch + TotRmsAbvGrd + 
#   MasVnrArea + Street + X1stFlrSF


summary(forwardAIC) # adj R^2 0.919 F~117, AIC = 29401.4
summary(forwardBIC) # adj R^2 0.911 F~192, BIC = 29907.7





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


