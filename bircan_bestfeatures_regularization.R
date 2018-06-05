    
    ###################################################
    ########## NUMERIC FEATURES SELECTION  ############
    ###################################################
    
    # NOTE : All the model's Y column has changed to log.SalePrice, therefore the previous commented values are no longer valid.
    # if you want to see the results please run the  #train = train %>% mutate(log.SalePrice = log(SalePrice)) code.
    
    library(dplyr)
    library(car)
    library(psych)
    train = read.csv('./data/train_superclean.csv') 
    train = train %>% mutate(log.SalePrice = log(SalePrice))
    test = read.csv('./data/test_superclean.csv')
    
   
    
    numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
    numericVarNames <- names(numericVars) #saving names vector for use later
    train_numVar <- train[, numericVars]
    data1 = na.fail(train_numVar) #Error in na.fail.default(train_numVar) : missing values in object
    data2 = na.omit(train_numVar) #1121 obs
    data3 = na.exclude(train_numVar) 
     
    
    
    # creating full linear model and summarise to see the significance of each variables
    model.1 = lm(log.SalePrice  ~ ., data = data1)
    summary(model.1)
    vif(model.1)
    alias(model.1)
    
    
    
    # Model formed with the only variables that has the highest significany
    model.2 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + X1stFlrSF + X2ndFlrSF + BsmtFullBath +
                   BedroomAbvGr + KitchenAbvGr + TotRmsAbvGrd + Fireplaces +  GarageCars +
                   WoodDeckSF + ScreenPorch + PoolArea, data = data1)
    vif(model.2) #creates some colliniearity
    AIC(model.2) #26768.64 ,28531.87
    summary(model.2)
    
    
    # extract the variables that creates colliniearity
    model.3 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
                   BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + 
                   WoodDeckSF + ScreenPorch + PoolArea, data = data1)
    vif(model.3) #no colliniearity
    AIC(model.3) #26987.86 - AIC got higher, model.2 is better, 28784.8
    
    
    
    # adding X1stFlrSF that created multiclliniearity in model.2
    model.4 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + 
                   BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
                   WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = data1)
    vif(model.4) # no colliniearity
    AIC(model.4) #26966.64 - AIC is still higher than model.2 , 28755.32
    
    
    
    # adding X1stFlrSF, X2ndFlrSF that created multiclliniearity in model.2
    model.5 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                   BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
                   WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF, data = data1)
    vif(model.5) # no colliniearity
    AIC(model.5) #26780.55 - AIC is much better than model 3 little higher than model 2 but no colliniearity
    
    
    
    # adding TotRmsAbvGrd that created multiclliniearity in model.2
    model.6 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath +
                   BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars + TotRmsAbvGrd +
                   WoodDeckSF + ScreenPorch + PoolArea, data = data1)
    vif(model.6) #no colliniearity
    AIC(model.6) #26872.29 - model 5 is the best so far
    
    
    
    #The model was found based on AIC forward method
    model.7 = lm(log.SalePrice  ~ OverallQual + GrLivArea + BsmtFinSF1 + TotalBsmtSF + 
                   YearRemodAdd + LotArea + GarageArea + KitchenAbvGr + MasVnrArea + 
                   BedroomAbvGr + TotRmsAbvGrd + MSSubClass + YearBuilt + OverallCond + 
                   GarageCars + OpenPorchSF + ScreenPorch + BsmtHalfBath, data = data1)
    vif(model.7) #GrLivArea, GarageCars, TotRmsAbvGrd  are over 4, 
    AIC(model.7) #26789.89
    
    #difference 7 from 5 GrLivArea, TotalBsmtSF, YearRemodAdd, GarageArea, TotRmsAbvGrd, OpenPorchSF, BsmtHalfBath
    #diffrence 5 from 7  LotFrontage, BsmtFullBath, X2ndFlrSF, X1stFlrSF, PoolArea, WoodDeckSF
    
    # mutual features between model 5 and 7
    model.7a = lm(log.SalePrice  ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea +
                    BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + ScreenPorch, data = data1)
    
    vif(model.7a)
    AIC(model.7a) #27028.79
    
    
    # adding some features from the difference (YearRemodAdd, BsmtFullBath, PoolArea, X2ndFlrSF, X1stFlrSF, 
    #OverallCond, WoodDeckSF  additionally Fireplaces
    model.7b = lm(log.SalePrice  ~ OverallQual + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                    BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                    PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                    OverallCond + WoodDeckSF + Fireplaces, data = data1)
    
    vif(model.7b) # no colliniearity
    AIC(model.7b) #26781.16
    
    
    
    
    # decided that the model includes some categorical variables like OverallQual, OverallCond. so now extracting those
    model.8 = lm(log.SalePrice  ~ BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                    BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                    PoolArea + YearRemodAdd + ScreenPorch + X2ndFlrSF + X1stFlrSF + 
                    WoodDeckSF + Fireplaces, data = data1)
    vif(model.8)
    AIC(model.8) #26980.15 - AIC has increased as expected, but still pretty good
    
    #this model matches with the model was discussed with marius
    
    #Changing GrLivArea with X2ndFlrSF and X1stFlrSF and adding TotRmsAbvGrd 
    model.8a = lm(log.SalePrice  ~ GrLivArea + BsmtFinSF1 + LotArea + KitchenAbvGr + MasVnrArea + 
                   BedroomAbvGr + MSSubClass + YearBuilt + GarageCars + BsmtFullBath + 
                   PoolArea + YearRemodAdd + ScreenPorch + 
                   WoodDeckSF + Fireplaces, data = data1)
    vif(model.8a)
    AIC(model.8a) #26978.12 the result is so close with the one above
    
     ---------------------------------------------------------------------------------------------------------------
    #marius & bircan
      
    # the model was decided the best with marius on Friday seems matched with my updated seperate features.
    model.11 = lm(log.SalePrice  ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                   YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                   MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
                   ScreenPorch + BsmtFullBath + Fireplaces + PoolArea, data = train)
    vif(model.11)
    AIC(model.11) #26980.15
    summary(model.11) #Adjusted R-squared:  0.7631 
    
    
    
    # adding BsmtUnfSF features
    model.12 = lm(log.SalePrice  ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                    YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                    MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                    ScreenPorch + BsmtFullBath + Fireplaces + PoolArea,  data = data1)
    
    vif(model.12) # multicolliniearity X1stFlrSF, BsmtFinSF1
    AIC(model.12) #26973.98 - better AIC
    summary(model.12) #Adjusted R-squared:  0.7646
    
    
    
    #extracting BsmtUnfSF and adding Full Bath
    model.13 = lm(log.SalePrice  ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                    YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                    MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr +
                    ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = data1)
    vif(model.13) # no colliniearity
    AIC(model.13) #26974.69
    summary(model.13) #Adjusted R-squared:  0.7644
    
    
    
    #adding BsmtUnfSF again
    model.14 = lm(log.SalePrice  ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF +
                  YearRemodAdd + LotArea + GarageCars + MasVnrArea + KitchenAbvGr +
                  MSSubClass + YearBuilt + X1stFlrSF + BedroomAbvGr + BsmtUnfSF +
                  ScreenPorch + BsmtFullBath + Fireplaces + PoolArea + FullBath,  data = data1)
    vif(model.14) # multicolliniearity X1stFlrSF, BsmtFinSF1
    AIC(model.14) #26968.63
    summary(model.14) #Adjusted R-squared:  0.7659
    
    
    
    #switching  BsmtUnfSF  to BsmtFinSF2
    model.15 = lm(log.SalePrice  ~  X2ndFlrSF +  BsmtFinSF1 + WoodDeckSF + BsmtFinSF2 +
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
    #######  CATEGORICAL FEATURES SELECTION  ##########
    ###################################################   



    categoricalVars <- which(sapply(train, is.factor))
    CategoricalVarNames <- names(categoricalVars)
    train_catVar <- train[, categoricalVars]
    train_catVar = cbind(train_catVar, log.SalePrice  = train$log.SalePrice )
    

    
    model.empty.cat = lm(log.SalePrice  ~ 1, data = train_catVar) #The model with an intercept ONLY.
    model.full.cat = lm(log.SalePrice  ~ ., data = train_catVar) #The model with ALL variables.
    scope = list(lower = formula(model.empty.cat), upper = formula(model.full.cat))
    
    
    #Forward AIC on categorical variables
    forwardAICat = step(model.empty, scope, direction = "forward", k = 2)
    
    # Step:  AIC=30429.52
    # log.SalePrice  ~ Neighborhood + KitchenQual + BsmtQual + FireplaceQu + 
    #   BldgType + BsmtExposure + HouseStyle + RoofMatl + ExterQual + 
    #   PoolQC + GarageFinish + Exterior1st + MasVnrType + Condition2 + 
    #   BsmtFinType1 + LotConfig + SaleCondition + RoofStyle + HeatingQC + 
    #   LotShape + CentralAir + Heating + GarageQual + GarageCond + 
    #   Condition1 + MSZoning + Alley + SaleType + Utilities
    
    
    
    #Forward BIC on categorical variables
    forwardBICat = step(model.empty, scope, direction = "forward", k = log(1460))
    
    # Step:  AIC=30969.78
    # log.SalePrice  ~ Neighborhood + KitchenQual + BsmtQual + FireplaceQu + 
    #   BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
    #   HouseStyle + GarageFinish + CentralAir
    
    summary(forwardAICat)
    summary(forwardBICat)
    vif(forwardAICat)
    alias(forwardAICat)
    vif(forwardBICat)
    
    summary(train_catVar)
    
    
    # less important ; Alley + MSZoning, LotShape , MasVnrType,
    # unneceseray variables :Condition2 + LotConfig + HeatingQC , Heating , Condition1, SaleType, 
    # variables that creates multicolliniearity:
    # Neighborhood, BsmtQual, BsmtFinType1, RoofStyle , GarageQual, GarageCond 
    
    # baseline is BIC >> adding features one by one from AIC model to find the best predictors
    model.0 = lm(log.SalePrice  ~  KitchenQual +  FireplaceQu + 
                   BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.0) # Adjusted R-squared:  0.7234, F-statistic: 96.42, p-value: < 2.2e-16
    vif(model.0) # ExterQual,KitchenQual
    AIC(model.0) # 35254.86
    
    
    # +  SaleCondition
    model.1 = lm(log.SalePrice  ~  KitchenQual +  FireplaceQu + SaleCondition +
                   BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.1) # Adjusted R-squared:  0.7289, F-statistic: 88.17, p-value: < 2.2e-16
    vif(model.1) # salecondition is significant but reduces F ??
    AIC(model.1) # 35230.71
    
    
    # +  Utilities
    model.2 = lm(log.SalePrice  ~  KitchenQual +  FireplaceQu +  Utilities + 
                   BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.2) # Adjusted R-squared:  0.7233 ,F-statistic: 94.04,  p-value: < 2.2e-16
    vif(model.2) 
    AIC(model.2) # 35256.4
    
    
    
    model.3 = lm(log.SalePrice  ~  KitchenQual +  FireplaceQu + MSZoning + 
                   LotShape + MasVnrType + BldgType + BsmtExposure + ExterQual + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.3) #Adjusted R-squared: 0.7408 , F-statistic: 84.4
    vif(model.3)
    AIC(model.3) # 35169.91
    
    
    
    #based on p values
    model.4 = lm(log.SalePrice  ~  MSZoning + LotConfig + BldgType + HouseStyle + RoofMatl + 
                   ExterQual  + BsmtExposure + KitchenQual +   
                   GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.4) #Adjusted R-squared:  0.6977 F-statistic:  85.2
    vif(model.4)
    AIC(model.4) # 35384.61
    
    
    
    model.5 = lm(log.SalePrice  ~  KitchenQual +  FireplaceQu + MSZoning + LotConfig + ExterQual +
                   LotShape + MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.5) #Adjusted R-squared:  0.7432  F-statistic:  71.67
    vif(model.5) # KitchenQual 
    AIC(model.5)  # 35160.44
    plot(model.5) 
    influencePlot(model.5) 
    
   
    model.6 = lm(log.SalePrice  ~  ExterQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
                   MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC + 
                   HouseStyle + GarageFinish + CentralAir , data = train_catVar)
    
    summary(model.6) #Adjusted R-squared:  0.7119 F-statistic:  73.1 
    vif(model.6) # best
    AIC(model.6)  # 35324.3
    plot(model.6)
    
    
    ##########################################
    #######  FULL MODEL SELECTION  ##########
    #########################################
    
    
    model.n = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                   YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                   BedroomAbvGr + KitchenAbvGr + Fireplaces +  GarageCars +
                   WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF +
                   ExterQual +  FireplaceQu +  Utilities + Alley + MSZoning + 
                   LotShape + MasVnrType + BldgType + BsmtExposure + RoofMatl + PoolQC +
                   HouseStyle + GarageFinish + CentralAir , data = train)
    summary(model.n) #Adjusted R-squared:  0.8981 , F-statistic: 147.3
    vif(model.n) # PoolQC , BldgType, PoolArea,MSSubClass, X2ndFlrSF , Fireplaces , FireplaceQu, RoofMatl 
    AIC(model.n) #34156.28
    
   
    
    # Step:  AIC=29401.39
    # log.SalePrice  ~ OverallQual  
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
    
    model.n1 = lm(log.SalePrice  ~ OverallQual + MSSubClass + LotArea + LotFrontage + OverallCond +
                    YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                    BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Exterior1st +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual +
                    Utilities +  MSZoning  + SaleCondition + Functional + TotRmsAbvGrd +
                    LotShape + MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                    GarageFinish + CentralAir , data = train)
    summary(model.n1) #Adjusted R-squared:  0.9066 , F-statistic: 128.2
    vif(model.n1) # TotRmsAbvGrd Exterior1st YearBuilt 
    AIC(model.n1) # 34094.42
    
    plot(model.n1)
    
    # Step:  BIC=29907.7
    # log.SalePrice  ~ OverallQual + GrLivArea + Neighborhood + BsmtQual + 
    #   RoofMatl + BsmtFinSF1 + MSSubClass + BsmtExposure + KitchenQual + 
    #   Condition2 + OverallCond + YearBuilt + LotArea + GarageArea + 
    #   TotalBsmtSF + PoolArea + ExterQual + SaleCondition + KitchenAbvGr + 
    #   X2ndFlrSF + BedroomAbvGr + PoolQC + ScreenPorch + TotRmsAbvGrd + 
    #   MasVnrArea + Street + X1stFlrSF
    
    model.n2 = lm(log.SalePrice  ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                     MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + 
                    BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                    Utilities +  MSZoning  + SaleCondition + Functional + Street + GarageQual +  
                    MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                    CentralAir , data = train)
    summary(model.n2) #Adjusted R-squared:  0.9098 , F-statistic: 141.8
    vif(model.n2) # YearBuilt 
    AIC(model.n2) # 33919.4
    
    
 
    
    model.n3 = lm(log.SalePrice  ~ OverallQual  + LotArea + LotFrontage + OverallCond +
                    MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + FullBath +
                    BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + 
                    MSZoning  + SaleCondition + Functional + GarageQual +  LotConfig + 
                    MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                    CentralAir, data = train)
    summary(model.n3) #Adjusted R-squared:  0.9084 , F-statistic: 156.5
    vif(model.n3) # GarageFinish YearBuilt 
    AIC(model.n3) # 33937.89
    
    
    
    model.n3a = lm(log.SalePrice  ~ OverallQual  + LotArea + LotFrontage + OverallCond + MasVnrType + Condition2 +
                    YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + RoofMatl + BsmtExposure  + FullBath +
                    BedroomAbvGr + KitchenAbvGr + GarageCars + KitchenQual+ MSZoning + GarageFinish + SaleCondition +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + Fence +  Utilities + FireplaceQu + CentralAir + MoSold,
                    data = train)
    
    summary(model.n3a) #Adjusted R-squared:  0.9082 , F-statistic: 141.8
    vif(model.n3a) # GarageFinish YearBuilt 
    AIC(model.n3a) # 34115
    
    
    
    
    
    model.n4 = lm(log.SalePrice  ~ OverallQual + LotArea + LotFrontage + OverallCond +
                    YearBuilt + MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF +
                    BedroomAbvGr + KitchenAbvGr + FireplaceQu +  GarageCars + Condition2 +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                    Utilities +  MSZoning  + SaleCondition + Functional + Street + GarageQual +  
                    MasVnrType + BsmtExposure + RoofMatl + YearRemodAdd + Fence + MoSold +
                    CentralAir, data = train)
    summary(model.n4) #Adjusted R-squared:  0.9137, F-statistic: 141.8
    vif(model.n4) # GarageFinish YearBuilt 
    AIC(model.n4) # 33954.18
    plot(model.n4)
    
    library(MASS)
    
    model.nn5 = rlm(SalePrice ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                    MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + Fireplaces +
                    KitchenAbvGr + GarageCars + Condition2 + HeatingQC +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                    Utilities +  MSZoning  + SaleCondition + Functional +   
                    BsmtExposure + RoofMatl + YearRemodAdd +  
                    CentralAir, data = train)
    summary(model.nn5) #Adjusted R-squared:  0.91  , F-statistic: 135.1
    vif(model.nn5) # 
    AIC(model.nn5) # 33941.43
    par(mfrow=c(2,3))
    plot(model.nn5)
    influencePlot(model.nn5)
    
    
    # best model
    model.n5 = lm(log.SalePrice ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                    MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + Fireplaces +
                    GarageCars + Condition2 + HeatingQC +
                    WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                    Utilities +  MSZoning  + SaleCondition + Functional +   
                     BsmtExposure + RoofMatl + YearRemodAdd +  
                    CentralAir, data = train)
    summary(model.n5) #Adjusted R-squared:  0.91  , F-statistic: 135.1
    vif(model.n5) # 
    AIC(model.n5) # 33941.43
    par(mfrow=c(2,3))
    plot(model.n5)
    influencePlot(model.n5)
    anova(model.n5)
    library(jtools)
    plot_summs(model.n5, rescale.distributions = TRUE ,  scale = TRUE, plot.distributions = TRUE)
    
    effect_plot(model.n5, pred = X2ndFlrSF  , interval = TRUE, plot.points = TRUE)
    
    
    
    
    sub_model.n5 = lm(log.SalePrice ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                        MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + 
                        KitchenAbvGr + GarageCars + Condition2 + HeatingQC +
                        WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + (Condition1)^2 +
                        Utilities +  MSZoning  + (SaleCondition)^2 + Functional +   
                        BsmtExposure + RoofMatl + YearRemodAdd +  
                        CentralAir, data = train)
    summary(sub_model.n5) #Adjusted R-squared:  0.91  , F-statistic: 135.1
    vif(sub.model.n5)
    plot()
    
    
    
    sub.model.n5 = lm(log(SalePrice) ~ YearBuilt +  LotArea + MiscVal + YearRemodAdd  +
                        MasVnrArea + GarageArea + TotalBsmtSF + BsmtFinSF1 + GrLivArea +
                        ScreenPorch +  WoodDeckSF + 
                        + X2ndFlrSF, data = train)
    summary(sub.model.n5) #Adjusted R-squared:  0.91  , F-statistic: 135.1
    vif(sub.model.n5)
    
    
    #MSE
    mean(abs(predict(model.n5) - train$log(SalePrice))) #0.08010317
    
    sum((predict(model.n5) - train$log.SalePrice)^2 / length(train$log.SalePrice)) #0.01298258
  
    # less important ; Alley + MSZoning, LotShape , MasVnrType,
    #unneceseray variables :Condition2 + LotConfig + HeatingQC , Heating , Condition1, SaleType, 
    # variables that creates multicolliniearity:
    # Neighborhood, BsmtQual, BsmtFinType1, RoofStyle , GarageQual, GarageCond 
    
    

    model.empty = lm(log.SalePrice  ~ 1, data = train) #The model with an intercept ONLY.
    model.full = lm(log.SalePrice  ~ ., data = train) #The model with ALL variables.
    scope = list(lower = formula(model.empty), upper = formula(model.full))
    

    
    ###################################################
    ###### REGULARIZATION AND CROSS  VALIDATION #######
    ###################################################
    
    library(ISLR)
    library(glmnet)
    
    
    ##### Ridge Regression #####
   
     
    
    #Creating training and testing sets with 70-30 split 
    set.seed(0)
    train_train_idx = sample(1:nrow(train), 7*nrow(train)/10)
    train_train = train[train_train_idx,]
    train_test  = train[-train_train_idx,]
    
    #sample model = model.n4
    
    x = model.matrix(log.SalePrice ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                       MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + Fireplaces +
                       KitchenAbvGr + GarageCars + Condition2 + HeatingQC +
                       WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                       Utilities +  MSZoning  + SaleCondition + Functional +   
                       BsmtExposure + RoofMatl + YearRemodAdd +  
                       CentralAir, data = train_train)
    
    y = train_train$log.SalePrice
   
    y.test = train_test$log.SalePrice
    
    
    x.test = model.matrix(log.SalePrice ~ OverallQual +  LotArea + LotFrontage + OverallCond +
                            MasVnrArea + BsmtFinSF1 + BsmtFullBath + X2ndFlrSF + Fireplaces +
                            KitchenAbvGr + GarageCars + Condition2 + HeatingQC +
                            WoodDeckSF + ScreenPorch + PoolArea + X1stFlrSF + KitchenQual + Condition1 +
                            Utilities +  MSZoning  + SaleCondition + Functional +   
                            BsmtExposure + RoofMatl + YearRemodAdd +  
                            CentralAir, data = train_test)
    
    
    grid = 10^seq(1, -5, length = 100)
    
    #Running 5-fold cross validation.
    set.seed(0)
    cv.ridge = cv.glmnet(x, y, lambda = grid, alpha = 0, nfolds = 10)
    plot(cv.ridge, main = "Ridge Regression\n")
    best.lambda.ridge = cv.ridge$lambda.min
    best.lambda.ridge # 0.01
     
    
    #What is the test MSE associated with this best value of lambda?
    ridge.models.train = glmnet(x, y, alpha = 0, lambda = grid)
    ridge.best.lambda.train = predict(ridge.models.train, s = best.lambda.ridge, newx = x.test)
    sqrt(mean((ridge.best.lambda.train - y.test)^2)) #0.04010766
    mean(abs(ridge.best.lambda.train - y.test)) #0.0968727 , absolute error
    
    
    
    #or
    ridge.best.lambda.train = predict.cv.glmnet(cv.ridge, s ="lambda.min", newx = x.test)
    mean((ridge.best.lambda.train - y.test)^2) #0.04010766
    
    
    
    
    ##### Lasso Regression #####
    
    lasso_model = glmnet(x, y, alpha = 1, lambda = grid)
    plot(lasso_model, xvar = "lambda", label = TRUE, main = "Lasso Regression")
    

    #Running 5-fold cross validation.
    set.seed(0)
    cv.lasso = cv.glmnet(x, y,
                         lambda = grid, alpha = 1, nfolds = 10)
    plot(cv.lasso, main = "Lasso Regression\n")
    best.lambda.lasso = cv.lasso$lambda.min
    best.lambda.lasso #0.01
    
    
    # What is the test MSE associated with this best value of lambda?
    lasso.model.train = glmnet(x, y, alpha = 1, lambda = grid)
    lasso.best.lambda.train = predict(lasso.model.train, s = best.lambda.lasso, newx = x.test)
    summary(lasso.best.lambda.train)
    plot(lasso.best.lambda.train)
    mean((lasso.best.lambda.train - y.test)^2) # 0.03932375
    
    #or
    lasso.best.lambda.train = predict.cv.glmnet(cv.lasso, s ="lambda.min", newx = x.test)
    mean((lasso.best.lambda.train - y.test)^2) # 0.03932375
    
    
    ################## Ashkay #####
    
    lasso.model.train = glmnet(x, y, alpha = 1, lambda = 10)
    mean(abs(y.test-predict(lasso.model.train,newx = x.test)))
    
    
    y_predict=predict(lasso.model.train, newx = x.test)
    y_predict
    y.test
    
    mean((y.test-y_predict)^2)
    
    mean(abs(y.test-y_predict))
    
    a.test1=model.matrix(data=a.test)
    
    
    predict(lasso.model.train,newx = a.test)
    
    
    
    lasso.best.lambda.train = predict(lasso.model.train, s = best.lambda.lasso, newx = x.test)
    summary(lasso.best.lambda.train)
    plot(lasso.best.lambda.train)
    mean((lasso.best.lambda.train - y.test)^2)
    
    
    ###################################
    
    
    
    
    
    
    
   
    
    
    
    #Full Model Lasso Regression
    
    train1 = train[-c(80,81,82)]
  
    
    model = lm(log.SalePrice ~ .,  data = train1)
    
    summary(model)
    vif(model)
    alias(model)
    sapply(train1, class)
    
    set.seed(0)
    train_train_id = sample(1:nrow(train1), 7*nrow(train1)/10)
    train_trainn = train1[train_train_id,]
    train_testt  = train1[-train_train_id,]
    
    xx = model.matrix(log.SalePrice ~ ., data = train_trainn)
    yy = train_trainn$log.SalePrice
    yy.test = train_testt$log.SalePrice
    
    
    grid = 10^seq(5, -2, length = 100)
    
    
    x.testt = model.matrix(log.SalePrice ~ ., data=train_testt)
    
    set.seed(0)
    cv.lassoo = cv.glmnet(xx, yy,
                         lambda = grid, alpha = 1, nfolds = 10)
    plot(cv.lassoo, main = "Lasso Regression\n")
    best.lambda.lassoo = cv.lassoo$lambda.min
    best.lambda.lassoo #0.01
    
    
    lasso.model.trainn = glmnet(xx, yy, alpha = 1, lambda = grid)
    lasso.best.lambda.trainn = predict(lasso.model.trainn, s = best.lambda.lassoo, newx = x.testt)
    mean((lasso.best.lambda.trainn - yy.test)^2) # 0.04186547
    
    lasso.best.lambda.trainn = predict.cv.glmnet(cv.lassoo, s ="lambda.min", newx = x.testt)
    mean((lasso.best.lambda.trainn - yy.test)^2) # 0.04186547
    

    
    
    SalePrice = predict(model.n5, newdata = test)
    test_y = cbind(test , SalePrice)
    View(test_y)
    test_y = select(test_y, "Id", "SalePrice")
    View(test_y)
    write.csv(test_y, file = 'linear_model_kaggle.csv')
    a = read.csv('linear_model_kaggle.csv')
    View(a)
   
   