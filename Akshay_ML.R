library(dplyr)
library(corrplot)

# Ames housing data set, Machine lerning project
# Data cleaning 
data = read.csv("train.csv", header=TRUE, sep = ',')
train = read.csv("train.csv", stringsAsFactors = F)
train = filter(train, GrLivArea<4000)
test = read.csv("test.csv", stringsAsFactors = F)
#data

#Basic numerical EDA for cars dataset.
summary(train) #Five number summaries.
#sapply(train, sd) #Standard deviations.
str(train)


##################################
#####Visualizing Missing Data#####
##################################
library(VIM) #For the visualization and imputation of missing values.
VIM::aggr(data) #A graphical interpretation of the missing values and their
#combinations within the dataset.
# right graph, red color represent the missing values of the data and right side 
# represents how much portion is missings

######################################################################################
# task-1,missing values in each columns
myNA=is.na(data)
dim(myNA)
x=colSums(myNA) # without normalised
x=colSums(myNA)/dim(myNA)[1] # devided with dimention to normalised 
x
barplot(x)
# Here, we can see that Alley, PoolQc, Fence, MiscFeature have more then 80% missing values
# I think this columns should be dropped.
# another highly missing values are in FireplaceQu(47%),LotFrontage(17%) also we need to drop this column
# we need to discuss what to do with variables with around 5% missing data ? should we drop ?
# if those varibles doesnt make sence to contribute for sale price or its already get corelated with other not missing variables
# then we can drop it.
# we can impute missing data which has less then 20% missing values


# drop columns
data1 = subset(data, select = -c(Alley, PoolQC, Fence, MiscFeature))
# looking individual column and counts
library(dplyr)
df1 = data  %>% filter(is.na(LotFrontage)) %>% group_by(LotConfig) %>%
  summarise(n=n()) %>% mutate(freq=n/sum(n))
df1

######################################################################################
# correlation matrix
# keep numerical columns
 numericVars = which(sapply(train, is.numeric)) #index vector numeric variables
numericVars_rest = which(sapply(test, is.numeric)) #index vector numeric variables

 numericVarNames = names(numericVars) #saving names vector for use later
 
 # numerical columns only
 train_numVar = train[, numericVars]
 test_numVar = test[, numericVars_rest]
 
 # correlation matrix
 cor_numVar = cor(train_numVar, use="pairwise.complete.obs") #correlations of all numeric variables
 
 #sort on decreasing correlations with SalePrice
 cor_sorted = as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
 
 #select only high correlations
 CorHigh = names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
 cor_numVar = cor_numVar[CorHigh, CorHigh]
 
 corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
 #corrplot(cor_numVar, method = "ellipse")
 #corrplot(cor_numVar, method = "color")
 
 # looking number of counts for unique values in the column
 train$Condition1
 test1 = as.factor(train$OverallQual)
 table(test1)
 
 ####################################################################
 # multiliner regression
 # numerical columns and egnorings NA
View(train_numVar)
 
 #Creating a saturated model (a model with all variables included).
 model = lm(SalePrice ~ . -Id, data = train_numVar)
 summary(model) 
 plot(model) #Assessing the assumptions of the model.
 
 
 library(car) #Companion to applied regression.
 influencePlot(model)
 
 vif(model.saturated) #Assessing the variance inflation factors for the variables
 #in our model, helpful for identify multicolinearity
 
 #Added variable plots for assessing the contribution of each additional variable.
 avPlots(model.saturated) #Distinct patterns are indications of good contributions
 #to the model; absent patterns usually are pointers to
 #variables that could be dropped from the model.
 
 
 #We can use stepwise regression to help automate the variable selection process.
 #Here we define the minimal model, the full model, and the scope of the models
 #through which to search:
 model.empty = lm(SalePrice ~ 1, data = na.omit(train_numVar)) #The model with an intercept ONLY.
 model.full = lm(SalePrice ~ ., data = na.omit(train_numVar)) #The model with ALL variables.
 scope = list(lower = formula(model.empty), upper = formula(model.full))
 
 
 
 library(MASS) #The Modern Applied Statistics library.
 
 #Stepwise regression using AIC as the criteria (the penalty k = 2).
 forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
 backwardAIC = step(model.full, scope, direction = "backward", k = 2)
 bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
 bothAIC.full = step(model.full, scope, direction = "both", k = 2)
 
 #Stepwise regression using BIC as the criteria (the penalty k = log(1460))
 forwardBIC = step(model.empty, scope, direction = "forward", k = log(1460))
 backwardBIC = step(model.full, scope, direction = "backward", k = log(1460))
 bothBIC.empty = step(model.empty, scope, direction = "both", k = log(1460))
 bothBIC.full = step(model.full, scope, direction = "both", k = log(1460))
 

 #Checking the model summary and assumptions of the reduced model.
 summary(forwardAIC)
 plot(forwardAIC)
 influencePlot(forwardAIC)
 vif(forwardAIC)
 avPlots(forwardAIC)
 confint(forwardAIC)
 
 #Predicting new observations.

 newdata = test_numVar
 View(newdata)
 
 
 predict(forwardAIC, newdata, interval = "confidence") #Construct confidence intervals
 #for the average value of an
 #outcome at a specific point.
 
 predict(forwardAIC, newdata, interval = "prediction") #Construct prediction invervals
 #for a single observation's
 #outcome value at a specific point.
 
 
 
 
