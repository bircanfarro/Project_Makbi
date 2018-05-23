library(MASS) 
library(car)
train = read.csv('train.csv') 


numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later
train_numVar <- train[, numericVars]


model.empty = lm(SalePrice ~ 1, data = na.omit(train_numVar)) #The model with an intercept ONLY.
model.full = lm(SalePrice ~ ., data = na.omit(train_numVar)) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))


#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))


#Checking the model summary and assumptions of the reduced model.
summary(forwardAIC)
summary(forwardBIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)



#Predicting new observations.
forwardAIC$fitted.values #Returns the fitted values.

newdata = data.frame(Murder = c(1.5, 7.5, 12.5),
                     HS.Grad = c(60, 50, 40),
                     Frost = c(75, 55, 175),
                     Population = c(7500, 554, 1212))

predict(forwardAIC, newdata, interval = "confidence") #Construct confidence intervals
#for the average value of an
#outcome at a specific point.

predict(forwardAIC, newdata, interval = "prediction") #Construct prediction invervals
#for a single observation's
#outcome value at a specific point.
confint(forwardAIC)