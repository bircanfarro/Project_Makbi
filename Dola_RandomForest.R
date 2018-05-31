#install.packages("randomForest")
library(randomForest)
library(MASS)
library(tree)
library(dplyr)

set.seed(0)
df_train <- read.csv("./data/train.csv", stringsAsFactors = F)
df_train <- df_train %>% mutate(sale_log=log(SalePrice))

#df_train <- df_train[(df_train$SalePrice<300000 | df_train$GrLivArea>4000),]

plot(df_train$SalePrice)
hist(df_train$SalePrice)
hist(df_train$sale_log)

#Get the fraction of missing values in train dataset
#not a high value but missing values have to be handled prior to running the randaom forest model
#Convert the following integer columns to factors. Note they were loaded as integers but are true factors ($MSSubClass, $YearBuilt, $YearRemodAdd, $GarageYrBlt, $MoSold, $YrSold) 
nodat <- sum(is.na(df_train)) / (nrow(df_train) * ncol(df_train))
nodat

#Check for missing values and get their column index
checkna <- function (dx) {
  set.seed(0)
  nacols <- c(names(which(sapply(dx, function(x) any(is.na(x))))))
  cc <- which(names(dx) %in% nacols)
  cc
}

checkna(df_train)

modeCol <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}


for (i in checkna(df_train)) {
  if (i %in% c(7, 58, 73, 74, 75)  )  {   #all(is.na(df_train[,i]))) {
      df_train[which(is.na(df_train[,i])),i] <- 'No record provide'
  } else {
    if( is.numeric(df_train[,i]) == TRUE) df_train[which(is.na(df_train[,i])),i] <- median(df_train[,i], na.rm=T)
    if( is.integer(df_train[,i]) == TRUE) df_train[which(is.na(df_train[,i])),i] <- median(df_train[,i], na.rm=T)
    if( is.character(df_train[,i]) == TRUE) df_train[which(is.na(df_train[,i])),i] <- modeCol(c(df_train[,i]))
    if( is.factor(df_train[,i]) == TRUE) df_train[which(is.na(df_train[,i])),i] <- modeCol(c(df_train[,i]))
  }
}

checkna(df_train)


charvars <- lapply(df_train, class) == "character"
intval <- lapply(df_train, class) == "integer"
df_train[, charvars] <- lapply(df_train[, charvars], as.factor)
df_train[, intval] <- lapply(df_train[, intval], as.numeric)
df_train[, c(2,77,78)] <- lapply(df_train[, c(2,77,78)], as.factor)  # $MSSubClass, $YearBuilt, $YearRemodAdd, $GarageYrBlt, $MoSold, $YrSold
str(df_train)

checkna(df_train)

rftrain = randomForest(SalePrice ~. -sale_log, data = df_train , importance = TRUE)
rftrain
#predict(rftrain,df_train)

mean(abs(df_train$SalePrice-predict(rftrain,df_train)))


########FROM LECTURE NOTES
#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(81)

for (n in 1:81) {
  fit = randomForest(sale_log ~ ., data = df_train, mtry = n)
  oob.err[n] = fit$mse[500]
  cat("We're performing iteration", n, "\n")
}

#Visualizing the OOB error.
plot(1:81, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rftrain)
varImpPlot(rftrain)
