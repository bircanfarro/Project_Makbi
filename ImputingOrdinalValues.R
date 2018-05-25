numericVars= numericVarNames = cor_numVar = cor_sorted = data = imputed = train = NA
#Resetting values each time this is rerun, making it easier to catch bugs.

# Our training data
data=read.csv("train.csv",stringsAsFactors = T)

# The function "impute"(x) is meant to assign ordinal values to those factors
# that are either binary or can be interpreted ordinally.
# Generally, the assigned values are 1 ... n, where n is the number of values that
# the given factor, "x", can take on. 
# Where this range can reasonably be interpreted as "worst to best",
# lower numbers are assigned to "worse" levels.
# NAs are not touched.
impute=function(x)
{
  categories = list(
  street = c("Grvl","Pave"),
  lotshape = c("Reg","IR1","IR2","Ir3"),
  utilities = c("ELO","NoSeWa","NoSewr","All pub"),
  landslope= c("Sev","Mod","Gtl"),
  basementexposure = c("No","Mn","Av","Gd"),
  quality = c("Po","Fa","TA","Gd","Ex"),
  basementfinish = c("Unf","LwQ","Rec","BLQ","ALQ","GLQ"),
  functionality = c("Sal","Sev","Maj2","Maj1","Mod","Min2","Min1","Typ"),
  garagefinish = c("Unf","RFn","Fin"),
  driveway = c("N","P","Y")
  )
  
  if(is.factor(x))
     for (c in categories)
        if (all(levels(x) %in% c) )
          return(match(x,c))
  return(x) # if nothing was imputed
  
}

library(corrplot)

# correlation matrix
# keep numerical columns
imputed = sapply(data,impute)
train=data.frame(imputed)
numericVars <- which(sapply(train, is.numeric)) #index vector numeric variables
numericVarNames <- names(numericVars) #saving names vector for use later

# correlation matrix
train_numVar <- train[, numericVars]
cor_numVar <- cor(train_numVar, use="pairwise.complete.obs") #correlations of all numeric variables

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))

#select only high correlations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.6)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]

corrplot(cor_numVar, tl.col="black", tl.pos = "lt")
corrplot(cor_numVar, method = "ellipse")
#corrplot(cor_numVar, method = "color")