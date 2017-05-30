# Loading Packages and Datasets
library(earth)
library(caret)
data(etitanic)
data(mdrr)


# Creating Dummy Variables
head(etitanic)
dummies1 <- model.matrix(survived ~ ., data=etitanic) # Make 1.
head(dummies1)
class(dummies1)
str(dummies1)

dummies2 <- dummyVars(survived ~ ., data=etitanic) # Make 2. # dummyVars{caret}
head(predict(dummies2, newdata = etitanic))
class(dummies2)
class(head(predict(dummies2, newdata = etitanic)))
str(dummies2)


# Zero- and Near Zero-Variance Predictors
nzv <- nearZeroVar(mdrrDescr, saveMetrics = TRUE) # nearZeroVar{caret}
nzv[nzv$nzv,]
'
freqRatio: frequency of most measured values / the frequency of the second most measured value
percentUnique: ???
zeroVar: var == 0 
nzv: near_zero_var
' 
dim(mdrrDescr)

(nzv2 <- nearZeroVar(mdrrDescr))
colnames(mdrrDescr[,nzv2]) # nzv[nzv$nzv,]
filteredDescr <- mdrrDescr[, -nzv2] # filtering zero or nzv observations
dim(filteredDescr)


# Identifying Correlated Predictors
descrCor <-  cor(filteredDescr)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999) # the number of high correlated variables
summary(descrCor[upper.tri(descrCor)])
summary(abs(descrCor[upper.tri(descrCor)]))

highlyCorDescr <- findCorrelation(descrCor, cutoff = .75) # find upper 0.75 correlated variable # findCorrelation {caret}
descrCor2 <- cor(filteredDescr[,-highlyCorDescr])
summary(descrCor2[upper.tri(descrCor2)])


# Linear Dependencies

(ltfrDesign <- matrix(0, nrow=6, ncol=6))
ltfrDesign[,1] <- c(1, 1, 1, 1, 1, 1)
ltfrDesign[,2] <- c(1, 1, 1, 0, 0, 0)
ltfrDesign[,3] <- c(0, 0, 0, 1, 1, 1)
ltfrDesign[,4] <- c(1, 0, 0, 1, 0, 0)
ltfrDesign[,5] <- c(0, 1, 0, 0, 1, 0)
ltfrDesign[,6] <- c(0, 0, 1, 0, 0, 1)

(comboInfo <- findLinearCombos(ltfrDesign))
ltfrDesign[, -comboInfo$remove]








