insurance=read.csv(file.choose(),header=T, stringsAsFactors=TRUE)

#Explorratory Data Analysis

head(insurance)
dim(insurance) #1338 observations & 7 variables
str(insurance)

summary(insurance) #summary statistics of the data

attach(insurance)
names(insurance)

#used only age bmi & children for scatterplot since these are the only variables  that are quantifiable and to avoid overwhelming visual graphs
pairs(insurance[c(1,3,4,7)]) # you can see significant relationship between age&charges and bmi&charges

par(mfrow=c(1,3)) # since sex smoker & region are categorical I decided to use boxplot for the remaining 3 variables
plot(sex,charges, xlab="Sex",ylab ="Charges") #higher upper quartile & higher upper whisker for males
plot(smoker,charges, xlab="Smoker",ylab ="Charges") # higher charges for smoker
plot(region,charges, xlab="Region",ylab ="Charges")

#after reviewing graphs, our initial assessment is that age bmi and smoker variables have more impact on charges than children sex and region.


#Linear Regression

contrasts(sex)
contrasts(smoker)
contrasts(region)

#first model is using all variables
lm.insurance = lm(charges~age+sex+bmi+children+smoker+region, data=insurance)
summary(lm.insurance) #sex & region may be insignificant

#second mode is using only 3 variables we identified in data exploration
lm.insurance2=lm(charges~age+bmi+smoker, data=insurance) 
summary(lm.insurance2) # Adjusted R-squared:  0.7469 

#third model is using the same 3 variables in model 2 with 1 interaction
lm.insurance3=lm(charges~age+bmi*smoker, data=insurance) 
summary(lm.insurance3) #bmi & smoker has interaction effect Adjusted R-squared:  0.8358 

#fourth model is using the same 3 variables in model 2 with 1 interaction
lm.insurance4=lm(charges~bmi+age*smoker, data=insurance) 
summary(lm.insurance4) #age & smoker has no interaction effect Adjusted R-squared:  0.7472

#best model so far is model 3 and 2

#Hold-out method

set.seed(1)
train=sample(nrow(insurance),nrow(insurance)*0.8)
insurance.train=insurance[train, ] #training data set
insurance.test=insurance[-train, ] #test data set
response.test=charges[-train]


model1.insurance = lm(charges~age+sex+bmi+children+smoker+region, data=insurance.train)
model2.insurance = lm(charges~age+bmi+smoker, data=insurance.train)
model3.insurance = lm(charges~age+bmi*smoker, data=insurance.train)
model4.insurance = lm(charges~bmi+age*smoker, data=insurance.train)

AIC(model1.insurance) #21649.65
BIC(model1.insurance) #21699.4
summary(model1.insurance) #Adjusted R-squared:  0.7479


AIC(model2.insurance) #21650.84
BIC(model2.insurance) #21675.72
summary(model2.insurance) #Adjusted R-squared:  0.7464 

AIC(model3.insurance) #21177.29
BIC(model3.insurance) #21207.14
summary(model3.insurance) #Adjusted R-squared:  0.8373

AIC(model4.insurance) #21647.09 lower AIC is better
BIC(model4.insurance) #21676.94 lower BIC is better
summary(model4.insurance) #Adjusted R-squared:  0.7475

#Best model with lowest AIC BIC and highest adjusted R-square is model 3


#cross-validation

set.seed(1)
k=5

Model1CVMSE=rep(0,k)
Model2CVMSE=rep(0,k)
Model3CVMSE=rep(0,k)
Model4CVMSE=rep(0,k)

folds=sample(1:k,nrow(insurance),replace=TRUE)

for(j in 1:k)
{
  Model1CV=lm(charges~age+sex+bmi+children+smoker+region,data=insurance[folds!=j,])
  Model1CVMSE [j]=mean((charges-predict(Model1CV,insurance))[folds==j]^2)
}
for(j in 1:k)
{
  Model2CV=lm(charges~age+bmi+smoker,data=insurance[folds!=j,])
  Model2CVMSE [j]=mean((charges-predict(Model2CV,insurance))[folds==j]^2)
}
for(j in 1:k)
{
  Model3CV=lm(charges~age+bmi*smoker,data=insurance[folds!=j,])
  Model3CVMSE [j]=mean((charges-predict(Model3CV,insurance))[folds==j]^2)
}
for(j in 1:k)
{
  Model4CV=lm(charges~bmi+age*smoker,data=insurance[folds!=j,])
  Model4CVMSE [j]=mean((charges-predict(Model4CV,insurance))[folds==j]^2)
}

Model1MSE=mean(Model1CVMSE)
Model2MSE=mean(Model2CVMSE)
Model3MSE=mean(Model3CVMSE)
Model4MSE=mean(Model4CVMSE)

Model1RMSE=sqrt(mean(Model1CVMSE))
Model2RMSE=sqrt(mean(Model2CVMSE))
Model3RMSE=sqrt(mean(Model3CVMSE))
Model4RMSE=sqrt(mean(Model4CVMSE))

#RMSE is better in terms of reflecting performance when dealing with large error values

#lowest MSE is model 3


# Regression Tree

library(tree)

set.seed(1)
train = sample(1:nrow(insurance), nrow(insurance)/2)
tree.insurance=tree(charges~.,insurance,subset=train)

summary(tree.insurance)

#Variables actually used in tree construction:
#"smoker" "age"    "bmi"
#Number of terminal nodes:  6

cv.insurance=cv.tree(tree.insurance,K=10)
cv.insurance

prune.insurance=prune.tree(tree.insurance,best=6)
par(mfrow=c(1,1))
plot(prune.insurance)
text(prune.insurance,pretty=0)

insurance.test=insurance[-train, "charges"]
tree.pred=predict(prune.insurance,newdata=insurance[-train,])
plot(tree.pred,insurance.test) #optional plot
sqrt(mean((tree.pred-insurance.test)^2)) # 5265.331

#Bagging (variation of Random Forest that uses all predictors)

library(randomForest)

set.seed(1) 
bag.insurance=randomForest(charges~.,data=insurance,subset=train,mtry=6,importance=TRUE) 
bag.insurance #Mean of squared residuals: 20061469    % Var explained: 85.74


yhat.bag = predict(bag.insurance,newdata=insurance[-train,])
plot(yhat.bag,insurance.test) #optional plot
sqrt(mean((yhat.bag-insurance.test)^2)) #5122.742

# Random Forest

library(randomForest)

set.seed(1) 
rf.insurance=randomForest(charges~.,data=insurance,subset=train,mtry=3,importance=TRUE) #take note that lowest MSE can be obtained with using only 3 variables
yhat.rf = predict(rf.insurance,newdata=insurance[-train,])
plot(yhat.rf,insurance.test) #optional plot
sqrt(mean((yhat.rf-insurance.test)^2)) #4983.785

importance(rf.insurance) # each variable is ranked by importance
varImpPlot(rf.insurance) # smoker highest importance variable & sex as lowest importance variable

#lowest RMSE is linear regression model 3 with rmse of 4932.851 and runner up is random forest rmse of 4983.785


