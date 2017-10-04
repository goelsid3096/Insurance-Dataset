#setting up working directory
setwd("G:/")

#various libraries
library(readxl)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(ROSE)
library(randomForest)

#reading of dataset
dataset = read_excel("Dataset.xlsx")

dataset$Response = as.factor(dataset$Response)

#changing column names(without space)

colnames(dataset)[12] = "MaritalStatus"
colnames(dataset)[11] = "LocationCode"
colnames(dataset)[18] = "PolicyType"
colnames(dataset)[22] = "TotalClaimAmount"
colnames(dataset)[23] = "VehicleClass"
colnames(dataset)[24] = "VehicleSize"
colnames(dataset)[3] = "CustomerLifetimeValue"
colnames(dataset)[7] = "EffectiveToDate"
colnames(dataset)[13] = "MonthlyPremiumAuto"
colnames(dataset)[14] = "MonthsSinceLastClaim"
colnames(dataset)[15] = "MonthsSincePolicyInception"
colnames(dataset)[16] = "NumberofOpenComplaints"
colnames(dataset)[17] = "NumberofPolicies"
colnames(dataset)[18] = "RenewOfferType"
colnames(dataset)[19] = "SalesChannel"
colnames(dataset)[20] = "RenewOferType"
colnames(dataset)[21] = "SaleChannel"

#converting dataset into factors

for(i in 1:ncol(dataset)){
  dataset[,i] = as.factor(as.numeric(dataset[,i]))
}

#splitting of data

split = sample.split(dataset[,4] , SplitRatio = .75)
train = subset(dataset, split==TRUE)
test = subset(dataset, split==FALSE)

dataset = as.data.frame(dataset)

# applying CART
tree = rpart(Response ~ ., data=train, method="class")
prp(tree)
predictData = predict(tree, newdata = test, type = "class")
table(test$Response, predictData)
predictROC = predict(tree, newdata = test)                                                                                                      
pred = prediction(predictROC[,2], test$Response)
per = performance(pred, "tpr", "fpr")
plot(per)

#correlating different variables; which are important and which are not
#correlation is done when dataset is numeric

cor(dataset)


#handling imbalanced data
#oversampling
is.na(dataset)
sum(is.na(dataset))
a = ovun.sample(Response ~ ., data=train, method = "over" , p=0.5)
tree.over = rpart(Response ~ ., data=train)
pred.over = predict(tree.over, newdata = test , type = "class")
roc.curve(test$Response, pred.over[,2])
tree_over = rpart(Response ~ ., data=a$data)
pred_over = predict(tree_over, newdata = test , type = "class")
table(test$Response, pred_over)

#Applying PCA
#PCA for PCA dataset must be numeric


for(i in 1:ncol(dataset)){
  dataset[,i] = as.numeric(dataset[,i])
}


dataset[,] = as.numeric(as.factor(dataset[,]))

mydata[,] = as.numeric(as.factor(mydata[,]))


pca = prcomp(pca.train, scale. = T)

names(pca)
pca$sdev
pca$rotation
pca$center
pca$scale
pca$x  
dim(pca$x)


std.dev = pca$sdev
var = std.dev^2
var[1:10]

mydata= subset(dataset, select = -c(Response))

pca.train = mydata[1:nrow(train),]
pca.test = mydata[-(1:nrow(train)),]

train.pca = data.frame(Response = train$Response, c$x)

rpart2= rpart(Response ~ ., data = train, method = "anova")

pca.train = mydata[1:nrow(mydata),]
pca.test = mydata[(1:nrow(mydata)),]


#Applying Random Forest
#For random forest Missing values must be imputes or removed
dataset = na.omit(dataset)

install.packages("randomForest")

train$Response = as.factor(train$Response)

dataset.forest = randomForest(Response ~ ., data = train )
#running on test data(final)
pred.forest = predict(dataset.forest, newdata = test )

table(test$Response, pred.forest)




