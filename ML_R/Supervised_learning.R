rm(list = ls())
# Import packages
library(ISLR)
library(rpart)
library(rpart.plot)
library(e1071)
library(tidyverse)
library(ada)
library(doParallel)
library(parallel)
library(randomForest)


### Import dataset
set.seed(1) # To fix the same random seed
data = OJ
# CH = citrus hill et mm = minute maid
# We select a training sample (2/3) and a test sample (1/3)
train = sample(nrow(data),nrow(data)/3*2)
data.train = data[train,]
data.test = data[-train,]

### Simple trees

tree.tune = tune.rpart(Purchase~. ,data=data.train,
                      cp=seq(0.001,0.1,0.001),
                      parms=list(split="information"))
tree.tune$best.performance
tree.tune$best.parameters
best.tree = rpart(Purchase~. ,data=data.train,
                 control=rpart.control(cp=tree.tune$best.parameters),
                 parms=list(split="information"))

prp(best.tree,type=0,extra=0,split.box.col="lightblue",cex=0.6)

# Random Forest
nvar = ncol(OJ)-1
bag = randomForest(Purchase~.,data=data.train,method="class",
                  parms=list(split="gini"),mtry=nvar)

rfvar = round(sqrt(ncol(OJ)),0)
rf = randomForest(Purchase~.,data=data.train,method="class",
                  parms=list(split="gini"), mtry = rfvar)

head(bag$oob.times)
mean(bag$oob.times)

mean(bag$oob.times)/bag$ntree
head(bag$votes)

head(bag$err.rate)#erreurs OOB

#représentation de l'erreur OOB
plot(bag$err.rate[,1],type="l",ylim=c(0.15,0.3),
     xlab="nombre d'itérations",
     ylab="erreur",col="red")
lines(rf$err.rate[,1],col="blue")
legend("topright", legend = c("Bagging", "Forêt aléatoire"), 
     col = c("red","blue"), pch = 15,bty = "n", pt.cex = 1, 
     cex = 0.8, horiz = FALSE, inset = c(0.1, 0.1))



rfp1 = randomForest(Purchase~.,data=data.train,method="class",
                   parms=list(split="gini"),mtry=1)
rfp4 = randomForest(Purchase~.,data=data.train,method="class",
                   parms=list(split="gini"),mtry=4)
rfp8 = randomForest(Purchase~.,data=data.train,method="class",
                   parms=list(split="gini"),mtry=8)
plot(rfp1$err.rate[,1],type="l",ylim=c(0.15,0.35),xlab="nombre d'itérations",
     ylab="erreur",col="purple")
lines(rfp4$err.rate[,1],col="red")
lines(rfp8$err.rate[,1],col="green")
lines(bag$err.rate[,1],col="blue")
legend("topright", legend = c("Bagging", "RF : p=8", "RF : p=4", 
                              "RF : p=1"), 
       col = c("blue","green","red","purple"), 
       pch = 15,bty = "n", pt.cex = 1, cex = 0.8, horiz = FALSE, 
       inset = c(0.1, 0.1))

detectCores(logical=FALSE)
nc = detectCores(logical=TRUE) - 1 # Il faut toujours laisser un coeur de disponible
# enregistrement d'un cluster avec le package doParallel
registerDoParallel(cores=nc)
# lancement de la commande à l'identique
rf.tune = tune.randomForest(Purchase~.,data=data.train,
                                             mtry=3:7, nodesize = 14:16)
#fermeture du cluster
stopImplicitCluster()

rf.tune$best.parameters
head(rf.tune$performances)

best.rf = randomForest(Purchase~.,data=data.train,method="class",
                      mtry=rf.tune$best.parameters$mtry,nodesize=rf.tune$best.parameters$nodesize,
                      parms=list(split="gini"),keep.forest=TRUE,
                      importance=TRUE)
importance(best.rf)
varImpPlot(best.rf,main="Random Forest",cex=0.8)

### Erreurs en test
pred.tree = predict(best.tree, newdata=data.test[,-1], type="class")
table(pred.tree,data.test[,1])
mean(pred.tree != data.test[,1])


#Prédiction et erreur en test de la meilleure forêt aléatoire
pred.rf = predict(best.rf, data.test[,-1], type="class")
table(pred.rf, data.test[,1])
mean(pred.rf != data.test[,1])

#Prédiction bagging
pred.bag = predict(bag, data.test[,-1], type="class")
table(pred.bag, data.test[,1])
mean(pred.bag != data.test[,1])

