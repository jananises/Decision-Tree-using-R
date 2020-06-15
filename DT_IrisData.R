###########################################################################################
data("iris")
library(caret)
library(C50)
library(party)

str(iris)
set.seed(1234)

#Train-test Split - 70/30
inTraininglocal <- createDataPartition(iris$Species, p=0.70, list=F)

training<-iris[inTraininglocal,]
testing<-iris[-inTraininglocal,]

#we will proceed to plot our Decision Tree using the conditional parting plot 
#Model Building
modelctree<-ctree(training$Species~., data = training) #Trails - Boosting Partition
modelctree

class(modelctree)
#Generate the model summary
summary(modelctree)

#Plot the tree
plot(modelctree)

#Predict for test data set
#pred2 <- predict(modelctree, newdata = testing[,-5], type = "prob")

testing[1:5,-5]

pred1 <- predict(modelctree, testing[,-5])
a<-table(testing$Species,pred1)
sum(diag(a))/sum(a)

#determine the node numbers of nodes some new observations are falling into
pred2 <- predict(modelctree, newdata = testing[,-5], type = "node")


### estimated class probabilities, a list
tr <- treeresponse(modelctree, newdata = testing[,-5])
tr

##################################################################################
#Bagging and Boosting Technique for Model
set.seed(1234)
acc<-c()
for(i in 1:100)
{
  print(i)
  #Data Partition
  inTraininglocal<-createDataPartition(iris$Species, p=0.70, list=F)
  
  training<-iris[inTraininglocal,]
  testing<-iris[-inTraininglocal,]
  
  #Model Building
  fittree<-ctree(training$Species~., data = training)
  #Predicting for test data set
  pred <- predict(fittree, testing[,-5])
  
  
  a<-table(testing$Species,pred)
  #Accuracy
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

summary(acc)
boxplot(acc)
