library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)
training<-read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testing<-read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!",""))
set.seed(100)
intrain<-createDataPartition(y=training$classe,p=0.7,list=FALSE)
mytrain<-training[intrain,]
mytest<-training[-intrain,]
nsv_train<-nearZeroVar(mytrain,saveMetrics=TRUE)
covariates_train<-row.names(subset(nsv_train,nzv=="FALSE"))
mytrain_clear1<-mytrain[,covariates_train]
trainingV3<-mytrain_clear1
for(i in 1:length(mytrain_clear1)) { #for every column in the training dataset
    if( sum( is.na( mytrain_clear1[, i] ) ) /nrow(mytrain_clear1) >= .6 ) { #if n?? NAs > 60% of total observations
         for(j in 1:length(trainingV3)) {
             if( length( grep(names(mytrain_clear1[i]), names(trainingV3)[j]) ) ==1)  { #if the columns are the same:
                 trainingV3 <- trainingV3[ , -j] #Remove that column
               }   
           } 
       }
}


dataneeded<-names(trainingV3[,2:58])
mytrain_clear<-trainingV3[,-1]
mytest_clear<-mytest[,names(mytrain_clear)]
testing_clear<-testing[,dataneeded]

for (i in 1:length(testing_clear) ) {
  for(j in 1:length(mytrain_clear)) {
    if( length( grep(names(mytrain_clear[i]), names(testing_clear)[j]) ) ==1)  {
      class(testing_clear[j]) <- class(mytrain_clear[i])
    }      
  }      
}


#predicting with tree
Mod1<-rpart(classe~.,method="class",data=mytrain_clear)
fancyRpartPlot(Mod1)

pred1<-predict(Mod1,newdata=mytest_clear,type="class")
confusionMatrix(pred1,mytest_clear$classe)

#predicting with random forest
Mod2<-randomForest(classe~.,data=mytrain_clear)
fancyRpartPlot(Mod2)

pred2<-predict(Mod2,newdata=mytest_clear,type="class")


Pred3<-predict(Mod2,newdata=testing_clear,type="class")
