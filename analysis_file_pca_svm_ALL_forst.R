library(caret)
library(tree)
library(rpart)
library(e1071)
library(randomForest)

setwd("C:/Users/Vivek/Desktop/coursera/PML_docs")

data_train<-read.csv('pml-training.csv')
data_test<-read.csv('pml-testing.csv')

SVM_all <-list()
predictors_all <-list()
pc_Obj_all <-list()


ind_na = is.na(data_train[1,])
data_train2 <- as.data.frame(data_train[,ind_na==FALSE])
data_test2 <- as.data.frame(data_test[,ind_na==FALSE])

features<-names(data_train2)
roll_ind<-grep("_roll_",features,fixed = TRUE)
pitch_ind<-grep("_pitch_",features,fixed = TRUE)
picth_ind<-grep("_picth_",features,fixed = TRUE)
yaw_ind<-grep("_yaw_",features,fixed = TRUE)

# roll_arm, pitch_arm, yaw_arm
#roll_ind<-grep("roll_arm",features,fixed = TRUE)
#pitch_ind<-grep("pitch_arm",features,fixed = TRUE)
#yaw_aind<-grep("yaw_arm",features,fixed = TRUE)

data_train2 <- as.data.frame( data_train2[, -c( roll_ind, pitch_ind, yaw_ind, picth_ind ) ] )
data_train3 <- data_train2[,-c(1,3,4,5,6,7)]


AllData<- data_train3
ind_all_predictors<- 2:53
ind_bad_predictors<- ind_all_predictors[apply(AllData[,2:53],2,sd)==0]
if (length(ind_bad_predictors)!=0) {
  
  ind_predictors <-ind_all_predictors[-ind_bad_predictors]
  AllData<- AllData[,-ind_bad_predictors]
}
predictors<-names(AllData)
predictors<-predictors[2:(dim(AllData)[2]-1)]

ind_all <- sample(1:dim(AllData)[1])
num_model <- round(length(ind_all)*.8)
indModel <- ind_all[1:(num_model)]

data_training <- AllData[indModel,]
data_CV       <- AllData[-indModel,]

rfmod <- randomForest(classe ~ ., data = data_training, do.trace = 100)
confusionMatrix(predict(rfmod,data_CV),data_CV$classe)
