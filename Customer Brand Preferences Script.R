library(caret)
library(readr)
library(readxl)

setwd("~/Dropbox/Ubiqum Code Academy/Module 2/Task 2")
ComplResponses<-read_xlsx("Survey_Key_and_Complete_Responses_excel.xlsx", 
                          sheet = "Survey Results Complete")

#To check Data types
# str(ComplResponses)
# summary(ComplResponses)
# is.na(ComplResponses)

#Data Visualization
# hist(ComplResponses$salary)
# hist(ComplResponses$credit)
# hist(ComplResponses$car)
# hist(ComplResponses$age)
# boxplot(ComplResponses$zipcode)
# boxplot(ComplResponses$salary)
# boxplot(ComplResponses$car)
# boxplot(ComplResponses$credit)
# plot(ComplResponses$brand,ComplResponses$salary)

#Change the type of Data
levels(ComplResponses$brand)<-c("Acer","Sony")
ComplResponses$brand<-as.factor(ComplResponses$brand)
ComplResponses$age<-as.integer(ComplResponses$age)
ComplResponses$elevel<-as.factor(ComplResponses$elevel)
ComplResponses$car<-as.factor(ComplResponses$car)
ComplResponses$zipcode<-as.factor(ComplResponses$zipcode)

#Descritize Age Attribute
# library(arules)
# ComplResponses$age<-discretize(ComplResponses$age, method = "interval", breaks=3)

#Split train/test sets to 75%/25%
set.seed(998)
inTrain<-createDataPartition(ComplResponses$brand, p=.75, list = FALSE)
TrainSize<-ComplResponses[inTrain,]
TestSize<-ComplResponses[-inTrain,]

#Invastigate Relationship Between Attributes
library(rpart)
library(rpart.plot)
DecisionTreeCorrelation<- rpart(brand ~ .,data=ComplResponses, method="class", maxdepth=5)
rpart.plot(DecisionTreeCorrelation, type= 1, extra=101)
library(labeling)
ggplot(ComplResponses,aes(salary,age,col=brand))+geom_point()

#Pre-Process for centering and scaling
trainX <- TrainSize[,names(TrainSize) != "brand"]
preProcValues <- preProcess(x=trainX, method=c("center", "scale"))

#KNN PREDICTION PROCESS
#Creating a 10-fold Cross-Validation
DifineCV<-trainControl(method = "repeatedcv", number = 10, repeats = 2) 
#Training
KNNmodel<-train(brand~ salary+age, data=TrainSize, method="knn", 
                trControl=DifineCV,preProcess = c("center","scale"), tuneLength = 20)
#Accuracy and Kappa values
KNNmodel
plot(KNNmodel)
#Prediction in the TestSize
knnPredict <- predict(KNNmodel,newdata = TestSize)
postResample(pred=knnPredict , obs=TestSize$brand)
confusionMatrix(knnPredict,TestSize$brand)
#General Customer Brand Preference
summary(knnPredict)
summary(ComplResponses$age)


#---------------------------------------------------------------------------------------

#RANDOM FOREST PREDICTION PROCESS
#Creating a random, 10-fold Cross-Validation
DifineCV<-trainControl(method = "repeatedcv", number = 10, repeats = 2, search="random")
#Training
library(randomForest)
RFmodel<-train(brand~ salary+age, data=TrainSize, method="rf", 
               trControl=DifineCV, preProcess = c("center","scale"))
RFmodel
#Prediction in the TestSize
RFPredict<-predict(RFmodel,newdata = TestSize)
postResample(pred = RFPredict, obs=TestSize$brand)
confusionMatrix(RFPredict,TestSize$brand)
#General Customer Brand Preference
summary(RFPredict)

#---------------------------------------------------------------------------------------

#Incomplete Responses Prediction
IncomplResponses<-read.csv("SurveyIncomplete.csv")

#Apply KNNmodel so that to predict the Brand
str(IncomplResponses)
IncomplResponses$brand<-as.factor(IncomplResponses$brand)
FinalKnnPredict <- predict(KNNmodel,newdata = IncomplResponses)
summary(FinalKnnPredict)
IncomplResponses$brand<-FinalKnnPredict

write.csv(IncomplResponses)

#Thank you 