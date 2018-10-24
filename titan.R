#Basic Machine Learning on kaggle titanic dataset
nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caret))
nomsg(library(mice))
nomsg(library(xgboost))
library(RANN)
library(caretEnsemble)
library(Amelia)
train<-read.csv("train.csv",stringsAsFactors = F)
train<-as.tibble(train)
#Remove cabin,name,Ticket for now as these may not predict survival 

#View and deal with nas
newtrain<-train %>%
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Survived=as.factor(Survived),Embarked=as.factor(Embarked),
         AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
      select(-PassengerId,-Name,-Ticket,-Cabin)
#Change levels
levels(newtrain$AgeGroup)<-c("Young","Mid Age","Aged")
levels(newtrain$Sex)<-c("F","M")
#Impute median
newtrain_1<-preProcess(newtrain,method="medianImpute")
newtrain_imp<-predict(newtrain_1,newtrain)
#checkNAs
anyNA(newtrain_imp)
#View NAS
newtrain_imp %>% 
  map_dbl(~sort(sum(is.na(.x),decreasing=T)))
#redo levels
newtrain_imp<-newtrain_imp %>% 
  mutate(AgeGroup=as.factor(findInterval(Age,c(0,18,35,100))))
levels(newtrain_imp$AgeGroup)<-c("Young","Mid Age","Aged")
anyNA(newtrain_imp)
#Let's visualise survival by Age Group
newtrain_imp %>% 
   ggplot(aes(Survived,fill=Sex))+geom_histogram(stat="count")+facet_wrap(AgeGroup~Pclass)+
  ggtitle("Survival by class,Agegroup and Gender")+
  theme(plot.title=element_text(hjust=0.5))+
  scale_fill_manual(values=c("orange","steelblue4"))
#The graph does suggest that being of mid Age and embarking in the third class made you more likely to die
#Overall more women than men survived.
#Partition our data into a training and test dataset

#Create partition
train1<-createDataPartition(newtrain_imp$Survived,p=0.8,list=F)
validate<-newtrain_imp[-train1,]
train1<-newtrain_imp[train1,]
#Set metric and control
control<-trainControl(method="cv",number =10)
metric<-"Accuracy"
#Set up models
set.seed(233)
fit.knn<-train(Survived~.,data=train1,method="knn",trControl=control,metric=metric)
set.seed(233)
fit.svm<-train(Survived~.,data=train1,method="svmRadial",trControl=control,metric=metric)
set.seed(233)
fit.cart<-train(Survived~.,data=train1,method="rpart",trControl=control,metric=metric)
set.seed(233)
fit.rf<-train(Survived~.,data=train1,method="rf",trControl=control,metric=metric)
set.seed(233)
fit.nb<-train(Survived~.,data=train1,method="nb",trControl=control,metric=metric)
#Try Gradiet Boosting
fit.gbm<-train(Survived~.,data=train1,method="gbm",trControl=control,metric=metric,
               verbose=F)
fit.xgboo<-train(Survived~.,data=train1,method="xgbTree",trControl=control,metric=metric,
                 verbose=F)

fit.glm1<-train(Survived~.,data=train1,method="glm",trControl=control,metric=metric)
result<-resamples(list(knn=fit.knn,svm=fit.svm,cart=fit.cart,rf=fit.rf,nb=fit.nb,
                       gbm=fit.gbm,xgb=fit.xgboo,glm=fit.glm1))
dotplot(result)

#Validate 
predicted<-predict(fit.rf,validate)
confusionMatrix(predicted,validate$Survived)

#Try on test data
test<-read.csv("test.csv",stringsAsFactors = F)
test<-as.tibble(test)
#............
#Make as train data

#.....
newtest<-test %>% 
  mutate(PassengerId=as.factor(PassengerId),Pclass=as.factor(Pclass),
         Embarked=as.factor(Embarked),
         AgeGroup=as.factor(findInterval(Age,c(0,18,35,100)))) %>% 
  select(-Ticket,-Name,-Cabin)
levels(newtest$Embarked)<-c("","C","Q","S")
levels(newtest$AgeGroup)<-c("Young","Mid Age","Aged")
levels(newtest$Sex)<-c("F","M")
#Find NAs
newtest %>% 
  map_lgl(~anyNA(.x))
#Preprocess and remove NAs from age and Fare
newtest_1<-preProcess(newtest,method="medianImpute")
newtest_imp<-predict(newtest_1,newtest)
   #map nas
anyNA(newtest_imp)
#Check Dona
newtest_imp<-newtest_imp %>% 
  mutate(AgeGroup=as.factor(findInterval(Age,c(0,18,35,100))))
levels(newtest_imp$AgeGroup)<-c("Young","Mid Age","Aged")
predictedtest<-predict(fit.gbm,newtest_imp,na.action=na.pass)
#Check variable  importance
plot(fit.rf,main="Random Forest")
plot(fit.gbm)
plot(fit.xgboo)
plot(fit.cart)
#Set column
Survival<-newtest_imp%>% 
  mutate(Survived=predictedtest) %>% 
  select(PassengerId,Survived)
#find the confusion matrix
cm<-confusionMatrix(predictedtest,Survival$Survived)
cm$overall
#Using xgboost
write.csv(Survival,"submitme45.csv",row.names = F)
#Feature Engineering


#Tuned GBM 
set.seed(233)
gbgrid<-expand.grid(n.trees=950,
                    interaction.depth=25,
                    shrinkage=0.01,
                    n.minobsinnode=15)
fit.gbm_modi<-train(Survived~.,data=train1,method="gbm",tuneGrid=gbgrid,
                    trControl=control,metric=metric,
               verbose=F)
print(fit.gbm_modi)
#
predictedtest_mod<-predict(fit.gbm_modi,newtest_imp,na.action=na.pass)
Survival<-newtest_imp%>% 
  mutate(Survived=predictedtest_mod) %>% 
  select(PassengerId,Survived)
#find the confusion matrix
cm1<-confusionMatrix(predictedtest_mod,Survival$Survived)
cm1$overall
write.csv(Survival,"submitme46.csv",row.names = F)
