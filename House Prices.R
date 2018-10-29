nomsg<-suppressMessages
nomsg(library(tidyverse))
nomsg(library(caretEnsemble))
nomsg(library(caret))
nomsg(library(RANN))
nomsg(library(mice))
nomsg(library(DiagrammeR))
nomsg(library(Amelia))
nomsg(library(Metrics))
#Start working on data cleaning
training<-read.csv("train.csv")
training<-as.tibble(training)
#Find missing data
View(training %>% 
  map_dbl(~sum(is.na(.x))) %>% 
  sort(decreasing = T))
#Missing
missmap(training,col=c("indianred","steelblue4"),main="Who's Missing out?!",
        y.cex=0.2,x.cex=0.3)
#Change levelsof all categorical data in this crude way. NA is not missing data
newtrain<-training %>% 
 mutate(GarageType=fct_explicit_na(GarageType,na_level = "No"),
        Alley=fct_explicit_na(Alley,na_level = "No"),
        Fence=fct_explicit_na(Fence,na_level = "No"),
        FireplaceQu=fct_explicit_na(FireplaceQu,na_level = "No"),
        GarageFinish=fct_explicit_na(GarageFinish,na_level = "No"),
        GarageQual=fct_explicit_na(GarageQual,na_level = "No"),
        GarageCond=fct_explicit_na(GarageCond,na_level = "No"),
        BsmtExposure=fct_explicit_na(BsmtExposure,na_level = "No"),
        BsmtFinType2=fct_explicit_na(BsmtFinType2,na_level = "No"),
        BsmtQual=fct_explicit_na(BsmtQual,na_level = "No"),
        MasVnrType=fct_explicit_na(MasVnrType,na_level = "No"),
        Electrical=fct_explicit_na(Electrical,na_level = "No"))
#Find remaining NAs
newtrain %>% 
  map_dbl(~sum(is.na(.x))) %>% 
  sort(decreasing = T)
levels(newtrain$GarageYrBlt)
#Change levels PoolQc and these others because NA is not missing data
 newtrain<-newtrain %>% 
   mutate(PoolQC=fct_explicit_na(PoolQC,na_level = "No"),
          MiscFeature=fct_explicit_na(MiscFeature,na_level = "No"),
          BsmtCond=fct_explicit_na(BsmtCond,na_level = "No"),
          BsmtFinType1=fct_explicit_na(BsmtFinType1,na_level = "No")) %>% 
          select(-Id)
 #Find remain missing data
 newtrain %>% 
   map_dbl(~sum(is.na(.x))) %>% 
   sort(decreasing = T)
 #Impute missing data with mice
 exclude<-c("GarageYrBlt","MasVnrArea")
 include<-setdiff(names(newtrain),exclude)
 set.seed(233)
 newtrain1<-newtrain[include]
 #Impute missing data
 newtrain_imp<-mice(newtrain1,m=1,method = "cart",printFlag = F)
 newtrain11<-complete(newtrain_imp)
 newtrain11 %>% 
   map_dbl(~sum(is.na(.x))) %>% 
   sort(decreasing = T)
 #Great, There is no more missing data
#Train
trainme<-createDataPartition(newtrain11$SalePrice,p=0.85,list=F)
validateme<-newtrain11[-trainme,]
trainme<-newtrain11[trainme,]
control<-trainControl(method ="cv",number=10)
metric<-"RMSE"
set.seed(233)
fit.svm<-train(SalePrice~.,data=trainme,method="svmRadial",trControl=control,metric=metric)
fit.knn<-train(SalePrice~.,data=trainme,method="knn",trControl=control,metric=metric)
fit.gbm<-train(SalePrice~.,data=trainme,method="gbm",trControl=control,metric=metric,
               verbose=F)
fit.xgb<-train(SalePrice~.,data=trainme,method="xgbTree",trControl=control,metric=metric)
set.seed(233)
fit.rf<-train(SalePrice~.,data=trainme,method="rf",trControl=control,metric=metric)

#.....
result<-resamples(list(svm=fit.svm,knn=fit.knn,gbm=fit.gbm,rf=fit.rf,xgb=fit.xgb))
dotplot(result)
print(fit.gbm)#0.88
print(fit.rf)#0.87
print(fit.xgb)#0.86
#Tuning the GBM model
gbmgrid<-expand.grid(
  n.trees=1000,
  interaction.depth=18,
  shrinkage=0.03,#Smaller more trees,
  n.minobsinnode=3#Higher values faster imputation
)
#Modified model test
fit.gbm_mod<-train(SalePrice~.,data=trainme,method="gbm",
                   trControl=control,metric=metric,
               verbose=F,tuneGrid=gbmgrid)

#Model Performaces
getTrainPerf(fit.gbm)
getTrainPerf(fit.xgb)
getTrainPerf(fit.rf)
getTrainPerf(fit.svm)
#Predict on validation set
predval<-predict(fit.gbm,validateme)
#Load test data
testing<-read.csv("test.csv")
#Remove NAs in a very crude way. Not suitable for extremely large data. Use a function instead
testing<-testing %>% 
  mutate(PoolQC=fct_explicit_na(PoolQC,na_level = "No"),
         MiscFeature=fct_explicit_na(MiscFeature,na_level = "No"),
         BsmtCond=fct_explicit_na(BsmtCond,na_level = "No"),
         BsmtFinType1=fct_explicit_na(BsmtFinType1,na_level = "No"),
         GarageType=fct_explicit_na(GarageType,na_level = "No"),
         Alley=fct_explicit_na(Alley,na_level = "No"),
         Fence=fct_explicit_na(Fence,na_level = "No"),
         FireplaceQu=fct_explicit_na(FireplaceQu,na_level = "No"),
         GarageFinish=fct_explicit_na(GarageFinish,na_level = "No"),
         GarageQual=fct_explicit_na(GarageQual,na_level = "No"),
         GarageCond=fct_explicit_na(GarageCond,na_level = "No"),
         BsmtExposure=fct_explicit_na(BsmtExposure,na_level = "No"),
         BsmtFinType2=fct_explicit_na(BsmtFinType2,na_level = "No"),
         BsmtQual=fct_explicit_na(BsmtQual,na_level = "No"),
         MasVnrType=fct_explicit_na(MasVnrType,na_level = "No"),
         Electrical=fct_explicit_na(Electrical,na_level = "No"))
#Impute missing data
#Preprocess the data
set.seed(233)
exclude<-c("GarageYrBlt","MasVnrArea","Alley")
include<-setdiff(names(testing),exclude)
set.seed(233)
newtest1<-testing[include]
#Impute missing data
newtest_imp<-mice(testing,m=1,method = "cart",printFlag = F)
newtest11<-complete(newtest_imp)
#Check for missing values 
View(newtest11 %>% 
  map_dbl(~sum(is.na(.x))) %>% 
  sort(decreasing = T))
#View the NA
getTrainPerf(fit.gbm_mod)
gbmgrid1<-expand.grid(
  n.trees=4556,
  interaction.depth=10,
  shrinkage=0.01,#Smaller more trees,
  n.minobsinnode=10#Higher values faster imputation
)
#Modified model test
set.seed(233)
fit.gbm_mod1<-train(SalePrice~.,data=trainme,method="gbm",
                   trControl=trainControl(method="repeatedcv",number=10,repeats=3),
                   metric=metric,
                   verbose=F,tuneGrid=gbmgrid1)
getTrainPerf(fit.gbm_mod1)
#modify xgboost

#Predict as we have 2 more missing values. replace these with 0
predictedme<-predict(fit.gbm_mod1,newtest11,na.action = na.pass)
resultme<-newtest11 %>% 
  mutate(SalePrice=predictedme) %>% 
  mutate_all(funs(replace(.,is.na(.),mean(.)))) %>% 
  select(Id,SalePrice)
anyNA(resultme)
#metrics
require(Metrics)
paste0("RMSE is ",rmse(resultme$SalePrice,predictedme))
paste0("RSE is ",rse(resultme$SalePrice,predictedme))    
write.csv(resultme,"mysubmit27.csv",row.names = F)


