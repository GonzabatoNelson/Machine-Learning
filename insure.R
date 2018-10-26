#Predicting Insurance Charges
insure<-read.csv("insurance.csv")
shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
shh(library(Amelia))   
#Check for missing values.
missmap(insure,col=c("steelblue3","snow"),main="Who's Skipping Insurance?",
        x.cex=0.7,y.cex=0.3)    
#Date contains no missing values.
insure<-as.tibble(insure)
#View structure of the dataset
str(insure)
levels(insure$sex)<-c("F","M")
#Create Agegroup column
insure %>% 
  select_if(is.numeric) %>% 
  map_dbl(~max(.x))
#Minimum age is 18 and maximum is 64
insure<-insure %>% 
  mutate(Agegroup=as.factor(findInterval(age,c(18,35,50,80))))
levels(insure$Agegroup)<-c("Youth","Mid Aged","Old")
levels(insure$smoker)<-c("N","Y")
levels(insure$region)<-c("NE","NW","SE","SW")
#Visualise distribution of charges by agegroup,sex and region
insure %>% 
  ggplot(aes(region,charges,fill=sex))+geom_boxplot()+facet_grid(~Agegroup)+
  ggtitle("Insurance Charge distribution by Age, Sex and region") 
 #It appears that those above 50 years("Old") are charged more with males in 
 #the North East paying the least. However, there are outliers. Let's look at these
insure %>% 
  filter(charges>=30000) %>% 
  ggplot(aes(region,charges,fill=sex))+geom_boxplot()+facet_grid(~Agegroup)+
  ggtitle("Outlier Charges") 
 #We see a general overall higher charge in females.
 #Divide the dataset into a training and validation set for some machine learning predictions
#Divide the dataset into a training and validation set for some machine learning predictions
trainds<-createDataPartition(insure$Agegroup,p=0.8,list=F)
validate<-insure[-trainds,] 
trainds<-insure[trainds,]  
#Set metric and control
control<-trainControl(method="cv",number=10)
metric<-"RMSE" 
#Set up models 
set.seed(233)
fit.knn<-train(charges~.,data=trainds,method="knn",trControl=control,metric=metric) 
set.seed(233)
fit.svm<-train(charges~.,data=trainds,method="svmRadial",trControl=control,metric=metric) 
set.seed(233)
fit.gbm<-train(charges~.,data=trainds,method="gbm",trControl=control,metric=metric,
               verbose=F) 
set.seed(233)
fit.xgb<-train(charges~.,data=trainds,method="xgbTree",trControl=control,metric=metric,
               verbose=F) 
fit.rf<-train(charges~.,data=trainds,method="xgbTree",trControl=control,metric=metric,
               verbose=F) 
results<-resamples(list(knn=fit.knn,svm=fit.svm,xgb=fit.xgb,gbm=fit.gbm,rf=fit.rf))   
#Visualize model accuarcies
dotplot(results)  
 #Choose the gbm model
getTrainPerf(fit.gbm)
#...RF Perf 
getTrainPerf(fit.rf)
#...XGB Perf
getTrainPerf(fit.xgb)
predicted<-predict(fit.gbm,validate)
plot(fit.gbm,main="GBM") 
