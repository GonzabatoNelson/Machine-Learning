#Predicting Insurance Charges
insure<-read.csv("insurance.csv")
shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
#Check for missing values.
insure %>% 
  map_lgl(~anyNA(.x))
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
   ggtitle("Insurance Charge distribution by Age, Sex and region")
 #We see a general overall higher charge in females.
 #Divide the dataset into a training and validation set for some machine learning predictions
 trainds<-createDataPartition(insure$Agegroup,p=0.8,list=F)
 validate<-insure[-trainds,] 
 trainds<-insure[trainds,] 
#Set metric and control
 control<-trainControl(method="repeatedcv",number=10,repeats=5)
 metric<-"Accuracy" 
 #Set up models
 set.seed(233)
 fit.knn<-train(Agegroup~.,data=trainds,method="knn",trControl=control,metric=metric) 
 set.seed(233)
 fit.cart<-train(Agegroup~.,data=trainds,method="rpart",trControl=control,metric=metric) 
 set.seed(233)
 fit.rf<-train(Agegroup~.,data=trainds,method="rf",trControl=control,metric=metric) 
 set.seed(233)
 fit.nb<-train(Agegroup~.,data=trainds,method="nb",trControl=control,metric=metric) 
 set.seed(233)
 fit.svm<-train(Agegroup~.,data=trainds,method="svmRadial",trControl=control,metric=metric) 
 results<-resamples(list(knn=fit.knn,nb=fit.nb,rf=fit.rf,svm=fit.svm,cart=fit.cart))
#Visualize model accuarcies
 dotplot(results)
 #Choose the svm model
 predicted<-predict(fit.svm,validate)
 confusionMatrix(predicted,validate$Agegroup) 
#The model accuarcy is not the best and can certainly be improved.  