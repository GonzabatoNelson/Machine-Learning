## ------------------------------------------------------------------------
shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
shh(library(reshape2))
trainset<-read.csv("data_set_ALL_AML_train.csv",stringsAsFactors = F,quote = "")
head(trainset,10)
#Replace everything that starts with X as some other name. Number
#Num is the experiment number?
colnames(trainset)<-gsub("X{1,}","Num",colnames(trainset))
trainset<-trainset %>% 
 select(-contains("call"),-Gene.Description) %>% 
  melt(id.vars="Gene.Accession.Number") %>% 
  spread(Gene.Accession.Number,value) %>% 
  rename(ExptNo.=variable)
#Find Missing values
#trainset %>% 
  #map_dbl(~sum(is.na(.x)))
#That was a bad way to check for NAs for a very large dataset.
#Simply use anyNa
anyNA(trainset)
#dataset contains no missing values
#Load patient data and cancer type
actual<-read.csv("actual.csv",stringsAsFactors = F,quote="")
#Susbset data for first 72 patients
trainme<-trainset[1:72,]
#bind to training set
patientdata<-cbind(trainme,actual)
patientdata<-patientdata %>% 
  select(patient,cancer,everything()) %>% 
  mutate(patient=as.factor(patient),cancer=as.factor(cancer))
training<-patientdata %>% 
  filter(!is.na(ExptNo.)) 
#Use this data to train the models on Cancer type. 
patientdata


## ------------------------------------------------------------------------

patientdata %>% 
select(-ExptNo.) %>% 
  gather("id","value",3:ncol(.)) %>% 
 mutate(GeneAccNum=as.factor(id),genelvls=value) %>% 
  select(-id,-value) %>% 
  top_n(20) %>% 
 ggplot(aes(GeneAccNum,patient))+geom_tile(aes(fill=genelvls))+
  theme(axis.text.x = element_text(angle=45),
plot.title = element_text(hjust=0.5))+facet_wrap(~cancer)+
  ggtitle("Top 20 Genes Expressed in AML and ALL patients")+
  labs(x="Gene Accession Number",caption="Based on original dataset")
  

## ------------------------------------------------------------------------
#First partition the data into a training and validating dataset.
training<-training %>% 
         select(-patient,-ExptNo.)
traincanc<-createDataPartition(training$cancer,p=0.85,list=F)
validatecanc<-training[-traincanc,]
traincanc<-training[traincanc,]
#Setup metric and control. We'll use accuracy as our metric
control<-trainControl(method="cv",number=5)
metric<-"Accuracy"
#Set up models
set.seed(233)
fit.svm1<-train(cancer~.,data=traincanc,method="svmRadial",trControl=control,metric=metric,
                verbose=F)
set.seed(233)
fit.knn1<-train(cancer~.,data=traincanc,method="knn",trControl=control,metric=metric)
set.seed(233)
fit.rf1<-train(cancer~.,data=traincanc,method="rf",trControl=control,metric=metric,
               verbose=F)
result<-resamples(list(rf=fit.rf1,svm=fit.svm1,knn=fit.knn1))
dotplot(result)

## ------------------------------------------------------------------------
predictcanc<-predict(fit.knn1,validatecanc)
confusionMatrix(predictcanc,validatecanc$cancer)

## ------------------------------------------------------------------------
testcanc<-read.csv("data_set_ALL_AML_independent.csv")
#Process our data to look ust like the training set
head(testcanc)
#Change col headings
colnames(testcanc)<-gsub("X{1,}","Num",colnames(testcanc))
#confirm changes
head(testcanc)
#Process data
testcanc<-testcanc %>% 
  select(-contains("call"),-Gene.Description) %>% 
  melt(id.vars="Gene.Accession.Number") %>% 
  spread(Gene.Accession.Number,value) %>%  
  rename(ExptNo.=variable) %>% 
  select(-ExptNo.)

#Check for missing values
anyNA(testcanc)
#dateset contains no missing values
#Apply model
predicttest<-predict(fit.knn1,testcanc)
testcanc1<-testcanc %>% 
          mutate(cancer=as.factor(predicttest),patientId=row_number()) %>% 
          select(patientId,cancer,everything())
#Confusion matrix
confusionMatrix(predicttest,testcanc1$cancer)

