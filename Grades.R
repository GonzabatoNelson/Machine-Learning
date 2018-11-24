shh<-suppressMessages
shh(library(tidyverse))
shh(library(caret))
shh(library(Amelia))
shh(library(rvest))
shh(library(RCurl))
shh(library(GGally))
#Reading our data
student_perf<-("http://archive.ics.uci.edu/ml/machine-learning-databases/00467/Sapfile1.arff")
#student_perf<-textConnection(student_perf)
student_perf_data<-read.csv(student_perf,header = F,comment.char = "@")
head(student_perf_data)
#write.csv(student_perf_data,"students.csv")
#The above works but a better way would be to use RWeka read.arff function
student_perf_data<-as.tibble(student_perf_data)
student_perf_data
names(student_perf_data)<-c("Gender","CST","TNP","TWP","IAP","ESP","Arr","Marital Status",
                            "LS","AS","FMI","FS","FQ","MQ","FO","MO","NF",
                            "SH","Ss","ME","TT","ATD")
#dataset description:https://www.researchgate.net/figure/Dataset-Description_tbl1_322331338 
student_perf_data<-student_perf_data %>% 
  rename(Caste=CST,Grade_X=TNP,Grade_XII=TWP,Internal_Grade=IAP,EndSem=ESP,
         Arrear=Arr,Marital=`Marital Status`,Home=LS,Fee=AS,Family_Income=FMI,
         Fanily_Size=FS,Father=FQ,Mother=MQ,Fat_Job=FO,Mot_Job=MO,
         Friends=NF,Study=SH,SchoolType=Ss,Medium=ME,Distance=TT,Attendance=ATD)
#Already clean data. Time for some EDA
levels(student_perf_data$EndSem)<-c("Best","Good","Pass","Very Good")
student_perf_data %>% 
  ggplot(aes(Gender,fill=Gender))+geom_bar(stat="count")+
  scale_fill_manual(values=c("indianred","steelblue3"))+
  ggtitle("Does Attendance Predict Academic performance? ")+
  facet_grid(EndSem~Attendance)+
  labs(caption="Y: Grade X: Attendance",y="Number of Students")+
  theme(plot.caption = element_text(size=12),
        plot.title = element_text(size=15,hjust=0.5,colour = "peru"),
        panel.grid= element_blank(),
        panel.background = element_rect(fill="white"),
        strip.background = element_rect(fill="tan3"),
        strip.text = element_text(colour = "white"))
#From the graph we see a possible correlation between attendance and academic performance
str(student_perf_data) 
levels(student_perf_data$Grade_X)<-c("Best","Good","Pass","Very Good")
levels(student_perf_data$Grade_XII)<-c("Best","Good","Pass","Very Good")
levels(student_perf_data$Internal_Grade)<-c("Best","Good","Pass","Very Good")
#remove marital status
student_perf_data<-student_perf_data %>% 
 select(-Marital)
#This is a very rudimentary way to do label encoding
#Let's test and see how well our models will do
#Target variable is EndSem
shh(library(onehot))
student_ohc<-onehot(student_perf_data,max_levels = 12)
student_ohc_data<-predict(student_ohc,student_perf_data)
#Time for some machien learning
student_ohc_data<-as.data.frame(student_ohc_data)
#Machine learning minus OHC
train_set<-createDataPartition(student_perf_data$EndSem,p=0.8,list=F)
validate_set<-student_perf_data[-train_set,]
train_set<-student_perf_data[train_set,]
#Set Metric and Control
control<-trainControl(method="cv",number = 10)
metric<-"Accuracy"
#Set up our models
set.seed(233)
fit.knn<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
               method="knn")
set.seed(233)
fit.gbm<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
               method="gbm",verbose=F)
set.seed(233)
fit.rf<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
               method="rf",verbose=F)
set.seed(233)
fit.rf<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
              method="rf",verbose=F)
set.seed(233)
fit.xgb<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
              method="xgbTree",verbose=F)
set.seed(233)
fit.cart<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
              method="rpart")
set.seed(233)
fit.svm<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
              method="svmRadial")
set.seed(233)
fit.nb<-train(EndSem~.,data = train_set,metric=metric,trControl=control,
              method="nb",verbose=F)
#Results
results<-resamples(list(knn=fit.knn,xgb=fit.xgb,cart=fit.cart,gbm=fit.gbm,
                        nb=fit.nb,rf=fit.rf,svm=fit.svm))
#Plot of our results
dotplot(results)
#Table of Accuracies
accuracies<-data.frame(knn=results$values$`knn~Accuracy`,xgb=results$values$`xgb~Accuracy`,
           svm=results$values$`svm~Accuracy`,cart=results$values$`cart~Accuracy`,
           nb=results$values$`nb~Accuracy`,rf=results$values$`rf~Accuracy`,
           gbm=results$values$`gbm~Accuracy`)
colorme<-sample(scales::hue_pal()(70))
accuracies %>% 
  gather("id","value",1:ncol(.)) %>% 
  ggplot(aes(id,value,fill=id))+geom_point()+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0,1))+
  geom_boxplot()+geom_point(col="gray35")+
  ggtitle("Average Accuracies of Our Models")+
  ggpubr::theme_pubclean()+
  guides(fill=F)+
  theme(plot.title = element_text(hjust=0.5))+
  labs(y="Accuracy",x="Model")
#Our best models are clearly the knn and xgboost
#Let's test on validation set
validated<-predict(fit.xgb,validate_set)
confusionMatrix(validated,validate_set$EndSem)
#The end for now