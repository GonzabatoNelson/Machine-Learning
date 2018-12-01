#Time series practice
shh<-suppressMessages
shh(library(tidyverse))
shh(library(forecast))
shh(library(foreign))
eeg_data<-read.arff("https://archive.ics.uci.edu/ml/machine-learning-databases/00264/EEG%20Eye%20State.arff")
eeg_data<-as.tibble(eeg_data)

#sensors AF3,P8 etc
#0 indicates the eye is open
#Values are time in minutes
#Exploratory data analysis
str(eeg_data)
#Does our data have any missing values
eeg_data %>% 
  map_dbl(~sum(is.na(.x)))
#There is only one missing value in AF4, replace this with 0
eeg_numeric<-eeg_data %>% 
  select_if(is.numeric) %>% 
  mutate_all(funs(replace(.,is.na(.),0)))
anyNA(eeg_numeric)
levels(eeg_data$eyeDetection)<-c("Open","Closed")
eeg_categoric<-eeg_data %>% 
               select_if(is.factor)
eeg_clean<-as.tibble(cbind.data.frame(eeg_categoric,eeg_numeric))
eeg_clean<-eeg_clean %>% 
  select(AF3,F7,everything()) %>% 
  rename(eye=eyeDetection)
#Check again that there are no missing values
eeg_clean %>% 
  map_lgl(~anyNA(.x))
#write.csv(eeg_clean,"eeg_clean.csv",row.names = F)
#Great we've successfully removed all NAs
#A few basic plots
str(eeg_clean)
eeg_clean
#Just make a fancy missmap
shh(library(Amelia))
options(warn=-1)
missmap(eeg_clean,col=c("springgreen3","yellow3"),
        main="Did we capture all the data?",y.cex = 0.00002)
#Distribution of our data
options(scipen=999)
eeg_clean %>% 
  ggplot(aes(eye,F7,fill=eye))+geom_boxplot()+
  ggpubr::theme_pubr()+
  guides(fill=F)+labs(x="Eye State",y="Sensor AF3",
                      caption="EEG Data",title="Eye State using sensor AF3")+
 theme(plot.title = element_text(hjust=0.5))
str(eeg_clean) 
#Look at number of times eye was open
eeg_clean %>% 
  ggplot(aes(eye,fill=eye))+geom_bar()+
  scale_fill_manual(values=c("indianred","dodgerblue","goldenrod"))+
  ggthemes::theme_excel()+
  labs(x="Eye State",y="Number of Times",
       caption="EEG Data",title="Eyes wide open,huh?")+
  guides(fill=F)+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="peachpuff"),
        axis.text = element_text(family="Arial",size=13),
        axis.title = element_text(family="Helvetica",size=15),
        plot.title = element_text(hjust=0.5,family="Helvetica",size=15,
                                 color="steelblue3"))

summary(eeg_clean)
#Make data time series
eeg_ts<-ts(eeg_clean[,c(2,4,6)],frequency = 60,start=c(2000))

autoplot(eeg_ts)
#Subset our data
eeg_sub<-window(eeg_ts,start=c(2000),end=c(4100))
autoplot(eeg_sub)+
  ggthemes::theme_gdocs()+
  labs(caption="EEG Data")+
  theme(plot.caption = element_text(size=14,colour="dodgerblue"),
        panel.background = element_rect(fill="grey12"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill="grey15"))
#Test for stationarity
tseries::adf.test(eeg_sub[,2])
#Data is stationary and we can thus proceed
#Choosing optimal p,d,q
auto.arima(eeg_sub[,2])
#seasonally adjusted series for sensor F3
eeg_sub2<-eeg_sub[,2] %>% 
  stl(s.window = "periodic") %>% 
  seasadj()
autoplot(eeg_sub2)+geom_line(col="dodgerblue")
#Differenced data
eeg_sub2 %>% 
  diff() %>% 
  ggtsdisplay()
#Fit an ARIMA with p=1,q=6
fit.arima<-Arima(eeg_sub2,order = c(1,1,0))
checkresiduals(fit.arima)
autoplot(fit.arima)
#Do a very rudimentary eye state classification
eeg_clean<-as.tibble(as.data.frame(eeg_clean))
eeg_clean
#if(!require(eegkit))install.packages("eegkit")
#using eegkit
shh(library(eegkit))
eegcap(electrodes = "10-10",type="2d")
eegcap(electrodes=names(eeg_clean),type="2d",
       cex.point =5.8,col.point = "dodgerblue",pch.point=13,
       pch.border=15,col.label = "snow",
       cex.label = 0.9)
eeg_long<-eeg_clean %>% 
  select(eye,everything()) %>% 
  gather("Electrode","Time",2:ncol(.)) 
#Just for fun
eeg_long<-eeg_long %>%  
  mutate(Time_S=Time*60,Time_Hr=Time/60) %>% 
  arrange(Time_Hr) 
eeg_long %>% 
  dplyr::filter(Time_Hr<=800) %>%
 ggplot(aes(Electrode,Time_Hr,col=eye))+geom_point()+geom_line()
#Attempting to plot a topopo plot
#Using Examples from stackooverflow
#if(!require(akima))install.packages("akima")
#Back to classification
#Split into a train and validation set
shh(library(caret))
train_eeg<-createDataPartition(eeg_clean$eye,p=0.8,list = F)
valideeg<-eeg_clean[-train_eeg,]
train_eeg<-eeg_clean[train_eeg,]
#Metric and control
control<-trainControl(method="cv",number=10)
metric<-"Accuracy"
#Set up models
set.seed(1234)
fit.knn<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="knn")
getTrainPerf(fit.knn)
set.seed(1234)
fit.gbm<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="gbm",
               verbose=F)
set.seed(233)
fit.rf<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="rf")
set.seed(233)
fit.svm<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="svmLinear")
set.seed(233)
fit.glm<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="glm")
set.seed(233)
fit.cart<-train(eye~.,data=train_eeg,trControl=control,metric=metric,method="rpart")
#View performance
perf<-resamples(list(knn=fit.knn,
                     svm=fit.svm,gbm=fit.gbm,rf=fit.rf,
                     glm=fit.glm,cart=fit.cart))
dotplot(perf)
#Test on validation set
validated<-predict(fit.knn,valideeg)
confusionMatrix(validated,valideeg$eye)
#Plotting our knn decision boundary
library(rattle)
fancyRpartPlot(fit.cart$finalModel)
plot(fit.rf$finalModel,main="Random Walk in the Forest")

