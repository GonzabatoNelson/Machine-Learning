#Bikes
#Add packages
suppressMessages(library(tidyverse))
suppressMessages(library(caret))
daily_bike<-read.csv("C:\\Users\\Lance Nelson\\Downloads\\Bike-Sharing-Dataset\\day.csv")
head(daily_bike)
daily_bike<-as.tibble(daily_bike)
str(daily_bike)
suppressMessages(require(lubridate))
daily_bike<-daily_bike %>% 
  mutate(instant=as.factor(instant),dteday=ymd(dteday),season=as.factor(season),
         holiday=as.factor(holiday),workingday=as.factor(workingday)) %>% 
  select(-mnth,-weekday) %>% 
  mutate_if(is.integer,as.double)
#Remove instant as it has no real value
head(daily_bike)
daily_bike<-daily_bike %>% 
  select(-instant,-yr) %>% 
  rename(Date=dteday,workday=workingday,registrd=registered,windspd=windspeed,
         Condition=weathersit) %>% 
  mutate(Condition=as.factor(Condition))
#Check for missing values
suppressMessages(require(Amelia))
anyNA(daily_bike)
#Map missing values
daily_bike %>% 
  map_lgl(~anyNA(.x))
#Our Data set contains no missig values
#Set some auto colours
cols<-sample(scales::hue_pal()(12))
#Do some Exploratory Data Analysis
daily_bike<-daily_bike %>%
  mutate(Year=year(Date),Month=month(Date),Day=wday(Date)) %>% 
  select(Year,Month,Day,everything(),-Date)  
  daily_bike %>% 
 group_by(Year) %>% 
  ggplot(aes(as.factor(Year),registrd,fill=Year))+geom_boxplot()+
  labs(x="Year",y="Registered Users")+
  guides(fill=F)+
  ggtitle("Registered Bike Users by Year")+
  theme(panel.background = element_rect(fill="snow"),
        panel.grid = element_blank(),
        axis.title.y=element_text(angle=60,vjust=0.5,size=12),
        axis.title.x.bottom = element_text(size=12),
        plot.title = element_text(hjust=0.5,size=15))
#Clearly there were on average more registered users in 2012 than 2011. Business
#was booming
#What of the casual count?
suppressMessages(require(reshape2))
#Time series plot requires long format
daily_bike %>% 
mutate(Date=make_date(Year,Month,Day)) %>% 
  select(Date,registrd,cnt,-Year,-Month,-Day) %>% 
  melt(id.vars="Date") %>% 
  ggplot(aes(Date,value,colour=variable))+geom_line()+
  ggthemes::theme_economist()+
labs(y="Number of Users",title="Bike Usage by Year and Month",
     caption="Nelson Made It")+
 scale_color_manual(values=c("green4","red"),
                    labels=c("Registered","Casual Count"))+
  theme(panel.background = element_rect(fill="white",color="black"),
        panel.grid = element_blank(),
        plot.background = element_blank(),
        axis.title =element_text(size=14),
        plot.title = element_text(hjust=0.5),
        plot.caption = element_text(size=15))+
  guides(color=guide_legend(title = "Type",title.theme = element_text(size=14)))
 #Looking at usage by month in 2011
daily_bike %>% 
  filter(Year==2011) %>% 
  mutate(Month=month(Month,label=T)) %>% 
  arrange(desc(registrd)) %>% 
  ggplot(aes(Month,registrd,fill=Month))+
  geom_col()+
  scale_fill_manual(values=cols)+
  guides(fill=guide_legend(reverse=F))+
  ggthemes::theme_pander()+
  ggtitle("Monthly Bike Usage in 2011")+
  scale_y_continuous(position="right")+
  theme(plot.title=element_text(hjust = 0.5),
        panel.background = element_rect(fill="papayawhip"),
        panel.grid = element_blank(),
        axis.title.y.right = element_text(angle = 78,vjust = 0.5))+
  labs(y="Number of Users")+
  guides(fill=F)
plotly::ggplotly()
#Make a time series beginning with May 2011 with built in time series function
bike_ts<-daily_bike %>% 
        group_by(Year,Month) %>% 
    summarise(casual=sum(casual)) %>% 
     ungroup() 
#Make a time series to predict the next five months
bike_time_series<-ts(bike_ts$casual,start = c(2011,12),end=c(2012,5),frequency = 12)
#Fit an Arima
suppressMessages(require(forecast))
fit.arima<-Arima(bike_time_series,order=c(1,0,12))
forecast::gghistogram(fit.arima)
#Actual Data
plot(fit.arima$x,col="indianred4",main="Actual vs Predicted Bike Usage",
     ylab="Number of Users")
#Fitted model values
lines(fitted(fit.arima),col="dodgerblue")
#Fcast
forecasted<-forecast(bike_time_series,h=3)
autoplot(forecasted)+
  autolayer(fitted(forecasted))
