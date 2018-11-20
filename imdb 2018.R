#imdb 2018
nomsg<-suppressMessages
nomsg(library(rvest))
nomsg(library(tidyverse))
movies<-read_html("https://www.imdb.com/search/title?count=100&release_date=2018,2018&title_type=feature")
#Getting Titles
titles<-html_text(html_nodes(movies,"h3.lister-item-header"))
head(titles)
titles<-sapply(strsplit(titles,"\n"),"[[",4)
str(titles)
#Getting genre
genre<-html_text(html_nodes(movies,".genre"))
head(genre)
str(genre)
#get only one Genre
Genre<-sapply(strsplit(genre,","),"[[",1)
str(Genre)
Genre<-str_remove_all(Genre,"\n")
#Getting Duration
RunTime<-html_text(html_nodes(movies,".runtime"))
head(RunTime)
#Extract Time from this
RunTime<-sapply(strsplit(RunTime," "),"[[",1)
str(RunTime)
#Rating
Rating<-html_text(html_nodes(movies,".text-muted .certificate"))
head(Rating)
str(Rating)
#Stars
metascore<-html_text(html_nodes(movies,".ratings-metascore"))
head(metascore)
#Keep Only Numbers
metascore<-sapply(strsplit(metascore,"\n"),"[[",2)
str(metascore)
votes<-html_text(html_nodes(movies,".sort-num_votes-visible"))
head(votes)
votes<-votes %>% 
  str_remove_all("\n")
votes<-sapply(strsplit(votes,":"),"[[",2)
votes<-votes %>% 
  str_remove_all(" | ") %>% 
  str_split("Gross") %>% 
  str_remove_all("[[:punct:]]") %>% 
  str_remove_all("\\D")
#Combine into single dataframe
Movies_df<-data.frame(Title=titles[1:80],metascore=metascore[1:80],Votes=votes[1:80],
                      Rating=Rating[1:80],Duration=RunTime[1:80],Genre=Genre[1:80],
                      stringsAsFactors = F)
Movies_df<-as.tibble(Movies_df)
head(Movies_df)
str(Movies_df)
Movies_df<-Movies_df %>% 
  mutate(metascore=as.numeric(metascore),Votes=as.numeric(Votes),
         Duration=as.numeric(Duration),Genre=as.factor(Genre),Title=as.factor(Title))
head(Movies_df)
summary(Movies_df$metascore)
#A simple plot
nomsg(require(tidytext))
#Choosing random colors
cols<-sample(scales::hue_pal()(10))
#
Movies_df %>% 
 arrange(desc(Votes)) %>% 
  top_n(10,Votes) %>% 
  mutate(Title=fct_reorder(Title,Votes)) %>% 
ggplot(aes(as.factor(Votes),Title,fill=Title))+
  geom_col(position =position_stack(reverse=F))+
  coord_flip()+
  ggthemes::theme_igray()+
  labs(x="User Votes",caption="Source: IMDB")+geom_text(aes(label=metascore))+
ggtitle("Top 10 Movies of 2018")+
  theme(axis.text.x = element_blank(),axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        plot.caption = element_text(size=14),
        plot.title = element_text(hjust=0.5))+
  guides(fill=guide_legend(reverse = T))+
  scale_fill_manual(values=cols)
#Some tidytext
nomsg(require(wordcloud))
nomsg(require(wordcloud2))
nrc<-get_sentiments("nrc")
movies_df1<-data_frame(line=1:nrow(Movies_df),text=Movies_df$Title)
#requires charcater input
require(reshape2)
movies_df1 %>% 
  mutate_if(is.factor,as.character) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>% 
  count(word,sentiment,sort=T) %>% 
  acast(word~sentiment,value.var = "n",fill=0) %>% 
  comparison.cloud(colors =c("gray20","gray"),max.words =50)
