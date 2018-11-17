nomsg<-suppressMessages
nomsg(library(rvest))
nomsg(library(tidyverse))
nomsg(library(plotly))
emma<-read_html("https://www.ncbi.nlm.nih.gov/pubmed/?term=emanuel%20syndrome")
titles<-html_text(html_nodes(emma,".rprt .title"))
head(titles)
authors<-html_text(html_nodes(emma,".rprt .supp .desc "))
head(authors)
Year_of_pub<-html_text(html_nodes(emma,".rprt .supp .details"))
head(Year_of_pub)
Year_of_Pub<-Year_of_pub %>% 
  str_remove_all("[[:punct:]]")
  Year_of_Pub<-sapply(strsplit(Year_of_pub,"\\."),"[[",2)
Year_of_pub<-sapply(strsplit(Year_of_Pub,";"),"[[",1)
Year_of_Pub<-sapply(strsplit(Year_of_pub," "),"[[",2)
PMID<-html_text(html_nodes(emma,"dd"))
#Create a data frame
str(authors)
First_Author<-sapply(strsplit(authors,","),"[[",1)
str(Year_of_Pub)
str(titles)
journals<-html_text(html_nodes(emma,".rprt .supp .details"))
str(journals)
head(journals)
journals<-sapply(strsplit(journals,"\\."),"[[",1)
#Create a DataFrame
emmanuel_syndrome<-data.frame(Journal=journals,First_Author=First_Author,
                              PMID=PMID,Year=Year_of_Pub,Title=titles)
head(emmanuel_syndrome)
ema_synd<-as.tibble(emmanuel_syndrome)
str(ema_synd)
nomsg(require(lubridate))
ema_synd %>% 
  mutate(Year=as.character.Date(Year)) %>% 
  filter(Year>=2015) %>% 
  ggplot(aes(Year,fill=Year))+geom_histogram(stat="count")+
  ggtitle("Emanuel Syndrome Best Match PubMed")+
  ggthemes::theme_economist()
 #Make
#Extract names and make a wordcloud
nomsg(require(tm))
nomsg(require(wordcloud))
nomsg(require(SnowballC))
#using tidytext
nomsg(require(tidytext))
Titles_df<-data.frame(line=1:20,text=ema_synd$Title)
Titles_df %>% 
  mutate(text=as.character(text)) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word,sort=T) %>% #Count word frequencies 
  mutate(word=reorder(word,n)) %>% 
  top_n(15,n) %>% 
  ggplot(aes(word,n,fill=word))+
  geom_col()+
  coord_flip()+
  labs(y="Word Frequency",title="What's in The Titles?",
       caption="Nelson learns to mine")+
  guides(fill=F)+
  ggthemes::theme_wsj()+
  theme(plot.title = element_text(hjust=0.5,color="indianred4"))
#Sentiment Analysis
nrc<-get_sentiments("nrc")
Titles_df %>% 
  mutate(text=as.character(text)) %>% 
  unnest_tokens(word,text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  ggplot(aes(sentiment,fill=sentiment))+geom_bar(stat="count")+
  ggtitle("How Positive are Our Titles?")+
  scale_fill_manual(values=c("steelblue","indianred4"))+
  ggthemes::theme_fivethirtyeight()+guides(fill=F)+
  labs(caption="Not So Positive")+
  theme(plot.caption = element_text(size=12,colour = 'navy'),
        plot.title = element_text(hjust=0.5,size=16,color="navy"))
#Most common Positive and Negative Words
Titles_df %>% 
  mutate(text=as.character(text)) %>% 
  unnest_tokens(word,text) %>% 
  inner_join(get_sentiments("bing")) %>% 
  group_by(sentiment) %>% 
  count(word,sort=T) %>% 
 ungroup() %>% 
  ggplot(aes(word,n,fill=sentiment))+geom_col(show.legend = F)+
  facet_wrap(.~sentiment,scales ="free_x")+coord_flip()+
  ggtitle("Top Words By Sentiment")+
  theme(plot.title = element_text(hjust=0.5))+
  labs(y="Contribution to Total Score")+
  ggpubr::theme_cleveland()
#A wordcloud
Titles_df %>% 
  mutate(text=as.character(text)) %>% 
  unnest_tokens(word,text) %>% 
  anti_join(stop_words) %>% 
  count(word) %>% 
  with(wordcloud(word,n,min.freq = 1))
  
