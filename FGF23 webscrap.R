library(microbenchmark)
library(tidyverse)
library(rvest)
library(XML)
url<-"https://www.ncbi.nlm.nih.gov/pubmed/?term=fgf23%20review"
my_url<-read_html(url)
titles<-html_text(html_nodes(my_url,".title"))
head(titles)
#Get authors
authors<-html_text(html_nodes(my_url,".desc"))
head(authors)
#Get Date of Publication
Journals<-html_text(html_nodes(my_url,".content .jrnl"))
head(Journals)
PMID<-html_text(html_nodes(my_url,".resc"))
PMID<-PMID %>% 
 str_replace_all("Free PMC Article","") %>% 
  str_replace_all("Free Article","") %>% 
  str_remove_all("Free Books & Documents") %>% 
  str_remove_all("PMID:")
PMID<-as.numeric(PMID)
str(PMID)
str(authors)
str(Journals)
str(titles)
#Publication Year
FGF<-data.frame(Author=authors,Journal=c(Journals,"NA"),
                Title=titles,PMID=PMID)
FGF<-as.tibble(FGF)
options(scipen=100)
FGF<-FGF %>% 
   mutate(Title=as.character(Title)) %>% 
   select(Title,Journal,PMID)
FGF %>% 
  ggplot(aes(Journal,as.factor(PMID),fill=Journal))+geom_bar(stat="identity")+
 coord_flip()+
  ggpubr::theme_cleveland()+
  theme(axis.text.x = element_text(angle=90,size=12))+
  guides(fill=F)+labs(y="PubMedID")
  
