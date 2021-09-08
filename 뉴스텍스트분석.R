library(stringr)
basic_url <- 'https://search.naver.com/search.naver?&where=news&query=%EC%9D%98%EC%82%AC&sm=tab_pge&sort=0&photo=0&field=0&reporter_article=&pd=0&ds=&de=&docid=&nso=so:r,p:all,a:all&mynews=0&cluster_rank=38&start=number&refresh_start=0'
urls <- NULL
for(x in 0:10){
  urls[x+1] <- str_replace(basic_url, "number", as.character(x*10+1))
  }
urls
library(rvest)
library(dplyr)
links <- NULL
for(url in urls){
  html <- read_html(url)
  links <- c(links, html %>% html_nodes('.txt_inline') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}

links
txts <- NULL
for(link in links){
  if(link!='#'){html <- read_html(link)
  txts <- c(txts, html %>% html_nodes('._article_body_contents') %>% html_text())}
  else{next}
}
library(tm)
txt2<-txts
for(i in 1:length(txt2)){
a[i]<-as.integer(regexpr("}", txt2[i]))
b[i]<-as.integer(regexpr("@", txt2[i]))
txt2[i]<-substr(txt2[i],a[i], b[i])
}
txt2<-gsub("<.+?>|\t|\n", " ",txt2)
txt2<-str_trim(txt2)
txt2<-unique(txt2)
txt2[1:6]
#텍스트 정리 완료
library(tidytext)
library(KoNLP)
word<-extractNoun(txt2)
typeof(word)
View(word)
capture.output(word, file="단어목록.txt")
#감정분석
remotes::install_github("mrchypark/KOSACR")

library(KOSACR)

KOSACR::get_kosac()
KOSACR::sentiments$lex %>% table #사전 종류 

#http://kkma.snu.ac.kr/documents/?doc=postag 품사태깅 참조 
KOSACR::sentiments %>% filter(lex=="polarity") %>% select(value) %>% table
pos.dic <- KOSACR::sentiments %>% filter(lex=="polarity") %>% filter(value %in% c("POS"))
neg.dic <- KOSACR::sentiments %>% filter(lex=="polarity") %>% filter(value %in% c("NEG"))

pos=0
for(wo in pos.dic){
  pos_match = str_count(word, wo)
  pos = pos+pos_match
}

neg=0
for(wo in neg.dic){
  neg_match = str_count(word, wo)
  neg = neg+neg_match
}

pos
neg

#명사만으로는 의미를 알 수가 없잖아..
text<-as.data.frame(txt2)
View(text)
typeof(text)
View(text %>% unnest_tokens(input=txt2, output='word') %>% count(word, sort = TRUE))
View(text %>% unnest_tokens(input=txt2, output='word', token="ngrams", n=2) %>% count(word, sort = TRUE))

library(widyr)
text %>% unnest_tokens(input=txt2, output='word', token="sentences")
#동시출현빈도 해보자..

#다음 크롤링
basic_url_d <-'https://search.daum.net/search?w=news&DA=PGD&enc=utf8&cluster=y&cluster_page=number&q=%EC%9D%98%EC%82%AC&p=number'
urls_d <- NULL
for(x in 0:10){
  urls_d[x+1] <- str_replace_all(basic_url_d, "number", as.character(x*10+1))
}
urls_d
links_d <- NULL
for(url in urls_d){
  html <- read_html(url)
  links_d <- c(links_d, html %>%  html_nodes('.wrap_cont') %>% html_nodes('.f_nb.date') %>% html_nodes('a') %>% html_attr('href') %>% unique())
}

links_d
txts_d <- NULL
for(link in links_d){
  html <- read_html(link)
  txts_d <- c(txts_d, html %>% html_nodes('.news_view') %>% html_text())}
txts_d[1:4]
library(tm)
txt2_d <-NULL
txt2_d<-txts_d
txt2_d<-gsub("[^[:alnum:]///' ]", " ",txt2_d)
txt2_d<-str_trim(txt2_d)
txt2_d<-unique(txt2_d)
txt2_d[1:6]
text_d<-as.data.frame(txt2_d)
View(text_d)
View(text_d %>% unnest_tokens(input=txt2_d, output='word') %>% count(word, sort = TRUE))
View(text_d %>% unnest_tokens(input=txt2_d, output='word', token="ngrams", n=2) %>% count(word, sort = TRUE))
#tf-idf
news_word <- 