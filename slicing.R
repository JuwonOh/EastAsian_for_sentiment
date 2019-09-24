require(tibbletime)
library(dplyr)
library(lubridate)
library(stringr)
library(qdap)

setwd('~/github/sentiment analysis')
  
load('news_data.Rdata')
news_data$content <- gsub("U.S", "US", news_data$content)
news_data$content <- gsub("US.", "US", news_data$content)
news_data$title <- gsub("U.S", "US", news_data$title)
news_data$title <- gsub("US.", "US", news_data$title)

fox <- news_data %>% 
  filter(source == "fox") %>% 
  select('id', "date", "content", "title")

nyt <- news_data %>% 
  filter(source == "nyt") %>% 
  select('id', "date", "content", "title")

wp <- news_data %>%  
  filter(source == "wp") %>% 
  select('id', "date", "content", "title")

cnn <- news_data %>%  
  filter(source == "cnn") %>% 
  select('id', "date", "content", "title")

wsj <- news_data %>% 
  filter(source == "wsj") %>% 
  select('id', "date", "title") 
wsj$week <- lubridate::week(ymd(wsj$date))

wsjC <- wsj %>% 
  filter(str_detect(title, "China|Beijing|Chinese|Xi"))
wsjC$nation <- "c"
wsjK <- wsj %>% 
  filter(str_detect(title, "Korea|Seoul|Korean|Jae-in"))
wsjK$nation <- "k"
wsjn <- wsj %>% 
  filter(str_detect(title, "North Korea|Pyongyang|North Korean|Jung-eun|Jungeun"))
wsjn$nation <- "n"
wsjJ <- wsj %>% 
  filter(str_detect(title, "Japan|Tokyo|Japanese|Abe|Sinzo"))
wsjJ$nation <- "j"
temp <- rbind(wsjC, wsjK, wsjJ, wsjn)
wsj_s <- temp[!duplicated(temp$title), ]
wsj_s$week <- lubridate::week(ymd(wsj_s$date))

sentence_slice <- function(df){
  df <- sentSplit(df, "content")
  dfC <- df %>% 
    filter(str_detect(content, "China|Beijing|Chinese|Xi"))
  dfC$nation <- "c"
  dfK <- df %>% 
    filter(str_detect(content, "South Korea|Seoul|South Korean|Moon"))
  dfK$nation <- "k"
  dfn <- df %>% 
    filter(str_detect(content, "North Korea|Pyongyang|North Korean|Jung-eun|Jungeun"))
  dfn$nation <- "n"
  dfJ <- df %>% 
    filter(str_detect(content, "Japan|Tokyo|Japanese|Abe"))
  dfJ$nation <- "j"
  temp <- rbind(dfC, dfK, dfJ, dfn)
  sentences <- temp[!duplicated(temp$content), ]
  sentences$week <- lubridate::week(ymd(sentences$date))
  return(sentences)
}
fox_s <- sentence_slice(fox)
nyt_s <- sentence_slice(nyt)
wp_s <- sentence_slice(wp)
cnn_s <- sentence_slice(cnn)

write.csv(fox_s, "fox_all_sentence.csv")
write.csv(wp_s, "wp_all_sentence.csv")
write.csv(nyt_s, "nyt_all_sentence.csv")
write.csv(wsj_s, "wsj_all_sentence.csv")
write.csv(cnn_s, "cnn_all_sentence.csv")
