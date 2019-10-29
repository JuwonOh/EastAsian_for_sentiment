#http://news1.kr/articles/?3685935: Goldstein 6.1, economic cooperation?
#https://nz.news.yahoo.com/korea-japan-talks-falter-trade-risk-194656816--spt.html: Goldstein 7.0. return properties?

setwd('~/Dropbox/ConflictCast/Data/GDELT2/KRJP')

library(dplyr)
library(ggplot2)

Mentions=read.csv("KRJS_TopSourcesMentions.csv", header=T, na.strings="?")
Mentions$SQLDATE = Mentions[,3]/1000000

Mentions2=group_by(Mentions, SQLDATE) #Mentions: all mentions; Mentions2: grouped by DATE
by_vs = Mentions2 %>% summarise(n=n(), EGoldstein=sum(GoldsteinScale)/n, 
                                   ETone=sum(MentionDocTone)/n, VarTone= var(MentionDocTone), AdjVarTone=VarTone*sqrt(n)) #assuemd that Tones are i.i.d

MentionsRed=Mentions2[!duplicated(Mentions2$MentionIdentifier), ] #MentionsRed: duplicated articles removed
MentionsRed=group_by(MentionsRed, SQLDATE)
by_vs2 = MentionsRed %>% summarise(n=n(), EGoldstein=sum(GoldsteinScale)/n, 
                                ETone=sum(MentionDocTone)/n, VarTone= var(MentionDocTone), AdjVarTone=VarTone*(n-1)) #assuemd that Tones are i.i.d
by_vs2$date=0:(dim(by_vs2)[1]-1)
by_vs2$date=as.Date(by_vs2$date, origin="2018-10-01") #needs correction
for(i in 1:dim(by_vs2)[1]){
  by_vs2[i,"date"]=as.Date(as.character(by_vs2[i,"SQLDATE"]), "%Y%m%d")
  i=i+1
}

draw_point=function(tbl, var, dates){
  temp=function(string){
    geom_point(data=subset(tbl, tbl$date==as.Date(string)), aes(x=date, y=EGoldstein*n), size=2, colour="#800020")
  }
  lapply(dates, temp)
} #aes(x=date, y=var) => error! why?


for(i in 1:2122){
  by_vs_nat[i,"date"]=as.Date(as.character(by_vs_nat[i,"Mentions$SQLDATE"]), "%Y%m%d")
  i=i+1
}

#Heteroskedasticity-adjusted Tone Variance with redundancy included
ggplot(data=by_vs, aes(x=date, y=AdjVarTone))+geom_line(colour="#00AFBB", size=1)+draw_point(by_vs, AdjVarTone, c("2018-10-30", "2019-07-01", "2019-08-02", "2019-08-22"))+theme_minimal()

#Heteroskedasticity-adjusted Tone Variance with redundancy removed
ggplot(data=by_vs2, aes(x=date, y=AdjVarTone))+geom_line(colour="#00AFBB", size=1)+draw_point(by_vs2, AdjVarTone, c("2018-10-30", "2019-07-01", "2019-08-02", "2019-08-22"))+theme_minimal()

ggplot(data=by_vs_nat, aes(x=date, y=AdjVarTone))+geom_line(colour="#00AFBB", size=1)+draw_point(by_vs_nat, AdjVarTone, c("2018-10-30", "2019-07-01", "2019-08-02", "2019-08-22"))+theme_minimal()


