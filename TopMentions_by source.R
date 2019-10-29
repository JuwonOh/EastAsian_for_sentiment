##############################################
## http://news1.kr/articles/?3685935: Goldstein 6.1, economic cooperation?
## https://nz.news.yahoo.com/korea-japan-talks-falter-trade-risk-194656816--spt.html: Goldstein 7.0. return properties?
##############################################
library(ggplot2)
library(dplyr)
library(tibbletime)
library(xts)
library(lubridate)
setwd('~/Dropbox/ConflictCast/Data/GDELT2/KRJP')

## load data
Mentions=read.csv("KRJP_TopSourcesMentions.csv", header=T, na.strings="?")

Mentions$SQLDATE = round(Mentions[,3]/1000000)
unique(Mentions$SQLDATE)
Mentions$SQLDATE = as.Date(as.character(Mentions$SQLDATE), format = "%Y%m%d")
str(Mentions)

## making daily df
Mentions2 = group_by(Mentions, SQLDATE) #Mentions: all mentions; Mentions2: grouped by DATE
by_vs = Mentions2 %>%
    summarise(n=n(), EGoldstein=sum(GoldsteinScale)/n, 
              ETone=sum(MentionDocTone)/n,
              VarTone= var(MentionDocTone),
              AdjVarTone=VarTone*sqrt(n)) #assuemd that Tones are i.i.d

## remove duplicate url -caution: if one url has multi event, it can be removed 
MentionsRed = Mentions2[!duplicated(Mentions2$MentionIdentifier), ]
## MentionsRed: duplicated articles removed
MentionsRed = group_by(MentionsRed, SQLDATE)
by_vs2 = MentionsRed %>%
    summarise(n=n(), EGoldstein=sum(GoldsteinScale)/n, 
              ETone=sum(MentionDocTone)/n, VarTone= var(MentionDocTone),
              AdjVarTone=VarTone*sqrt(n)) #assuemd that Tones are i.i.d
by_vs2$date=0:(dim(by_vs2)[1]-1)
 #needs correction


draw_point=function(tbl, var, dates){
  temp=function(string){
      geom_point(data=subset(tbl, tbl$date==as.Date(string)),
                 aes(x=date, y=AdjVarTone), size=2, colour="#800020")
  }
  lapply(dates, temp)
} #aes(x=date, y=var) => error! why?


for(i in 1:387){
  by_vs2[i,"date"]=as.Date(as.character(by_vs[i,"SQLDATE"]), "%Y%m%d")
  i=i+1
}

## Selecting top 50 source
colnames(Mentions)
source_feq <- as.data.frame(table(Mentions["MentionSourceName"]))
top9 <- arrange(source_feq, desc(Freq))[1:9,]
top9_source <- top9$Var1
top9
## selecting specific press

daily_top9 <- Mentions %>% filter(MentionSourceName%in%top9_source) %>% 
  group_by(MentionSourceName, SQLDATE, EventBaseCode) %>% 
  summarise(n=n(), EGoldstein=sum(GoldsteinScale)/n, ETone=sum(MentionDocTone)/n, VarTone= var(MentionDocTone))#assuemd that Tones are i.i.d

daily_top9$EventBaseCode <- as.factor(daily_top9$EventBaseCode)
y.position <- max(daily_top9$ETone)
## Avtone by press

p1 <- daily_top9 %>%
  ggplot(aes(x=SQLDATE, y=ETone, group=MentionSourceName)) + 
  geom_point(aes(size=n, colour= MentionSourceName))+
  geom_smooth(method="loess", se=F)+
  #scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
  scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Avtone by 9 press", caption = "Source: SNU IR Data Center",
         y = "Average Tone", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0)+
  facet_wrap(~MentionSourceName)

pdf(file="KRJP_Avtone_by_press.pdf",family="sans", width=12, height=10)
p1
dev.off()

##Break by period
min <- as.Date("2019-06-01")
max <- as.Date("2019-09-30")

p2 <- daily_top9 %>%
  ggplot(aes(x=SQLDATE, y=ETone, group=MentionSourceName)) + 
  scale_x_date(date_breaks = "months" , date_labels = "%y-%m", limits = c(min, max)) + 
  geom_point(aes(size=n, colour= MentionSourceName))+
  geom_smooth(method="loess", se=F)+
  #scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
  labs(title="Avtone by 9 press", caption = "Source: SNU IR Data Center", y = "Average Tone", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  #annotate("text", x = as.Date("2018-10-30"), y = y.position, angle=270, size = 3, label = "Court Decision", hjust = 0)+
  annotate("text", x = as.Date("2019-07-01"), y = y.position, angle=270, size = 3, label = "Sanction four items", hjust = 0)+
  annotate("text", x = as.Date("2019-08-02"), y = y.position, angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
  annotate("text", x = as.Date("2019-08-22"), y = y.position, angle=270, size = 3, label = "GSOMIA", hjust = 0)+
  facet_wrap(~MentionSourceName)

pdf(file="KRJP_Avtone_by_press_2019.pdf",family="sans", width=12, height=10)  
p2
dev.off()

min <- as.Date("2018-10-01")
max <- as.Date("2019-01-30")

p3 <- daily_top9 %>%
  ggplot(aes(x=SQLDATE, y=ETone, group=MentionSourceName)) + 
  scale_x_date(date_breaks = "months" , date_labels = "%y-%m", limits = c(min, max)) + 
  geom_point(aes(size=n, colour= MentionSourceName))+
  geom_smooth(method="loess", se=F)+
  #scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
  labs(title="Avtone by 9 press", caption = "Source: SNU IR Data Center", y = "Average Tone", x="Month") +
  theme(axis.text.x  = element_text(size = 10, angle = 90, colour = "black",
                                    vjust = 1, hjust = 1)) +
  annotate("text", x = as.Date("2018-10-30"), y = y.position, angle=270, size = 3, label = "Court Decision", hjust = 0)+
  annotate("text", x = as.Date("2018-12-20"), y = y.position, angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
  #annotate("text", x = as.Date("2019-07-01"), y = y.position, angle=270, size = 3, label = "Sanction four items", hjust = 0)+
  #annotate("text", x = as.Date("2019-08-02"), y = y.position, angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
  #annotate("text", x = as.Date("2019-08-22"), y = y.position, angle=270, size = 3, label = "GSOMIA", hjust = 0)+
  facet_wrap(~MentionSourceName)

pdf(file="KRJP_Avtone_by_press_2018.pdf",family="sans", width=12, height=10)  
p3
dev.off()

## Using the event code and quad code, let's look more specifically at how media reports events.

daily_top9_q <- Mentions %>% filter(MentionSourceName%in%top9_source) %>% 
  group_by(SQLDATE, MentionSourceName, QuadClass) %>% 
  summarise(n=n()) %>% 
  as_tbl_time(daily_top9_q, index = SQLDATE)

monthly_top9_q <-  Mentions %>% filter(MentionSourceName%in%top9_source) %>%
  mutate(week = floor_date(SQLDATE, "month")) %>% 
  group_by(week, MentionSourceName, QuadClass) %>%
  summarise(n=n(), quad = log(sum(n))) %>% 
  group_by(week, MentionSourceName) %>% 
  mutate(percentage = n/sum(n))

monthly_top9_q $QuadClass <- factor(monthly_top9_q $QuadClass, 
                                    labels=c('Verbal Cooperation',
                                             'Material Cooperation',
                                             'Verbal Conflict',
                                             'Material Conflict.'))

## monthly plot portion of Eventcode 
y.position <- max(monthly_top9_q$percentage)
p4 <- monthly_top9_q %>%
    ggplot(aes(x=week, y=percentage, group=MentionSourceName, colour= QuadClass)) + 
    geom_point()+
    geom_line(aes(x=week, y=percentage, group=QuadClass))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Eventcode", caption = "Source: SNU IR Data Center", y = "quad", x="Month") +
    theme(legend.position="bottom", legend.box = "horizontal",
          axis.text.x  = element_text(size = 10, angle = 90,
                                      colour = "black",vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0)+
    facet_wrap(~MentionSourceName)

pdf(file="KRJP_weekly_quadP_sourece.pdf",family="sans", width=12, height=10)
p4  
dev.off()

## using top10 eventcode and using kbs 

library('gdeltr2')
cameo_events <- get_codes_cameo_events()
colnames(cameo_events)<- c("idParentCode","EventCode", "descriptionCAMEOEvent", "isParentCode")
cameo_events <- cameo_events[cameo_events$isParentCode == FALSE, ] %>% 
  filter(!grepl("specified below", cameo_events[cameo_events$isParentCode == FALSE, ]$descriptionCAMEOEvent))


kbs <- Mentions %>% 
  filter(MentionSourceName %in% "kbs.co.kr")
kbs_event_feq <- as.data.frame(table(kbs["EventCode"]))
kbstop10 <- arrange(kbs_event_feq, desc(Freq))[1:10,]
kbstop10_event <- kbstop10$Var1

kbs_top10 <- kbs %>% filter(EventCode %in% kbstop10_event) %>% 
  mutate(week = floor_date(SQLDATE, "week")) %>% 
  group_by(week, EventCode) %>% 
  summarise(n=n()) %>% 
  group_by(week) %>% 
  mutate(percentage = n/sum(n))

kbs_top10 <- merge(kbs_top10, cameo_events, by = "EventCode") 
kbs_top10$descriptionCAMEOEvent <- factor(kbs_top10$descriptionCAMEOEvent)
str(kbs_top10)

## monthly plot N and portion of eventcode by source 
y.position <- max(kbs_top10$n)
p5 <- kbs_top10 %>%
    ggplot(aes(x=week, y=n, colour= descriptionCAMEOEvent,
               shape=descriptionCAMEOEvent)) + 
    geom_point()+
    geom_line(aes(x=week, y=n, alpha=descriptionCAMEOEvent))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Eventcode: KBS", 
         caption = "Source: SNU IR Data Center",
         y = "log prevalence", x="Month") +
    theme(legend.position="bottom", legend.box = "horizontal",
          axis.text.x  = element_text(size = 10, angle = 90,
                                      colour = "black",vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0)+
    guides(fill=guide_legend(title="Event Description"))

pdf(file="KRJP_kbs_weeky_N_raw_eventcode.pdf",family="sans", width=12, height=7)  
p5
dev.off()

y.position <- max(kbs_top10$percentage)
p6 <- kbs_top10 %>%
    ggplot(aes(x=week, y=percentage, colour= descriptionCAMEOEvent,
               shape=descriptionCAMEOEvent)) + 
    geom_point()+
    geom_line(aes(x=week, y=percentage, alpha=descriptionCAMEOEvent))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Eventcode: KBS", caption = "Source: SNU IR Data Center",
         y = "percentage", x="Month") +
    theme(legend.position="bottom", legend.box = "horizontal",
          axis.text.x  = element_text(size = 10, angle = 90,
                                      colour = "black",vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0)+
    guides(fill=guide_legend(title="Event Description"))
pdf(file="KRJP_kbs_weekly_P_eventcode.pdf",family="sans", width=12, height=7)  
p6

dev.off()

## using top10 eventcode and using japantimes

jt <- Mentions %>% 
  filter(MentionSourceName == "japantimes.co.jp")
jt_event_feq <- as.data.frame(table(jt["EventCode"]))
jttop10 <- arrange(jt_event_feq, desc(Freq))[1:10,]
jttop10_event <- jttop10$Var1

jt_top10 <- jt %>% filter(EventCode %in% jttop10_event) %>% 
  mutate(week = floor_date(SQLDATE, "week")) %>% 
  group_by(week, EventCode) %>% 
  summarise(n=n()) %>% 
  group_by(week) %>% 
  mutate(percentage = n/sum(n))

jt_top10 <- merge(jt_top10, cameo_events, by = "EventCode") 
jt_top10$descriptionCAMEOEvent <- factor(jt_top10$descriptionCAMEOEvent)
str(jt_top10)


## weekly plot logN and portion of eventcode by source_jp

y.position <- max(jt_top10$n)
p7 <- jt_top10 %>%
    ggplot(aes(x=week, y=n, colour= descriptionCAMEOEvent,
               shape=descriptionCAMEOEvent)) + 
    geom_point()+
    geom_line(aes(x=week, y=n, alpha=descriptionCAMEOEvent))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Eventcode: Japan Times",
         caption = "Source: SNU IR Data Center",
         y = "Prevalence", x="Month") +
    theme(legend.position="bottom", legend.box = "horizontal",
          axis.text.x  = element_text(size = 10, angle = 90,
                                      colour = "black",
                                      vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0) +
    guides(fill=guide_legend(title="Event Description"))

pdf(file="KRJP_jt_weekly_N_raw_eventcode.pdf",family="sans", width=12, height=7)  
p7

dev.off()

y.position <- max(jt_top10$percentage)
p8 <- jt_top10 %>%
    ggplot(aes(x=week, y=percentage, colour= descriptionCAMEOEvent,
               shape=descriptionCAMEOEvent)) + 
    geom_point()+
    geom_line(aes(x=week, y=percentage, alpha=descriptionCAMEOEvent))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title="Eventcode: Japan Times", caption = "Source: SNU IR Data Center",
         y = "percentage", x="Month") +
    theme(legend.position="bottom", legend.box = "horizontal",
          axis.text.x  = element_text(size = 10, angle = 90,
                                      colour = "black",vjust = 1, hjust = 1)) +
    annotate("text", x = as.Date("2018-10-30"), y = y.position,
             angle=270, size = 3, label = "Court Decision", hjust = 0)+
    annotate("text", x = as.Date("2018-12-20"), y = y.position,
             angle=270, size = 3, label = "Low-altitude flyby", hjust = 0)+
    annotate("text", x = as.Date("2019-07-01"), y = y.position,
             angle=270, size = 3, label = "Sanction four items", hjust = 0)+
    annotate("text", x = as.Date("2019-08-02"), y = y.position,
             angle=270, size = 3, label = "Whitelist removal", hjust = 0)+
    annotate("text", x = as.Date("2019-08-22"), y = y.position,
             angle=270, size = 3, label = "GSOMIA", hjust = 0) +
    guides(fill=guide_legend(title="Event Description"))
pdf(file="KRJP_jt_weekly_P_eventcode.pdf",family="sans", width=12, height=7)  
p8
dev.off()



