#########################
plot.list <- list()

## Selecting top 10 source

colnames(Mentions)
source_feq <- as.data.frame(table(Mentions["MentionSourceName"]))
top <- arrange(source_feq, desc(Freq))[1:20,]
top_source <- as.character(top$Var1)

## Load event code book
library('gdeltr2')
cameo_events <- get_codes_cameo_events()
colnames(cameo_events)<- c("idParentCode","EventCode", "descriptionCAMEOEvent", "isParentCode")
cameo_events <- cameo_events[cameo_events$isParentCode == FALSE, ] %>% 
  filter(!grepl("specified below", cameo_events[cameo_events$isParentCode == FALSE, ]$descriptionCAMEOEvent))

### preprocess
for(i in 1:length(top_source)){
  event_top10 <- Mentions %>%
    filter(MentionSourceName %in% top_source[[i]])
  source_event_feq <- as.data.frame(table(event_top10["EventCode"]))
  sourcetop10 <- arrange(source_event_feq, desc(Freq))[1:10,]
  sourcetop10_event <- as.character(sourcetop10$Var1)
  
  kbs_top10 <- event_top10 %>% filter(EventCode %in% sourcetop10_event) %>% 
    mutate(week = floor_date(SQLDATE, "week")) %>% 
    group_by(week, EventCode) %>% 
    summarise(n=n()) %>% 
    group_by(week) %>% 
    mutate(percentage = n/sum(n))
  
  kbs_top10 <- merge(kbs_top10, cameo_events, by = "EventCode") 
  kbs_top10$descriptionCAMEOEvent <- factor(kbs_top10$descriptionCAMEOEvent)
  
  ## weekly plot N and portion of eventcode by source 
  y.position <- max(kbs_top10$n)
  p5 <- kbs_top10 %>%
    ggplot(aes(x=week, y=n, colour= descriptionCAMEOEvent,
               shape=descriptionCAMEOEvent)) + 
    geom_point()+
    geom_line(aes(x=week, y=n, alpha=descriptionCAMEOEvent))+
    ## scale_shape_manual(values = c(1:length(unique(senate_monthly_n$party)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%y-%m") + 
    labs(title= paste0("Eventcode:", top_source[[i]]),
         caption = "Source: SNU IR Data Center",
         y = "prevalence", x="Month") +
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
  
  pdf(file=paste0("~/Dropbox/ConflictCast/plot/KRJP_", top_source[[i]], "_weeky_N_raw_eventcode.pdf"),family="sans", width=12, height=7)
      
  print(p5)
  dev.off()
}

## percent by source
for(i in 1:length(top_source)){
  event_top10 <- Mentions %>%
    filter(MentionSourceName %in% top_source[[i]])
  source_event_feq <- as.data.frame(table(event_top10["EventCode"]))
  sourcetop10 <- arrange(source_event_feq, desc(Freq))[1:10,]
  sourcetop10_event <- as.character(sourcetop10$Var1)
  
  kbs_top10 <- event_top10 %>% filter(EventCode %in% sourcetop10_event) %>% 
    mutate(week = floor_date(SQLDATE, "week")) %>% 
    group_by(week, EventCode) %>% 
    summarise(n=n()) %>% 
    group_by(week) %>% 
    mutate(percentage = n/sum(n))
  
  kbs_top10 <- merge(kbs_top10, cameo_events, by = "EventCode") 
  kbs_top10$descriptionCAMEOEvent <- factor(kbs_top10$descriptionCAMEOEvent)
  
  ## weekly plot N and portion of eventcode by source 
  
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
 
  pdf(file=paste0("~/Dropbox/ConflictCast/plot/KRJP_", top_source[[i]], "_weeky_p_eventcode.pdf"),family="sans", width=12, height=7)
  print(p6)
  dev.off()
}

## nationality

colnames(Mentions)
nationality_feq <- as.data.frame(table(Mentions["Nationality"]))
top <- arrange(nationality_feq, desc(Freq))[1:20,]
top_source <- as.character(top$Var1)

for(i in 1:length(top_source)){
  event_top10 <- Mentions %>%
    filter(Nationality %in% top_source[[i]])
  source_event_feq <- as.data.frame(table(event_top10["EventCode"]))
  sourcetop10 <- arrange(source_event_feq, desc(Freq))[1:10,]
  sourcetop10_event <- as.character(sourcetop10$Var1)
  
  kbs_top10 <- event_top10 %>% filter(EventCode %in% sourcetop10_event) %>% 
    mutate(week = floor_date(SQLDATE, "week")) %>% 
    group_by(week, EventCode) %>% 
    summarise(n=n()) %>% 
    group_by(week) %>% 
    mutate(percentage = n/sum(n))
  
  kbs_top10 <- merge(kbs_top10, cameo_events, by = "EventCode") 
  kbs_top10$descriptionCAMEOEvent <- factor(kbs_top10$descriptionCAMEOEvent)
  
  ## weekly plot N and portion of eventcode by source 
  
  y.position <- max(kbs_top10$percentage)
  p7 <- kbs_top10 %>%
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
  
  pdf(file=paste0("~/Dropbox/ConflictCast/plot/KRJP/nationality_", top_source[[i]], "_weeky_p_eventcode.pdf"),family="sans", width=12, height=7)
  print(p7)
  dev.off()
}

