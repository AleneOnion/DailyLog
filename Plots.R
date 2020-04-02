library(readxl)
library(tidyr)
library(dplyr)
library(tidyverse)
library(lubridate)

alene<-read_excel("C:/Users/amonion.000/New York State Office of Information Technology Services/LMAS - General/telecommuting_journal_V2 (002)_AMO.xls",sheet="Work Journal",col_types = c("date", "text","text", "numeric", "numeric","text","text","text"))
alene<-as.data.frame(alene)
alene<-alene %>%
  # rename with first row
  set_names(c("DATE","Project","Description of Work","Estimated hrs","Actual hrs","Planned/Unplanned","Result","Comments")) %>%
  # delete the first row
  slice(-2) %>% 
  filter(!is.na(DATE)) %>% 
  mutate(employee="Alene Onion") %>% 
  distinct() 
alene<-alene %>% 
  mutate(week=((yday(DATE)-5)/7),
         week=as.integer(week)) %>% 
  group_by(week,Project) %>% 
  summarize(hrs=sum(`Actual hrs`)) %>% 
  ungroup() %>% 
  select(Project,week,hrs) %>% 
  distinct() %>% 
  mutate(hrs=ifelse(is.na(hrs),0,hrs)) %>% 
  filter(!is.na(Project)) %>% 
  distinct() %>% 
  mutate(week=as.character(week))

ggplot(data=alene, aes(x=Project, y=hrs, fill=week)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal()+
  scale_fill_brewer(palette="Blues") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
