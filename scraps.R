junk<-hrs %>% 
  mutate(Project=toupper(Project),
         week=as.character(week)) %>% 
  rename(hours=`Actual hrs`) %>% 
  mutate(hours=ifelse(is.na(hours),0,hours)) %>% 
  select(week,Project,hours) %>% 
  filter(Project=="GENERAL/SECTION") %>% 
  group_by(week) %>% 
  summarise(hourst=sum(hours)) %>% 
  ungroup()
junk

junk<-combined %>% 
  filter(Project=="GENERAL/SECTION")
junk


rmarkdown::render('Daily.Log.HTML.Rmd',  
                  output_file =  paste("Daily.Log.HTML.", format((Sys.Date()-1),format='%Y-%m-%d'), ".html", sep=''), 
                  output_dir = 'C:/Users/amonion.000/New York State Office of Information Technology Services/LMAS - General/Daily.Logs')
