library(readr)
library(tidyverse)

#data processing script for board games

#load data in - there are 125,058 total board games... need to cut it down a bit
bgg_data<-read_csv("./raw_data/bggdata.csv") 

# now filtered to 9k - a bit better!
bgg_data3<-bgg_data %>%
  filter(usersrated>200) %>%
  filter(owned>200)%>%
    textcat(str_to_lower(name0))%in% c("english") ~ name0,
    textcat(str_to_lower(name1))%in% c("english") ~ name1,
    textcat(str_to_lower(name2))%in% c("english") ~ name2,
    textcat(str_to_lower(name3))%in% c("english") ~ name3,
    textcat(str_to_lower(name4))%in% c("english") ~ name4,
    textcat(str_to_lower(name5))%in% c("english") ~ name5,
    textcat(str_to_lower(name6))%in% c("english") ~ name6,
    textcat(str_to_lower(name7))%in% c("english") ~ name7,
    textcat(str_to_lower(name8))%in% c("english") ~ name8,
    textcat(str_to_lower(name9))%in% c("english") ~ name9,
    TRUE ~ name0)) %>%
  select(new_name, name0,name1,name2,name3,name4,name5,name6,name7,name8,name9,everything()) %>%
  select(-contains("honor"),-contains("version"),-contains("implementation"),-contains("accessory"),-contains("expansion"),-contains("compilation"))

bgg_data3 <-bgg_data3 %>%
  select(-contains("honor"),-contains("version"),-contains("implementation"),-contains("accessory"),-contains("expansion"),-contains("compilation"))

  


write_csv(bgg_data3,"./output/board_game_dataset.csv")
