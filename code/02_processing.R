## 02- looking at importing googlesheets.

#set up environment ----

library(tidyverse)
library(googlesheets4)

#Import data ----

SS<-"https://docs.google.com/spreadsheets/d/1Y0Odg0ar_YsraYrcQ4r-oJJzSDpIK97SkLBv0yhRMWk/edit#gid=1796417297"
data_import<-googlesheets4::read_sheet(SS, sheet = "full_dataset")

## Clean data ----

data_import2<-data_import %>%
    select(-contains("rank"))%>%
    mutate(across(contains("designer")|contains("artist")|contains("category")| contains("mechanic") |
                  contains("family")| contains("publisher"),
                ~ ifelse(.x == "NA", NA, .x)))
test<-head(data_import,100) %>%
  select(contains("rank")) %>%
  mutate(across(contains("rank")),as.numeric(.x))

#EDA ----


create_table <- function(name = "") {
  new_table <- data_import3 %>%
    select(ID, Name, contains(as.character(name))) %>%
    pivot_longer(
      cols = contains(as.character(name)),
      names_to = "VarNum",
      values_to = as.character(name),
      values_drop_na = TRUE
    )
  
  new_table2 <- new_table %>%
    group_by_at(name) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  return(new_table2)
}


all_mechanics<-c(data_import$mechanic0, data_import$mechanic1, data_import$mechanic2,data_import$mechanic3,
                 data_import$mechanic4, data_import$mechanic5, data_import$mechanic6, data_import$mechanic7,
                 data_import$mechanic8, data_import$mechanic9)
unique_mechanics<-unique(all_mechanics)
# unique_mechanics - there are 183 unique mechanics
#####

all_categorys<-c(data_import$category0, data_import$category1, data_import$category2,data_import$category3,
                 data_import$category4, data_import$category5, data_import$category6, data_import$category7,
                 data_import$category8, data_import$category9)
unique_categorys<-unique(all_categorys)
# unique_categories - there are 85 unique categories

all_publishers<-c(data_import$publisher0, data_import$publisher1, data_import$publisher2,data_import$publisher3,
                 data_import$publisher4, data_import$publisher5, data_import$publisher6, data_import$publisher7,
                 data_import$publisher8, data_import$publisher9)
unique_publishers<-unique(all_publishers)

unique_publishers
# - there are 2802 unique publisher

all_familys<-c(data_import$family0, data_import$family1, data_import$family2,data_import$family3,
                  data_import$family4, data_import$family5, data_import$family6, data_import$family7,
                  data_import$family8, data_import$family9)
unique_familys<-unique(all_familys)
# unique_categories - there are 85 unique family

all_designers<-c(data_import$designer0, data_import$designer1, data_import$designer2,data_import$designer3,
               data_import$designer4, data_import$designer5, data_import$designer6, data_import$designer7,
               data_import$designer8, data_import$designer9)
unique_designers<-unique(all_designers)
# there are 3975 unique designers

all_artists<-c(data_import$artist0, data_import$artist1, data_import$artist2,data_import$artist3,
               data_import$artist4, data_import$artist5, data_import$artist6, data_import$artist7,
               data_import$artist8, data_import$artist9)
unique_artists<-unique(all_artists)
#there are 5155 unique artist


### Creating unique ID to be able to get relational database tables (publisher, artist, etc.)

data_import3 <- data_import2 %>%
  mutate(ID = row_number()) %>%
  group_by(ID) %>%
  mutate(actualID = cur_group_id()) %>%
  select(actualID, ID, everything())


create_table <- function(name = "") {
  new_table <- data_import3 %>%
    select(ID, Name, contains(as.character(name))) %>%
    pivot_longer(
      cols = contains(as.character(name)),
      names_to = "VarNum",
      values_to = as.character(name),
      values_drop_na = TRUE
    )
  
  new_table2 <- new_table %>%
    group_by_at(name) %>%
    summarise(total = n()) %>%
    arrange(desc(total))
  return(new_table2)
}
artist_table<-create_table(name="artist")
publisher_table2<-create_table(name="publisher")

print_x<-function(x=""){
  
  print(as.character(x))
}
