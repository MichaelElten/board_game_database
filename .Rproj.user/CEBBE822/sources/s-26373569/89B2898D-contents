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

## Creating unique ID to be able to get relational database tables (publisher, artist, etc.)

data_import3 <- data_import2 %>%
  mutate(ID = row_number()) %>%
  group_by(ID) %>%
  mutate(actualID = cur_group_id()) %>%
  select(actualID, ID, everything()) %>%
  ungroup()

#EDA ----

get_unique_attribute <- function(attribute = "") {

  unique_table <- data_import3 %>%
    select(ID, Name, contains(as.character(attribute))) %>%
    pivot_longer(
      cols = contains(as.character(attribute)),
      names_to = "VarNum",
      values_to = as.character(attribute),
      values_drop_na = TRUE)

  
  unique_num <- unique_table %>%
    select(-ID, -Name, -VarNum) %>%
    distinct() %>%
    arrange(attribute) %>%
    nrow()
  
print(paste0("there are ", unique_num, " unique ",attribute,"s"))
}

get_unique_attribute(attribute="mechanic") #182 
get_unique_attribute(attribute="category") #84
get_unique_attribute(attribute="publisher") #2801
get_unique_attribute(attribute="family") #2104
get_unique_attribute(attribute="designer") #3974
get_unique_attribute(attribute="artist") #5154

create_table <- function(attribute = "") {
  new_table <- data_import3 %>%
    select(ID, Name, contains(as.character(attribute))) %>%
    pivot_longer(
      cols = contains(as.character(attribute)),
      names_to = "VarNum",
      values_to = as.character(attribute),
      values_drop_na = TRUE
    )
  return(new_table)
}


artist_table<-create_table(attribute="artist")
category_table<-create_table(attribute="category")
designer_table<-create_table(attribute="designer")
family_table<-create_table(attribute="family")
mechanic_table<-create_table(attribute="mechanic")
publisher_table<-create_table(attribute="publisher")


get_top_ranked<-function(attribute="", top_x=10){
  new_table <- data_import3 %>%
    select(ID, Name, contains(as.character(attribute))) %>%
    pivot_longer(
      cols = contains(as.character(attribute)),
      names_to = "VarNum",
      values_to = as.character(attribute),
      values_drop_na = TRUE
    )
  
  top_ranked_table <- new_table %>%
  group_by_at(attribute) %>%
  summarise(total = n()) %>%
  arrange(desc(total)) %>%
    head(top_x)
return(top_ranked_table)
}

get_top_ranked(attribute="artist") #note - uncredited should be coded as NA
get_top_ranked(attribute="category")
get_top_ranked(attribute="designer") #note - uncredited should be coded as NA
get_top_ranked(attribute="family")
get_top_ranked(attribute="mechanic")
get_top_ranked(attribute="publisher")

