## 02- looking at importing googlesheets.

#set up environment ----

library(tidyverse)
library(googlesheets4)

#Import data ----
#perhaps just want to refresh the dataset from live version only occasionally

# SS<-"https://docs.google.com/spreadsheets/d/1Y0Odg0ar_YsraYrcQ4r-oJJzSDpIK97SkLBv0yhRMWk/edit#gid=1796417297"
# data_import<-googlesheets4::read_sheet(SS, sheet = "full_dataset")
# fix_data_import <-data_import %>%
# mutate(across(where(is.list),as.character))
# write_csv(fix_data_import, "./output/checked_data_30Jan2021.csv")

data_import<-read_csv("./output/checked_data_30Jan2021.csv") %>%
  select(-Thumbnail)

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

table_main<-data_import3 %>%
  select(-contains("designer"), -contains("artist"), -contains("category"), -contains("mechanic"),
         -contains("family"), -contains("publisher"), -contains("subdomain"))
# TO DO: clean "expansion to / alternative to" field to refer to ID var.

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
get_unique_attribute(attribute="subdomain") #8

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


table_artist<-create_table(attribute="artist")
table_category<-create_table(attribute="category")
table_designer<-create_table(attribute="designer")
table_family<-create_table(attribute="family")
table_mechanic<-create_table(attribute="mechanic")
table_publisher<-create_table(attribute="publisher")
table_subdomain<-create_table(attribute="subdomain")



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
get_top_ranked(attribute="subdomain")

#function to return relevant stats to a board game;
get_attributes<-function(bg_name=""){
bg_table<-table_main %>%
  filter(str_to_lower(Name)==str_to_lower(as.character(bg_name))) 

game_ID<-bg_table$ID

game_artists<-table_artist %>%
  filter(ID==game_ID)

game_designers<-table_designer %>%
  filter(ID==game_ID)

game_categories<- table_category %>%
  filter(ID==game_ID)

game_familys<- table_family%>%
  filter(ID==game_ID)
  


  return(list(bg_table, game_artists, game_familys, game_designers, game_categories))
}
apples_to_apples<-get_attributes(bg_name="Apples to apples")
