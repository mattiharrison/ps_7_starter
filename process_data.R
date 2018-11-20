library(tidyverse)
library(fs)

midterm_results <- read_csv("mt_2_results.csv") %>% 
  filter(district != "sen",
         district != "gov",
         district != "AL") %>% 
  mutate(total = dem_votes + rep_votes + other_votes) %>% 
  filter(total != 0)

midterm_results$state <- tolower(midterm_results$state)

results <- midterm_results %>% 
  mutate(state_district = paste(state, district))

download.file(url = "https://goo.gl/ZRCBda",
              destfile = "master.zip",
              quiet = TRUE,
              mode = "wb")
unzip("master.zip")
file_delete("master.zip")
all <- dir_ls("2018-live-poll-results-master/data")

z <- map_dfr(all, read_csv, .id = "source")
z_1 <- separate(z, col = source, into = c("year", "live", "poll", "results","master/data/elections", 
                                          "polls", "statedistrictsenate", "round"), sep = "-")
z_2 <- separate(z_1, col = statedistrictsenate, into = c("state", "district"), sep = 2)

z_clean <- z_2 %>% 
  select(state:COAL) %>% 
  filter(district != "sen" |
         district != "gov", 
         round == "3.csv") %>% 
  mutate(state_district = paste(state, district))

all <- full_join(results, z_clean, by= "state_district")%>% 
  select(win_name:state_district, response:race_eth) %>% 
  mutate(race_eth = fct_relevel(race_eth, c("Hispanic", "White", "Black", "Asian", "Other"))) %>% 
  filter(ager != "[DO NOT READ] Don't know/Refused",
         ager != "[DO NOT READ] Refused",
         race_eth != "[DO NOT READ] Don't know/Refused",
         gender != "[DO NOT READ] Don't know/Refused") %>% 
  filter(state_district != "az sen",
         state_district != "fl sen",
         response != 3,
         response != 4, 
         response != 5,
         response != 6)
  
all$ager <- recode(all$ager, "18 to 34" = "18-34", "35 to 49" = "35-49", "50 to 64" = "50-64", 
                   "65 and older" = "65+")


write_rds(all, path = "ps_7/results.rds")
