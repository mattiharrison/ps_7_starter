library(tidyverse)
library(fs)

midterm_results <- read_csv("mt_2_results.csv")


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
         round == "3.csv")

