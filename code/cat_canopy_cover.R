# concatenate canopy cover measurements

require(tidyverse)
require(lubridate)
source("helper_functions.R")

pix <- list.files("data/canopy-cover-pictures/",pattern = ".*rds$",full.names = TRUE) %>%
  map(read_rds()) %>%
  list_rbind()

cc <- nestbox_drive_get("canopy_cover_picture_log") %>%
  select(date,nestbox,picture_file_name,canopy_cover) %>%
  mutate(date = as_date(date,tz = "America/Los_Angeles"),
         picture_file_name = ifelse(str_detect(picture_file_name,"100"),str_c(picture_file_name,".JPG"),NA),
         picture_file_name = str_replace(picture_file_name,"100-","IMG_"),
         canopy_cover = as.numeric(canopy_cover)) %>%
  left_join(pix,by = c("picture_file_name" = "id")) %>%
  mutate(canopy_cover = ifelse(is.na(canopy_cover),100-DIFN,canopy_cover),
         year = year(date)) %>%
  select(year,nestbox,canopy_cover)
