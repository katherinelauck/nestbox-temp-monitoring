#### Build growth data for SEM modelling

# Author: Katherine Lauck
# Last updated: 14 Nov 2023

require(tidyverse)
require(lubridate)

### todo
# 1. Find out how many attempts have more than three measurements, and if those measurements are legit, and if so, how to incorporate them into the growth function
# 2. Line 85 group_by(Nestbox,color,year) %>% might end up grouping second attempts together? And then if the limit is three rows of data to calculate growth, those nests might end up excluded. May need to code a way to tell apart first and second attempts.
# 3. Line 123 filter(!is.infinite(gweight)) %>% filters out infinite gweights, but need to figure out why they're happening and make sure I'm not throwing away data
# 4. Remove line 140 !(year == 2023)) when 2023 data is usable

# pull data from google drive (not yet fully proofed)
g <- read_rds("data/g.rds")
t <- read_rds("data/temp.rds")
cc <- read_rds("data/cc.rds")
h <- read_rds("data/humidity.rds")

# function to calculate growth by matching color and nestbox

growth <- function(d,...) {
  out <- d %>% select(year,Nestbox,color,date,Species, # select relevant columns
                      `Weight (g)`,
                      `Right Wing Chord (mm)`,
                      `Tail Length (mm)`,
                      `Skull Length (mm)`,
                      `Nares-Tip (mm)`,
                      `Tarsus (mm)`) %>%
    arrange(date) %>% # order by date
    mutate(weight = as.character(`Weight (g)`) %>% as.numeric(), # make all growth columns numeric
           wing = as.character(`Right Wing Chord (mm)`) %>% as.numeric(),
           tail = as.character(`Tail Length (mm)`) %>% as.numeric(),
           skull = as.character(`Skull Length (mm)`) %>% as.numeric(),
           bill = as.character(`Nares-Tip (mm)`) %>% as.numeric(),
           tarsus = as.character(`Tarsus (mm)`) %>% as.numeric())
  if(nrow(out)==2){ # if we have more than one measurement of chicks per nest, meaning we are able to calculate growth, then:
    out <- add_column(out, # calculate growth in grams or millimeters per day (so a standardized rate)
                      gweight = c(NA,(out$weight[2]-out$weight[1])/as.numeric(out$date[2]-out$date[1])),
                      gwing = c(NA,(out$wing[2]-out$wing[1])/as.numeric(out$date[2]-out$date[1])),
                      gtail = c(NA,(out$tail[2]-out$tail[1])/as.numeric(out$date[2]-out$date[1])),
                      gskull = c(NA,(out$skull[2]-out$skull[1])/as.numeric(out$date[2]-out$date[1])),
                      gbill = c(NA,(out$bill[2]-out$bill[1])/as.numeric(out$date[2]-out$date[1])),
                      gtarsus = c(NA,(out$tarsus[2]-out$tarsus[1])/as.numeric(out$date[2]-out$date[1])))
  } else if(nrow(out)==3){ # if we have three measurements, calculate two growth intervals per day
    out <- add_column(out,
                      gweight = c(NA,(out$weight[2]-out$weight[1])/as.numeric(out$date[2]-out$date[1]),
                                  (out$weight[3]-out$weight[2])/as.numeric(out$date[3]-out$date[2])),
                      gwing = c(NA,(out$wing[2]-out$wing[1])/as.numeric(out$date[2]-out$date[1]),
                                (out$wing[3]-out$wing[2])/as.numeric(out$date[3]-out$date[2])),
                      gtail = c(NA,(out$tail[2]-out$tail[1])/as.numeric(out$date[2]-out$date[1]),
                                (out$tail[3]-out$tail[2])/as.numeric(out$date[3]-out$date[2])),
                      gskull = c(NA,(out$skull[2]-out$skull[1])/as.numeric(out$date[2]-out$date[1]),
                                 (out$skull[3]-out$skull[2])/as.numeric(out$date[3]-out$date[2])),
                      gbill = c(NA,(out$bill[2]-out$bill[1])/as.numeric(out$date[2]-out$date[1]),
                                (out$bill[3]-out$bill[2])/as.numeric(out$date[3]-out$date[2])),
                      gtarsus = c(NA,(out$tarsus[2]-out$tarsus[1])/as.numeric(out$date[2]-out$date[1]),
                                  (out$tarsus[3]-out$tarsus[2])/as.numeric(out$date[3]-out$date[2])))
  } else{ # otherwise, growth can't be calculated.
    out <- add_column(out,
                      gweight = NA,
                      gwing = NA,
                      gtail = NA,
                      gskull = NA,
                      gbill = NA,
                      gtarsus = NA)
  }
}

gr <- g %>%
  mutate(blood_num = ifelse(str_detect(Comments,"B([:digit:]){3}"), # is there a blood number? if so,
                            str_extract(Comments,"B( )?([:digit:]){3}"), # extract it
                            `Blood number`), # if not, use the listed blood number, which may be NA
         color = Comments %>% str_to_lower() %>%
           str_extract("(blue|green|red|yellow|orange|pink|purple)2?"), # Extract color from comments
         year = year(`Banding Date`), # Extract year
         date = as_date(`Banding Date`)) %>% # Extract date
  arrange(year,Nestbox,color,date) %>% # Order by year, nestbox, color, date
  group_by(Nestbox,color,year) %>% # group by nestbox, color, year. This might actually exclude second attempts!
  group_map(growth,.keep = TRUE) %>% # map growth over each group. See todo.
  list_rbind() %>% # bind into tibble
  filter(!is.na(gweight),!is.na(color)) # drop NAs

wing <- tibble( # recreate wing to age chart from Museum of Fish and Wildlife Biology
  age = c(rep(seq(0,18,1),3),0,0),
  species = c(rep(c("WEBL","TRES","ATFL"),each = 19),"skipped","Skipped"),
  wing = c(6,7,8,9,11,13,16,19,23,27,31,35,39,43,47,51,55,59,63,6,6,7,8,10,13,16,20,25,30,35,41,47,52,57,62,66,72,78,8,8,9,12,14,17,21,25,32,38,44,50,55,60,65,70,75,77,79,0,0)
) %>% group_by(species)

dg <- gr %>%
  mutate(site = factor(str_extract(Nestbox,"([:alpha:]|[:punct:])+")), # extract site from nestbox
         habitat = fct_collapse(site, # collapse sites into habitat classes
                                Orchard = c("BRO","MCE","PICO","MCO","FBFO"),
                                Grassland = c("MBNG","RRRG","PICG","FBFG"),
                                `Row crop` = c("MBNR","PICR","RRRR","FBFR"),
                                Forest = c("MBNC","PCC","PCT","RRC","RRE","SFP","FBF","RRRC")),
         habitat = factor(habitat,levels = c("Forest","Orchard","Grassland","Row crop")), # order factor correctly
         interval = interval(date-ddays(7),date,tz = "America/Los_Angeles"), # specify interval to extract temp measurements
         juliandate = yday(date)) %>% # extract julian date
  left_join(cc,by = join_by(year == year,Nestbox == nestbox),multiple = "first") # join canopy cover measurements

filtertemp <- map2(pull(dg,interval),pull(dg,Nestbox),function(x,y){filter(t,date %within% x,box == y)}) # filter temp data to the applicable dates and boxes
filterh <- map2(pull(dg,interval),pull(dg,Nestbox),function(x,y){filter(h,date %within% x,box == y)})

dg <- dg %>%
  mutate(meanmaxtempI = filtertemp %>% # calculate mean max internal temp
           map_dbl(function(x){filter(x,logger_position == "I") %>% pull(max) %>% mean()}),
         maxmaxtempI = filtertemp %>% # max max internal temp
           map_dbl(function(x){filter(x,logger_position == "I") %>% pull(max) %>% max(na.rm = TRUE)}),
         cumulativeover40I = filtertemp %>% # time over 40 degrees C internal
           map_dbl(function(x){filter(x,logger_position == "I") %>% pull(time_above_40) %>% sum()}),
         meanmaxtempO = filtertemp %>% # mean max external temp
           map_dbl(function(x){filter(x,logger_position == "O") %>% pull(max) %>% mean()}),
         maxmaxtempO = filtertemp %>% # max max external temp
           map_dbl(function(x){filter(x,logger_position == "O") %>% pull(max) %>% max(na.rm = TRUE)}),
         cumulativeover40O = filtertemp %>% # time over 40 degrees C external
           map_dbl(function(x){filter(x,logger_position == "O") %>% pull(time_above_40) %>% sum}),
         meanh = filterh %>% # max max external temp
           map_dbl(function(x){pull(mean) %>% mean()}),
         meanmaxh = filterh %>% # max max external temp
           map_dbl(function(x){pull(max) %>% mean()}),
         meanminh = filterh %>% # max max external temp
           map_dbl(function(x){pull(min) %>% mean()})) %>%
  # filter(!is.infinite(gweight)) %>% # filter out infinite gweights - need to figure out why this is happening
  mutate(across(c(meanmaxtempI, # scale numeric responses and predictors
                  meanmaxtempO,
                  maxmaxtempI,
                  maxmaxtempO,
                  meanh,
                  maxh,
                  minh,
                  cumulativeover40I,
                  cumulativeover40O,
                  canopy_cover,
                  gweight,
                  gwing,
                  gtail,
                  gtarsus,
                  gbill,
                  gskull,
                  juliandate),
                ~ scale(.x)[,1])) %>%
  filter(!(site %in% c("FBFG","FBFR"))
         # , # remove Full Belly Farm nests
         # !(year == 2023)
         ) %>% # remove 2023 nests, but this line can be dropped soon
  mutate(site = str_replace_all(site,"MCE","MCO"), # Collapse site vectors when there are multiple site abbreviations per actual site
         site = str_replace_all(site,"PCT","PCC"),
         site = str_replace_all(site,"RRE","RRC"),
         site = str_replace_all(site,"SFP","MBNC"),
         site = as_factor(site), # factorize site
         year = as_factor(year), # factorize year
         Species = as_factor(Species)) # factorize species

write_rds(dg,"data/growth.rds")
