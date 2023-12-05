#### Functions to load data from nestbox-temp-monitoring shared drive
#### Author: Katherine Lauck
#### Last updated: 4/18/2022


nestbox_drive_get <- function(sheet_name) {
  ### Return tibble of google sheet from shared drive nestbox-temp-monitoring given the quoted name of the google sheet without the file extension.
  ### Also checks for the correct package googledrive version.
  require(tidyverse)
  require(googledrive)
  require(googlesheets4)
  require(rlang)

  if(compareVersion(as.character(packageVersion("googledrive")),"1.9.0.9000") == -1){
    abort("Oopsies, your version of package googledrive is out of date! Update with devtools::install_github(\"tidyverse/googledrive\")")
  }
  drive_get(sheet_name, shared_drive = "nestbox-temp-monitoring") %>%
    read_sheet()
}


gear_in_field <- function() {
  ### Return tibble of gear remaining in field.

  gear <- nestbox_drive_get("equipment_tracking")
  gear_out <- filter(gear, action == "deploy" | action == "collect") %>%
    count(equipment_name) %>%
    filter(n < 2)

  return(filter(gear, equipment_name %in% gear_out$equipment_name))

}

identify_end <- function(checks,start) {
  ### Identify the end of an attempt, given a series of checks and the index of the beginning of the attempt.
  # First end is one of:
  # Skip (-1) after first nestlings
  # 0 nestlings and 0 eggs after first eggs
  # 0 nestlings after first nestlings
  # Decrease and then increase in eggs
  # Decrease in eggs and then no nestlings after 3 checks
  # No nestlings at fifth check of eggs

  for(i in seq(start,nrow(checks))){
    if(checks$Nestlings[i] > 0 &
       identical(checks$Nestlings[i+1],-1)) {
      end <- i + max(which(checks$Nestlings[(i+1):(i+3)] == -1))
      break
    } else if(checks$Nestlings[i] > 0 &
              identical(checks$Nestlings[i+1],0)){
      end <- i + 1
      break
    } else if(checks$Eggs[i] > 0 &
              identical(checks$Eggs[i+1],0) &
              identical(checks$Nestlings[i+1],0)){
      end <- i + 1
      break
    } else if(checks$Eggs[i] > 0 &
              checks$Eggs[i+1] < checks$Eggs[i] &
              any(checks$Eggs[(i+2):(i+4)] > checks$Eggs[i+1],na.rm = TRUE)){
      end <- i + 1
      break
    } else if(checks$Eggs[i] > 0 &
              checks$Eggs[i+1] < checks$Eggs[i] &
              all(identical(checks$Nestlings[(i+2):(i+4)],0),na.rm = TRUE)){
      end <- i + 1
      break
    } else if(checks$Eggs[i] > 0 &
              all(identical(checks$Nestlings[(i+1:i+4)],0),na.rm = TRUE)){
      end <- i + 4
      break
    } else {end <- NA;next}
  }

  return(end)

}

identify_start <- function(checks, end){
  ### Identify the start of an attempt, given a series of checks and the index of the end of the last attempt.
  start <- end + min(which(checks$Eggs[(end + 1):(nrow(checks))] > 0),na.rm = TRUE)
  return(start)
}

collapse <- function(checks,...){
  ### Collapse a set of checks from one nest collected as part of the Nestbox Highway Project into information about each attempted nest with eggs constructed, one attempt per line.

  checks <- arrange(checks,Date) # order checks by date to allow parsing of attempts

  start <- 0
  end <- 0

  while(!is.na(end[length(end)]) & end[length(end)] < length(checks)){
    first_egg <- identify_start(checks,end[length(end)])
    start <- c(start,first_egg)
    if(identical(start[length(start)],Inf)) {end <- c(end,NA);break}
    nest_end <- identify_end(checks,first_egg)
    end <- c(end,nest_end)
  }

  attempt_map <- tibble(attempt = seq(0,length(start)-1),start = start, end = end) %>%
    filter(start != Inf,attempt != 0)

  attempt <- rep(NA,times = nrow(checks))

  if(nrow(attempt_map) == 0) {return(NULL)} else {
    if(is.na(attempt_map$end[nrow(attempt_map)])) {attempt_map$end[nrow(attempt_map)] <- nrow(checks)}
    for(i in attempt_map$attempt){
      attempt[attempt_map$start[i]:attempt_map$end[i]] <- i
    }
  }

  checks <- mutate(checks, attempt = attempt) %>%
    filter(!is.na(attempt)) %>%
    group_by(attempt)

  eggs_unhatched <- function(checks,...){
    if(max(checks$Nestlings) == 0){return(0)} else {
      return(checks$Eggs[which(checks$Nestlings == max(checks$Nestlings))] %>%
               min(na.rm = TRUE)
      )
    }
  }

  eggs_aband <- function(checks,...){
    if(max(checks$Nestlings) == 0){return(checks$Eggs[nrow(checks)])} else {return(0)}
  }

  wing <- tibble(
    age = c(rep(seq(0,18,1),3),0,0),
    species = c(rep(c("WEBL","TRES","ATFL"),each = 19),"skipped","Skipped"),
    wing = c(6,7,8,9,11,13,16,19,23,27,31,35,39,43,47,51,55,59,63,6,6,7,8,10,13,16,20,25,30,35,41,47,52,57,62,66,72,78,8,8,9,12,14,17,21,25,32,38,44,50,55,60,65,70,75,77,79,0,0)
  ) %>% group_by(species)

  nest_age <- function(checks,wing) {
    wing_obs <- if_else(str_detect(checks$Notes,"(WC|wc)") %>% replace_na(FALSE),
                        true = str_extract_all(checks$Notes,"[[:digit:]]+") %>%
                          # str_split(",") %>%
                          map(function(x){if(identical(x,character(0))){NA} else {x}}) %>%
                          map(function(x){if(!is.na(x)){as.numeric(x) %>% mean(na.rm = TRUE)} else {NA}}) %>%
                          map(function(x){replace_na(x,0)}) %>%
                          map(function(x){filter(wing,abs(wing-x)==min(abs(wing-x)))}) %>%
                          map2(.y = checks$Species,.f = function(x,y){filter(x,species == y)}) %>%
                          map(ungroup) %>%
                          map(function(x){select(x,age)}) %>%
                          map(function(x){return(x[1,])}) %>%
                          dplyr::bind_rows() %>%
                          pull(age) %>%
                          as.integer(),
                        false = NA_integer_)
    mutate(checks,age = wing_obs)
  }

  site <- rep(checks$Site[1],nrow(attempt_map))
  box <- rep(checks$Nestbox[1],nrow(attempt_map))
  attempt <- attempt_map$attempt
  species <- rep(checks$Species[1],nrow(attempt_map)) # placeholder; should be correctly associated with each attempt
  male <- rep(NA,nrow(attempt_map))
  female <- rep(NA,nrow(attempt_map))
  first_egg_date <- group_map(checks, ~ (as_date(.x$Date[1]) - .x$Eggs[1])) %>%
    unlist() %>%
    as_date()
  first_egg_certainty <- rep("Estimated",nrow(attempt_map))
  clutch_size <- group_map(checks, ~ max(.x$Eggs)) %>% unlist()
  incubation_begins <- first_egg_date + clutch_size
  eggs_hatched <- group_map(checks, ~ max(.x$Nestlings)) %>% unlist()
  eggs_failed <- group_map(checks, eggs_unhatched) %>% unlist()
  eggs_abandoned <- group_map(checks, eggs_aband) %>% unlist()
  eggs_eaten <- rep(0,nrow(attempt_map))
  eggs_destroyed <- rep(0,nrow(attempt_map)) # placeholder; we may have HOSP this season so may need to use this column more correctly
  egg_unk <- clutch_size - eggs_hatched - eggs_failed - eggs_abandoned - eggs_eaten - eggs_destroyed
  brood_size <- eggs_hatched
  hatch_date <- group_map(checks, ~ (as_date(.x$Date[1]) - .x$Eggs[1] ) ) %>% # placeholder; should be calculated using nest_age
    unlist() %>%
    as_date()


  # add calculations for the rest of the columns

  # return tibble of columns

  return(tibble(#Comments = comments,
    Site = site,
    Nestbox = box,
    `Attempt No.` = attempt,
    Species = species,
    `Male ID` = male,
    `Female ID` = female,
    `First Egg Date` = first_egg_date,
    `First Egg Date Certainty` = first_egg_certainty,
    `Clutch Size` = clutch_size,
    `Incubation Begins Date` = incubation_begins,
    `Eggs Hatched` = eggs_hatched,
    `Eggs Failed-to-Hatch` = eggs_failed,
    `Eggs Abandoned` = eggs_abandoned,
    `Eggs Depredated` = eggs_eaten,
    `Eggs Destroyed` = eggs_destroyed,
    `Egg Unknown Fate` = egg_unk,
    `Brood Size` = brood_size,
    `Hatch Date` = hatch_date
  ))

}

eggs_nestlings_to_numeric <- function(string) {
  strip <- str_match(string,"([0-9]+)|(Skipped)|(skipped)") %>% replace_na("0")
  strip[which(strip %in% c("Skipped","skipped"))] <- "-1"
  return(as.numeric(strip[,1]))
}

get_nest_checks <- function(){
  ### Return tibble of nest checks collapsed into one line per attempt.
  ### This function also calculates lay date and fledge date, when possible, from wing chord according to age chart provided by Nestbox Highway project

  require(lubridate);require(tidyverse);require(DescTools)

  d <- nestbox_drive_get("nest_checks") %>%
    mutate(Eggs = eggs_nestlings_to_numeric(Eggs),Nestlings = eggs_nestlings_to_numeric(Nestlings)) %>%
    group_by(Nestbox)

  return(group_split(d) %>% map(collapse) %>% map_dfr(bind_rows))

}

get_temp_data <- function(dir){

  require(lubridate)
  require(tidyverse)

  ### Return tibble of daily high, daily low, and daily range temperature from HOBO loggers. Other columns included are date, habitat, canopy cover, box, site, and whether the logger is on the inside or outside of the box.

  f <- list.files(dir,pattern = "csv",full.names = TRUE)
  collapse <- function(file_name) {
    # Given relative file name of temperature log from one logger, collapse into tibble with one row for each complete day of temperature data.
    if(read_csv(file_name) %>% problems() %>% nrow() > 100){
      t <- (readLines(file_name) %>% tibble() %>% slice(2:n()) %>% pull() %>%
              I() %>% read_csv(col_names = c("x1","x2","x3")))[,2:3]
    } else {
      t <- read_csv(file_name)[,2:3]
    }
    logger_name <- str_extract(file_name,paste0("(?<=^",dir,"/)[:graph:]+(?=\\s)"))
    names(t) <- c("datetime","temp")
    t$datetime <- mdy_hms(t$datetime, tz = "America/Los_Angeles")
    t$date <- t$datetime %>% date()
    above_40 <- function(d,...){
      ifelse(nrow(filter(d,temp > 40,.preserve = TRUE)) == 0,
             0,
             interval(filter(d,temp > 40,.preserve = TRUE) %>% pull(datetime) %>% min(na.rm = TRUE),
                      filter(d,temp > 40,.preserve = TRUE) %>% pull(datetime) %>% max(na.rm = TRUE)) / dhours(1))
    }
    time_above_40 <- group_map(t %>% group_by(date),above_40)
    sum <- t %>%
      group_by(date) %>%
      summarize(mean = mean(temp,na.rm = TRUE),
                max = quantile(temp,probs = 0.95,na.rm = TRUE),
                min = quantile(temp,probs = 0.05,na.rm = TRUE),
                range = max-min) %>%
      mutate(time_above_40 = unlist(time_above_40),
             box = str_extract(logger_name,"[:graph:]*(?=-[:alpha:])"),
             logger_position = str_extract(logger_name,"(?<=-)[:alpha:]+$"),
             identity = paste(box,logger_position,as.character(date(date[1])),sep = "_"))
  }

  return(map(f,collapse))

}


get_humidity_data <- function(dir){

  require(lubridate)
  require(tidyverse)

  ### Return tibble of daily high, daily low, and daily range temperature from HOBO loggers. Other columns included are date, habitat, canopy cover, box, site, and whether the logger is on the inside or outside of the box.

  f <- list.files(dir,pattern = "csv",full.names = TRUE)
  collapse <- function(file_name) {
    # Given relative file name of temperature log from one logger, collapse into tibble with one row for each complete day of temperature data.
    if(read_csv(file_name) %>% problems() %>% nrow() > 100){
      t <- (readLines(file_name) %>% tibble() %>% slice(2:n()) %>% pull() %>%
              I() %>% read_csv(col_names = c("x1","x2","x3")))[,2:3]
    } else {
      t <- read_csv(file_name)[,2:3]
    }
    logger_name <- str_extract(file_name,paste0("(?<=^",dir,"/)[:graph:]+(?=\\s)"))
    names(t) <- c("datetime","temp")
    t$datetime <- mdy_hms(t$datetime, tz = "America/Los_Angeles")
    t$date <- t$datetime %>% date()
    above_40 <- function(d,...){
      ifelse(nrow(filter(d,temp > 40,.preserve = TRUE)) == 0,
             0,
             interval(filter(d,temp > 40,.preserve = TRUE) %>% pull(datetime) %>% min(na.rm = TRUE),
                      filter(d,temp > 40,.preserve = TRUE) %>% pull(datetime) %>% max(na.rm = TRUE)) / dhours(1))
    }
    time_above_40 <- group_map(t %>% group_by(date),above_40)
    sum <- t %>%
      group_by(date) %>%
      summarize(mean = mean(temp,na.rm = TRUE),
                max = quantile(temp,probs = 0.95,na.rm = TRUE),
                min = quantile(temp,probs = 0.05,na.rm = TRUE),
                range = max-min) %>%
      mutate(time_above_40 = unlist(time_above_40),
             box = str_extract(logger_name,"[:graph:]*(?=-[:alpha:])"),
             logger_position = str_extract(logger_name,"(?<=-)[:alpha:]+$"),
             identity = paste(box,logger_position,as.character(date(date[1])),sep = "_"))
  }

  return(map(f,collapse))

}
