library(tidyr)
library(zoo)
library(tidyverse)
library(magrittr)
library(data.table)
library(parallel)
library(tictoc)
library(Rcpp)

###file load --------Alter from mutate for filetypes other than Catapult
read_plus <- function(flnm) {
  fread(flnm, skip = 8) %>%
    mutate(filename=gsub(" .csv", "", basename(flnm))) %>%
    separate(filename, c('Match', 'z2', 'z3', 'z4', 'z5', 'z6')," ") %>%
    mutate(Name = paste(z4, z5)) %>%
    select(c(2,3,13,19))
}

###Create variables for distance calc --- Alter spped thresholds for your preference
Variable_create <- function(newdf, old_df){
  newdf <- data.frame(matrix(NA, nrow = nrow(old_df), ncol = ncol(old_df)+7))
  newdf <- old_df %>%
    mutate("SpeedHS" = case_when(Velocity <= 5.5 ~ 0,
                                 Velocity > 5.5 ~ Velocity),
           "SpeedSD" = case_when(Velocity <= 7 ~ 0,
                                 Velocity > 7 ~ Velocity)) %>%
    group_by(Match, Name) %>%
    mutate(Time_diff = Seconds-lag(Seconds))%>%
    mutate(Time_diff = case_when(is.na(Time_diff) ~ 0,
                                 Time_diff < 0 ~ 0,
                                 Time_diff > 0.1 ~ 0.1,
                                 T ~ Time_diff),
           Dist = Velocity*Time_diff,
           Dist_HS = SpeedHS*Time_diff,
           Dist_SD = SpeedSD*Time_diff) %>% ungroup() %>%
    select(c(3,4, 8:10))
}
##################################
### DO NOT TOUCH ;) #################
##################################
cppFunction('
            NumericVector run_sum_v2(NumericVector x, int n) {
            
            int sz = x.size();
            
            NumericVector res(sz);
            
            // sum the values from the beginning of the vector to n 
            res[n-1] = std::accumulate(x.begin(), x.end()-sz+n, 0.0);
            
            // loop through the rest of the vector
            for(int i = n; i < sz; i++) {
            res[i] = res[i-1] + x[i] - x[i-n];
            }
            
            // pad the first n-1 elements with NA
            std::fill(res.begin(), res.end()-sz+n-1, NA_REAL);
            
            return res;
            }')

##Create Periods in M/Min --------Increase/Decrease Based on Rolling Window Preference 
mutate_func <- function(x){
  x %>%
    mutate(`Period_2` = `Period_2` /2,
           `Period_3` = `Period_3`/3,
           `Period_4` = `Period_4`/4,
           `Period_5` = `Period_5`/5,
           `Period_6` = `Period_6`/6,
           `Period_7` = `Period_7`/7,
           `Period_8` = `Period_8`/8,
           `Period_9` = `Period_9`/9,
           `Period_10` = `Period_10`/10
    )
}

##Summarise -------_Alter based on Rolling WIndow Preference
summary_func <- function(summarydf,td,hs,vhs){
  summarydf <- cbind(td, hs, vhs)
  summarydf <- summarydf[,c(1:4, 8,12)]
  names(summarydf) <- c("Match", "Name", "Time Period(Mins)", "M.Min", "HS M.Min", "VHS M.Min")
  summarydf %<>%
    group_by(Name, Match, `Time Period(Mins)`) %>%
    filter_if(is.numeric, any_vars(.<250)) %>%
    mutate_if(is.numeric, funs(round(.,0)))
}

############File Path
C_A <- list.files(path="ADD FILE PATH HERE", 
                  pattern="*.csv", full.names = T) %>%
  map_df(function(x) read_plus(x))

###########Create Necessary metrics for distances 
df_1 <- Variable_create(df_1, C_A)

#######M.Min --------Alter 1:10 for rolling windows of difference lengths
df_td <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist, x))][]
df_td %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:5)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_at(c(3:12), max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

#######High Speed M.Min --------Alter 1:10 for rolling windows of difference lengths
df_hs <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist_HS, x))][]
df_hs %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:5)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_at(c(3:12), max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

#######Very High Speed M.Min --------Alter 1:10 for rolling windows of difference lengths
df_sd <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist_SD, x))][]
df_sd %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:5)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_at(c(3:12), max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

###Create Summary File of all 
summary_file <- summary_func(summary_file, df_td, df_hs, df_sd)
