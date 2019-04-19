library(tidyverse)
library(magrittr)
library(data.table)
library(parallel)
library(Rcpp)

###file load --------Alter from mutate for filetypes other than Catapult
read_plus <- function(flnm) {
  data.table::fread(flnm, skip = 8) %>%
    dplyr::mutate(filename=gsub(" .csv", "", basename(flnm))) %>%
    tidyr::separate(filename, c('Match', 'z2', 'z3', 'z4', 'z5', 'z6')," ") %>%
    dplyr::mutate(Name = paste(z4, z5)) %>%
    dplyr::select(c(2:4,13,19))
}

###Read list of catapult files
list_catapult_csv <- function(path = ".", regexp = "[.]csv$", 
                              ignore.case = TRUE, invert = FALSE, ...){
  tor::list_any(path, read_plus, regexp = regexp, ignore.case = ignore.case, 
                invert = invert, ...)
}

###Create variables for distance calc --- Alter speed thresholds for your preference
Variable_create <- function(newdf, old_df){
  newdf <- data.frame(matrix(NA, nrow = nrow(old_df), ncol = ncol(old_df)+7))
  newdf <- old_df %>%
    mutate(Velocity = case_when(Velocity >12 ~ 0,
                                T ~Velocity),
      "SpeedHS" = case_when(Velocity <= 5.5 ~ 0,
                                 Velocity > 5.5 ~ Velocity),
           "SpeedSD" = case_when(Velocity <= 7 ~ 0,
                                 Velocity > 7 ~ Velocity)) %>%
    group_by(Match, Name) %>%
    mutate(Time_diff = Seconds-lag(Seconds),
           Accel = abs(Acceleration),
           Time_diff = case_when(is.na(Time_diff) ~ 0,
                                 Time_diff < 0 ~ 0,
                                 Time_diff > 0.1 ~ 0.1,
                                 T ~ Time_diff),
           Dist = Velocity*Time_diff,
           Dist_HS = SpeedHS*Time_diff,
           Dist_SD = SpeedSD*Time_diff) %>% ungroup() %>%
  select(4,5, 9:12)
}
##################################
### Rolling Sum Function -- DO NOT TOUCH ;) #################
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

##################################
### Rolling Average Function -- DO NOT TOUCH ;) #################
##################################
cppFunction('
            NumericVector run_mean(NumericVector x, int n) {
            
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
            
            
            res = res/n;
            
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
summary_func <- function(td,hs,vhs, Acc){
  df <- cbind(td, hs, vhs, Acc)
  df <- df[,c(1:4, 8,12, 16)]
  names(df) <- c("Match", "Name", "Time Period(Secs)", "M.Min", "HS M.Min", "VHS M.Min", "Avg Acc")
  df %<>%
    group_by(Name, Match, `Time Period(Secs)`) %>%
    filter_if(is.numeric, any_vars(.<250)) %>%
    mutate_at(vars(4:6), funs(round(.,0)))
}

############File Path
C_A <- list_catapult_csv(path="MinXMin2") %>%
                            bind_rows(.)

###########Create Necessary metrics for distances 
df_1 <- Variable_create(df_1, C_A)

#######M.Min --------Alter 1:10 for rolling windows of difference lengths
df_td <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist, x))][]
df_td %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:6)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_if(is.numeric, max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

#######High Speed M.Min --------Alter 1:10 for rolling windows of difference lengths
df_hs <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist_HS, x))][]
df_hs %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:6)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_if(is.numeric, max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

#######Very High Speed M.Min --------Alter 1:10 for rolling windows of difference lengths
df_sd <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_sum_v2(Dist_SD, x))][]
df_sd %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:6)) %>%
  group_by(Name, Match) %>%
  mutate_func(.) %>% 
  dplyr::summarise_if(is.numeric, max) %>%
  gather("Time_Period", "m.min", -Name, -Match)

#######Average Acceleration --------Alter 1:10 for rolling windows of difference lengths
df_Acc <- setDT(df_1, key=c("Match","Name"))[
  ,by=.(Match, Name), paste0("Period_", 1:10)
  := mclapply((1:10)*600, function(x) run_mean(Accel, x))][]
df_Acc %<>%
  filter(complete.cases(.)) %>%
  select(-c(3:6)) %>%
  group_by(Name, Match) %>%
   dplyr::summarise_if(is.numeric, max) %>%
  gather("Time_Period", "Avg Accel", -Name, -Match)


###Create Summary File of all 
summary_file <- summary_func(df_td, df_hs, df_sd, df_Acc)

