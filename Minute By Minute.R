###Load Package
library(tidyverse)
library(magrittr)
library(data.table)

###file load
read_plus <- function(flnm) {
  fread(flnm, skip = 8) %>% 
    mutate(filename=gsub(" .csv", "", basename(flnm))) %>%
    separate(filename, c('Match', 'z2', 'z3', 'z4', 'z5', 'z6')," ") %>%
    mutate(Name = paste(z4, z5)) %>%
    select(c(2,3,13,19))
}

###Create variables for distance calc in Minute By Minute
Variable_create2 <- function(newdf, old_df){
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
           Dist_SD = SpeedSD*Time_diff,
           one_Min = cut(Seconds, breaks = seq(-1, 10800, by = 60))) %>%
    ungroup() %>%
    select(c(3,4, 8:11))
}

############File Path
C_A <- list.files(path="Add folder path here", 
                  pattern="*.csv", full.names = T) %>%
  map_df(function(x) read_plus(x))

df_1 <- Variable_create2(df_1, C_A)

###Dist Per Min
Min_by_min <- df_1 %>% 
  group_by(Name, Match, one_Min) %>%
  dplyr::summarize(Dist=sum(Dist),
                   HS_Dist=sum(Dist_HS),
                   Dist_Sprint=sum(Dist_SD))

###Creating Minute Number col & filtering excessivel high values
Min_by_min %<>%
  group_by(Name, Match) %>%
  mutate(one_Min = 1,
         one_Min = cumsum(one_Min)) %>%
  filter_at(vars(4:6), any_vars(.<250)) %>%
  mutate_at(vars(4:6), funs(round(.,0)))



