library(Rcpp)

read_plus <- function(flnm) {
  fread(flnm) %>%
    select(c(2,3,5,6))
}

###Create variables for distance calc in Minute By Minute
Variable_create <- function(newdf, old_df){
  newdf <- data.frame(matrix(NA, nrow = nrow(old_df), ncol = ncol(old_df)+7))
  newdf <- old_df %>%
    mutate("SpeedHS" = case_when(`Speed (m/s)` <= 5.5 ~ 0,
                                 `Speed (m/s)` > 5.5 ~ `Speed (m/s)`),
           "SpeedSD" = case_when(`Speed (m/s)` <= 7 ~ 0,
                                 `Speed (m/s)` > 7 ~ `Speed (m/s)`)) %>%
    group_by(`Session Id`, `Player Display Name`) %>%
    mutate(Time_diff = `Elapsed Time`-lag(`Elapsed Time`))%>%
    mutate(Time_diff = case_when(is.na(Time_diff) ~ 0,
                                 Time_diff < 0 ~ 0,
                                 Time_diff > 0.1 ~ 0.1,
                                 T ~ Time_diff),
           Elapsed = ave(Time_diff, FUN=cumsum),
           Dist = `Speed (m/s)`*Time_diff,
           Dist_HS = SpeedHS*Time_diff,
           Dist_SD = SpeedSD*Time_diff,
           one_Min = cut(Elapsed, breaks = seq(-1, 10800, by = 60))) %>%
    ungroup() %>%
    select(c(1,2, 9:12))
}

##mutate
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


##Summarise
summary_func <- function(td,hs,vhs){
  df <- cbind(td, hs, vhs)
  df <- df[,c(1:4, 8,12)]
  names(df) <- c("Session", "Name", "Time Period(Mins)", "M.Min", "HS M.Min", "VHS M.Min")
  df %<>%
    group_by(Session, Name, `Time Period(Mins)`) %>%
    filter_if(is.numeric, any_vars(.<250)) %>%
    mutate_if(is.numeric, funs(round(.,0)))
}

###Rolling Sum function (C++)
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
            }
            ')


