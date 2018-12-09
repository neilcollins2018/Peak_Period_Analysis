# Catapult Data Analysis
R-Script for peak period extraction from Catapult GPS files and producing a minute by minute breakdown, designed for large file number and fast analysis 

## Prerequisites

Packages required
```
library(tidyverse)
library(magrittr)
library(data.table)
library(parallel)
library(Rcpp)

```

## Custom Functions

### File Load Function
The first function is for the file load. The `data.table::fread` function uses `skip=8` to skip the first 8 lines which prevent the file loading in the correct format. 
The following lines create a column from the original filename, then pull it apart and create columns for `Match` and `Name`.
This works as long as your filename is in the following format: Session Export for FirstName LastName 12345.csv
If your session name has more than  single word in it, i.e Defence Session, then join the words with a hyphen or underscore or alter the `tidyr::separate` line. Same goes for players with double barrell surnames.

```
read_plus <- function(flnm) {
  fread(flnm, skip = 8) %>%
    mutate(filename=gsub(" .csv", "", basename(flnm))) %>%
    separate(filename, c('Match', 'z2', 'z3', 'z4', 'z5', 'z6')," ") %>%
    mutate(Name = paste(z4, z5)) %>%
    select(c(2,3,13,19))
}
```
### Function To Create Different Distance Metrics
Creating the dataframe before adding to it led to speed improvements here. 

`dplyr::case_when` takes the velocity and creates new columns where velocity is changed to zero if under the selcted speed threshold. These metrics can be changed for different thresholds.
If you want to use relative thresholds, it would involve having a separate dataframe for the threshold, joining through a `dplyr::left_join` and then having the columns instead of speed values in the `case_when`

The time difference between measurements is then calculated with an error corrrection included where negative times, `NA` values or time over 0.1 seconds are corrected to prevent irregular readings.
Distances per measurement are calculated using Velocity*Time=Distance

Any unnecessary columns are removed 

Acceleration script has slight difference where the absolute values are found

```

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

```

### Rolling Sum Function
Rolling sum function created through C++ and the `Rcpp` package
This method led to considerable speed improvements over other methods

Acceleration script has very rollinf average function

```
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
```
### Create Meters Per Minute Values
Grouped multiple `dplyr::mutate` for efficency. 
If different window lengths used, this will need to be changed. 

```
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

```
### Create Summary Dataframe

Combines the dataframes for each speed threshold through `cbind`, renames and then carried out a quick filter for any irregular values.
This filtering will depend on your sport, here it is set to filter out any values over 250m/min.
It then rounds to zero decimal places, again this can be altered if perferred

Acceleration script also includes acceleration here. 

```
summary_func <- function(summarydf,td,hs,vhs){
  summarydf <- cbind(td, hs, vhs)
  summarydf <- summarydf[,c(1:4, 8,12)]
  names(summarydf) <- c("Match", "Name", "Time Period(Mins)", "M.Min", "HS M.Min", "VHS M.Min")
  summarydf %<>%
    group_by(Name, Match, `Time Period(Mins)`) %>%
    filter_if(is.numeric, any_vars(.<250)) %>%
    mutate_if(is.numeric, funs(round(.,0)))
}
```
### Load FIles
This creates a list of the `.csv` files within the folder referenced and then builds a dataframe of them using `purrr:map_df` and the `read_plus` function created earlier. 
`read_plus` can be changed to `data.table::fread` or `readr::read_csv` if preferred. I would advise against `read.csv` as it can take considerably longer.

```
C_A <- list.files(path="ADD/FILE/PATH/HERE", 
                  pattern="*.csv", full.names = T) %>%
  map_df(function(x) read_plus(x))
```
### Using function to create distance metrics

```
df_1 <- Variable_create(df_1, C_A)
```
### Creating Rolling Sums
This is the part of the script that can be most time consuming. 
Using `data.table` and `DTkey` led to minor speed improvements. 
`parallel::mclapply` allows use of multiple processor cores instead of a single core which the majority of R uses. 
The `run_sum_v2` function thorugh `Rcpp` was quicker than any other rollling sum functions trialled. 

The `(1:10)*600` creates the multiple window lengths. Alter here for windows of different lengths.
Alter `paste0("Period_", 1:10)` to name rolling sum columns differently. 

`dplyr` works thorugh resulting dataframe to remove `NA` values with `complete.cases()`, unnecessary columns are removed then values changed to meters per minute through the function created earlier. `dplyr::summarise_at` extracts the peak period data for each window length for each player from each session/match in the dataset. `dplyr::gather` changes the data from wide to long. This allows for easier plotting or further analysis. If exporting to Excel, removing the `gather` and altering the `summary_func` may be easier. 

This step is repeated for each distance metric. 

```
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

```
### Create Summary File of all 

Binds dataframes for different speed thresholds and creates final dataframe. 

```

summary_file <- summary_func(summary_file, df_td, df_hs, df_sd)

```
