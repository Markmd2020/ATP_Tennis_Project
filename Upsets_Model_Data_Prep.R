############Upsets Model Data Prep######

#Load libraries
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(reshape2)
library(lubridate)
library(GGally)
library(scales)
library(naniar)
library(MASS)
library(leaps)

#Set seed for reproducibility
set.seed(135)

#Read data from 2022 till 2025
years <- 2022:2025
path <- "C:/Applied_Stats_Project/Data"
for (y in years) {
  assign(paste0("atp_", y),
         read.csv(file.path(path, paste0(y, ".csv"))))
}

#Check that the code worked and dimensions are consistent
head(atp_2022)
head(atp_2023)
head(atp_2024)
head(atp_2025)
dim(atp_2022)
dim(atp_2023)
dim(atp_2024)
dim(atp_2025)

#Combine all datasets together
atp_data <- data.frame(rbind(atp_2022,atp_2023,atp_2024,atp_2025))
head(atp_data)

atp_data_cleaned <- atp_data%>%
  filter((l_1stIn>=l_1stWon) & (w_1stIn>=w_1stWon) & (w_svpt - w_1stIn -w_df >=0) & (l_svpt - l_1stIn -l_df >0)
         & (w_bpFaced>=w_bpSaved) & (l_bpFaced>=l_bpSaved)
         & (w_svpt - w_1stIn -w_df >= w_2ndWon) & (l_svpt - l_1stIn -l_df >l_2ndWon) ) %>% 
  mutate(w_1stRetIn=l_1stIn,
         w_1stRetWon=l_1stIn-l_1stWon,
         w_2ndRetIn=l_svpt - l_1stIn -l_df,
         w_2ndRetWon=w_2ndRetIn-l_2ndWon,
         w_bpIn =l_bpFaced,
         w_bpConverted=l_bpFaced-l_bpSaved,
         l_1stRetIn=w_1stIn,
         l_1stRetWon=w_1stIn-w_1stWon,
         l_2ndRetIn=w_svpt - w_1stIn -w_df,
         l_2ndRetWon=l_2ndRetIn-w_2ndWon,
         l_bpIn =w_bpFaced,
         l_bpConverted=w_bpFaced-w_bpSaved)
summary(atp_data_cleaned)

#Prepare the data
winners_df <-atp_data_cleaned %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))

#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_data_cleaned %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("l")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

losers_df$win_game <- "No"  

#Quality checks before stacking both datasets
all(dim(winners_df) ==dim(losers_df))
all(names(winners_df)==names(losers_df))

match_stats <- data.frame(rbind(losers_df,winners_df))
head(match_stats)
match_stats$win_game <- as.factor(match_stats$win_game)
colnames(match_stats)

match_stats1 <-match_stats %>%
  na.omit() %>%
  mutate(first_point_serve_win_pct=X1stWon/X1stIn,
         second_point_serve_win_pct=X2ndWon/(svpt - X1stIn -df),
         first_ret_win_pct = X1stRetWon/X1stRetIn,
         second_ret_win_pct = X2ndRetWon/X2ndRetIn,
         bp_conversion_pct=bpConverted/bpIn,
         aces_df_ratio=ace/pmax(df,1),
         bp_saved_pct=bpSaved/bpFaced)
head(match_stats1)
summary(match_stats1) 

#Setup tournament date
match_stats1$tourney_date <- ymd(match_stats1$tourney_date)

#US 2025 Data Prep

#Retrieve tourney start data
match_stats1 %>%
  filter(tourney_name=="US Open" & year(tourney_date)==2025)%>%
  summarise(min(tourney_date))

min_tourney_start_date <- as.Date("2025-08-24")

#Retrieve age and height for each player
us_2025_df <- match_stats1 %>%
  filter(tourney_name=="US Open" & year(tourney_date)==2025)%>%
  arrange(name,tourney_date)%>%
  group_by(name)%>%
  slice(1)%>%
  ungroup() %>%
  dplyr::select(name,age,ht,rank)

#Tag IQR statistical summaries for the last year
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/match_stats_iqr_func.R")

us_open_2025_iqr_df <- stats_iqr_func(data=match_stats1,ref_date = min_tourney_start_date,
                                      window_days = 365,bySurface = FALSE)

us_2025_df1 <- us_2025_df %>%
               left_join(us_open_2025_iqr_df,by="name")

test <- stats_avg_func(data=match_stats1,ref_date = min_tourney_start_date,
               window_days = 365,bySurface = FALSE)
any(duplicated(unique(test$name)))
test1 <- stats_iqr_func(data=match_stats1,ref_date = min_tourney_start_date,
                        window_days = 365,bySurface = FALSE)

any(duplicated(unique(test1$name)))


us_open_2025_iqr_df <- data.frame(us_open_2025_iqr_df)
head(us_open_2025_iqr_df)
str(us_open_2025_iqr_df)

us_open_2025_iqr_df$X.name.