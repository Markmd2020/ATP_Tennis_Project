#############ATP 2025:Outliers detection######

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
library(purrr)

#Set seed for reproducibility
set.seed(135)

#Read data
atp_2025 <- read.csv("C:/Applied_Stats_Project/Data/2025.csv")

#Initial Summary
str(atp_2025)

#Descriptive statistics
summary(atp_2025)

#Prepare the data
winners_df <-atp_2025 %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("w")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("w"))


#Create win column
winners_df$win_game <- "Yes"

losers_df <- atp_2025 %>%
  dplyr::select(surface,tourney_date,tourney_name, starts_with("l")) %>%
  dplyr::rename_with(~ sub("^[^_]*_", "", .x), starts_with("l"))

losers_df$win_game <- "No"

#Quality checks before stacking both datasets
all(dim(winners_df) ==dim(losers_df))
all(names(winners_df)==names(losers_df))

match_stats <- data.frame(rbind(losers_df,winners_df))
head(match_stats)
match_stats$win_game <- as.factor(match_stats$win_game)

#Exclude missing values
match_stats1 <- na.omit(match_stats)

#Split into categorical and numerical variables
atp_2025_cat_vars <- match_stats1 %>% dplyr::select(where(~ is.character(.x) || is.factor(.x)))
atp_2025_num_vars   <- match_stats1 %>% dplyr::select(where(is.numeric))

#Explore variable names
colnames(atp_2025_cat_vars)
colnames(atp_2025_num_vars)

#Retrieve Hampel Filter Outlier function
source("C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project/Hampel_Outlier_Detection.R")

#Explore age and height
summary(match_stats$age)
summary(match_stats$ht)

#The method will be applied for numerical variables and will be focused on match stats
match_metrics <-setdiff(colnames(atp_2025_num_vars),
                        c("tourney_date","seed","rank","rank_points","age","ht"))

#Exclude missing from outliers calculation
match_stats2 <- atp_2025_num_vars[,match_metrics]

#Apply Hampel Outlier function
colSums(map_df(match_stats2,hampel_outlier_detection))/nrow(match_stats2)

#Summary of values to check if the outliers look reasonable
summary(match_stats2)

#Investigate if ranking and match outcome is related to outliers
outlier_df <-data.frame(cbind(match_stats1[,c("name","win_game","rank")],
                 map_df(match_stats2,hampel_outlier_detection)))
head(outlier_df)

outlier_summary_df <- outlier_df%>%
      group_by(win_game)%>%
      dplyr::summarise(across(all_of(match_metrics),~mean(.x,na.rm = TRUE),
                              .names="outlier_pct_{.col}")) %>%
  pivot_longer(
    cols = starts_with("outlier_pct_"),
    names_to = "metric",
    values_to = "mean_value"
  )

#Data Visualisation 
ggplot(outlier_summary_df, aes(x = metric, y = round(mean_value,2), fill = win_game)) +
  geom_col(position = "dodge") +
  labs(
    title = "Mean Outlier Percentages by Match Metric",
    x = "Metric",
    y = "Outlier Percentage",
    fill = "Win Game"
  ) +
  scale_y_continuous(
    limits = c(0, 0.1),
    labels = scales::percent
  )
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )




