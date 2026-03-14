########Upsets Grand Slams Model Development######

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
library(Information)
library(ggcorrplot)


#Read datasets
aus_open_2023 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/aus_open_2023_df5.rds")
aus_open_2024 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/aus_open_2024_df5.rds")
aus_open_2025 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/aus_open_2025_df5.rds")
french_open_2023 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/french_open_2023_df5.rds")
french_open_2024 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/french_open_2024_df5.rds")
french_open_2025 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/french_open_2025_df5.rds")
wimbledon_2023 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/wimbledon_2023_df5.rds")
wimbledon_2024 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/wimbledon_2024_df5.rds")
wimbledon_2025 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/wimbledon_2025_df5.rds")
us_open_2023 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/us_2023_df5.rds")
us_open_2024 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/us_2024_df5.rds")
us_open_2025 <- readRDS(
  file="C:/Users/MarkM/OneDrive/Documents/ATP_Tennis_Project1/GrandSlams_Datasets/us_2025_df5.rds")

#Join all datasets into a single file
grand_slams_df <- data.frame(rbind(aus_open_2023,aus_open_2024,aus_open_2025,
                        french_open_2023,french_open_2024,french_open_2025,
                        wimbledon_2023,wimbledon_2024,wimbledon_2025,
                        us_open_2023,us_open_2024,us_open_2025))
#Explore datasets
head(grand_slams_df)
str(grand_slams_df)
summary(grand_slams_df) 

#Convert rank bin as character
grand_slams_df$rank_bin <- as.factor(grand_slams_df$rank_bin)

#Explore how target variable varies by rank bin
grand_slams_df %>%
  group_by(rank_bin)%>%
  summarise(avg_bonus_round=mean(bonus_round),
            bin_total=n())

#Refactor categories
grand_slams_df <- grand_slams_df %>%
  mutate(
    rank_bin = case_when(
      rank_bin %in% c("ATP Rank 9-16", "ATP Rank 17-32") ~ "ATP Rank 9-32",
      TRUE ~ rank_bin
    )
  )

#Split the data into train and test datasets with 70/30 split
train_ids <- sample(c("Y","N"),nrow(grand_slams_df),replace=TRUE,prob=c(0.7,0.3))
grand_slams_train <- grand_slams_df[train_ids=="Y",]
grand_slams_test <- grand_slams_df[train_ids=="N",]

#Subset data and calculate IV
excl_vars <- c("name","id","rank","rank_bin")
setdiff(colnames(grand_slams_df),excl_vars)

rank1_8_df <- grand_slams_train[grand_slams_train$rank_bin=="ATP Rank 1-8",
                          setdiff(colnames(grand_slams_df),excl_vars)]

rank9_32_df <- grand_slams_train[grand_slams_train$rank_bin=="ATP Rank 9-32",
                          setdiff(colnames(grand_slams_df),excl_vars)]

rank33_64_df <- grand_slams_train[grand_slams_train$rank_bin=="ATP Rank 33-64",
                              setdiff(colnames(grand_slams_df),excl_vars)]

rank65_128_df <- grand_slams_train[grand_slams_train$rank_bin=="ATP Rank 65-128",
                               setdiff(colnames(grand_slams_df),excl_vars)]

#Calculate IV metrics for the four cohorts of tennis players

#Rank 1-8
rank_1_8_IV <- create_infotables(data=rank1_8_df, y="bonus_round", bins=5, parallel=FALSE)
rank_1_8_IV_df <- data.frame(rank_1_8_IV$Summary)
rank_1_8_top_vars <- rank_1_8_IV_df[rank_1_8_IV_df$IV>0.2,"Variable"] 

#Rank 9-32
rank_9_32_IV <- create_infotables(data=rank9_32_df, y="bonus_round", bins=5, parallel=FALSE)
rank_9_32_IV_df <- data.frame(rank_9_32_IV$Summary)
rank_9_32_top_vars <- rank_9_32_IV_df[rank_9_32_IV_df$IV>0.2,"Variable"] 

#Rank 33-64
rank_33_64_IV <- create_infotables(data=rank33_64_df, y="bonus_round", bins=5, parallel=FALSE)
rank_33_64_IV_df <- data.frame(rank_33_64_IV$Summary)
rank_33_64_top_vars <- rank_33_64_IV_df[rank_33_64_IV_df$IV>0.2,"Variable"] 

#Rank 65-128 
rank_65_128_IV <- create_infotables(data=rank65_128_df, y="bonus_round", bins=5, parallel=FALSE)
rank_65_128_IV_df <- data.frame(rank_65_128_IV$Summary)
rank_33_64_top_vars <- rank_65_128_IV_df[rank_65_128_IV_df$IV>0.2,"Variable"] 

top_vars_intersect <-Reduce(intersect,
                    list(rank_1_8_top_vars,rank_9_32_top_vars,rank_33_64_top_vars,rank_33_64_top_vars))

top_vars_union <-Reduce(union,
                        list(rank_1_8_top_vars,rank_9_32_top_vars,rank_33_64_top_vars,rank_33_64_top_vars))

# Compute correlation matrix 

# Function to add correlation coefficients
cor_matrix <- cor(na.omit(grand_slams_train[,top_vars_intersect]))

# Plot the correlation matrix
ggcorrplot(cor_matrix, method = "circle", type = "lower", 
           lab = TRUE, lab_size = 3, colors = c("red", "white", "blue"))

#Exclude vars with
excl_vars1 <- c("ace_q50","ace_q75","avg_first_point_serve_win_pct",
                "first_point_win_serve_pct_q75","second_point_serve_win_pct_q25")

mod_vars  <- c(setdiff(top_vars_intersect,excl_vars1),"bonus_round","rank_bin")

#Update training and test dataset with core predictors
grand_slams_train1 <- grand_slams_train[,mod_vars]
grand_slams_test1 <- grand_slams_test[,mod_vars]
grand_slams_train1$bonus_round <- as.factor(grand_slams_train1$bonus_round)
grand_slams_train1$rank_bin <- as.factor(grand_slams_train1$rank_bin)
grand_slams_test1$bonus_round <- as.factor(grand_slams_test1$bonus_round)
grand_slams_test1$rank_bin <- as.factor(grand_slams_test1$rank_bin)

#Fit Full Model Intercept Only
full_model_int_only <-  glmer(
  bonus_round ~ avg_first_ret_win_pct + avg_second_point_serve_win_pct + avg_ace
    + avg_second_ret_win_pct + bp_conversion_pct_q75 + avg_bp_conversion_pct +
    first_point_serve_win_pct_q75 + bp_saved_pct_q75 + (1 | rank_bin),
  data = grand_slams_train1,
  family = binomial,
  control = glmerControl(optimizer = "bobyqa")
  
)

#Stepwise AIC selection
options(na.action = "na.fail")

models <- dredge(full_model_int_only, rank = "AICc")
best_model <- get.models(models, 1)[[1]]

cat("\n### Best Model from dredge ###\n")
summary(best_model) 

predict(full_model_int_only,newdata=grand_slams_test1,type="response")

summary(full_model_int_only)



