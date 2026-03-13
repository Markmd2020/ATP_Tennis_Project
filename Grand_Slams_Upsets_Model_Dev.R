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

