# Purpose: Calculate a few statistics about population in US in 2019
# Author: Zoey
# Date created: 8/19
# Date updated: 8/19

# ---- Setup ----
setwd("C:/Users/zoeys/Documents/Thesis/Census_Data/Social")

#load packages
library(readxl)
library(plm)
library(lme4)
library(lmtest)
library(zoo)
library(dplyr)
library(tidyr)
library(sandwich)
library(ggplot2)
library(ggforce)
library(stargazer)
library(broom)
library(forestplot)
library(writexl)
library(stringr)
library(ggcorrplot)
library(car)
library(reshape2)

#load data
df_sj <- read_xlsx("Raw_Data/1.0-communities.xlsx",
                   sheet = "Data")
df_re <- read_xlsx("Raw_Data/Race&Ethnicity/ACSDT5Y2019.B03002-Data.xlsx",
                   sheet = "Sheet1")

# ---- Data Cleaning ----
# rename census tract variable
df_sj <- df_sj %>% 
  rename(GEOID10 = "Census tract 2010 ID")

df_sj <- df_sj %>%  #adding air pollution and asthma incidence
  rename(pct_black = "Percent Black or African American alone",
         pct_indian = "Percent American Indian / Alaska Native",
         pct_asian = "Percent Asian",
         pct_haw = "Percent Native Hawaiian or Pacific",
         pct_2race = "Percent two or more races",
         pct_white = "Percent White",
         pct_hisp = "Percent Hispanic or Latino",
         pct_other = "Percent other races",
         pct_disadv = "Percentage of tract that is disadvantaged by area",
         exp_ag_loss = "Expected agricultural loss rate (Natural Hazards Risk Index)",
         flood_risk = "Share of properties at risk of flood in 30 years",
         fire_risk = "Share of properties at risk of fire in 30 years",
         energy_burd = "Energy burden",
         no_plumb = "Share of homes with no kitchen or indoor plumbing (percent)",
         no_plumb_norm = "Share of homes with no kitchen or indoor plumbing (percentile)",
         waste_water = "Wastewater discharge",
         leak_ust = "Leaky underground storage tanks",
         leak_ust_norm = "Leaky underground storage tanks (percentile)",
         imperv_surface = "Share of the tract's land area that is covered by impervious surface or cropland as a percent",
         imperv_surface_norm = "Share of the tract's land area that is covered by impervious surface or cropland as a percent (percentile)",
         pm2 = "PM2.5 in the air",
         pm2_norm = "PM2.5 in the air (percentile)",
         asthma_norm = "Current asthma among adults aged greater than or equal to 18 years (percentile)",
         life_exp = "Life expectancy (years)",
         med_inc = "Median household income as a percent of area median income",
         pct_unemp = "Unemployment (percent)",
         pov_sub100 = "Percent of individuals < 100% Federal Poverty Line",
         no_hs_deg = "Percent individuals age 25 or over with less than high school degree",
         pct_tribal_area = "Percent of the Census tract that is within Tribal areas",
         population = "Total population")

# Clean Race and Ethnicity dataset
# rename tract id
df_re <- df_re %>% 
  rename(GEOID10 = "GEO_ID")

#fixing geo id format 
df_re$GEOID10 <- substr(df_re$GEOID10,start = 10,stop = nchar(df_re$GEOID10))

#creating race & ethnicity vars of interest
df_re <- df_re %>% 
  mutate(hisp_nw = B03002_012E-B03002_013E,
         ind_hnh = B03002_005E+B03002_015E,
         white_hnh = B03002_003E+B03002_013E) %>% 
  rename(hisp_wnw = "B03002_012E",
         white_nh = "B03002_003E",
         ind_nh = "B03002_005E",
         black_nh = "B03002_004E") %>% 
  select(-NAME)

#merge dfs 
df <- merge(df_sj,df_re,by="GEOID10")

# ---- Quick stats - US 2019 totals & percentages ----
df_percent_race <- df %>% 
  summarise(total_pop = sum(population),
            total_white_nh = sum(white_nh),
            total_black_nh = sum(black_nh),
            total_ind_nh = sum(ind_nh),
            total_hisp = sum(hisp_wnw)) %>% 
  mutate(pct_white = total_white_nh/total_pop,
         pct_black = total_black_nh/total_pop,
         pct_ind = total_ind_nh/total_pop,
         pct_hispanic = total_hisp/total_pop) %>% 
  mutate(total_other = total_pop - (total_white_nh+total_black_nh+total_ind_nh+total_hisp),
         pct_other = total_other/total_pop)
