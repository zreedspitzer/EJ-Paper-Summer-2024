#Purpose: Running model with SJ variables and Lack of Green Space (percentiles)
# (with redefined CRB tracts and race variables)
#Author: Zoey Reed-Spitzer
#Date Created: 4/16
#Date Updated: 5/26 creating final models for EJ thesis chapter (pop percents) and corr matrix

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
df_mi <- read_xlsx("Raw_Data/Mean_Income_2019/ACSST5Y2019.S1902-Data.xlsx",
                   sheet = "Sheet1")
df_pci <- read_xlsx("Raw_Data/PerCap_Income_2019/ACSDT5Y2019.B19301-Data.xlsx",
                    sheet = "Sheet1")
df_are <- read_xlsx("C:/Users/zoeys/Documents/Thesis/Census_Data/Tracts/Combined_Area/Tracts_2010_Geography.xlsx",
                    sheet = "Sheet1")
df_crb <- read_xlsx("Raw_Data/Tracts_50inCRB.xlsx", #changed since 5/16 w/ more restrictive tract choice
                    sheet = "Sheet1")
df_re <- read_xlsx("Raw_Data/Race&Ethnicity/ACSDT5Y2019.B03002-Data.xlsx",
                   sheet = "Sheet1")
df_hh <- read_xlsx("Raw_Data/Households/ACSDT5Y2019.B25001-Data.xlsx",
                   sheet = "Sheet1")

# ---- Data Cleaning ----
# rename census tract variable
df_sj <- df_sj %>% 
  rename(GEOID10 = "Census tract 2010 ID")

# merge df_sj with tracts in CRB
df_sj_crb <- merge(df_crb,df_sj,by="GEOID10")
# 7764 observations (matches number of tracts in CRB)

df_sj_crb <- df_sj_crb %>%  #adding air pollution and asthma incidence
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

df_sj_crb <- replace(df_sj_crb, is.na(df_sj_crb), 0) #replacing NA's with 0

# combining other variables
## Area ##
#max_length <- nchar(max(df_are$GEOID10)) #area 1: Identify the maximum length of GEO_IDs
#df_are$GEOID10 <- str_pad(df_are$GEOID10, width = max_length, side = "left", pad = "0") #area 2: Pad GEO_IDs with leading zeros to match the maximum length

df_are <- df_are %>% 
  select(GEOID10,ALAND10,AWATER10) #re-check what units of area these are (I believe its square meters)

## Income ##
## #fix geo ID format of per cap and med income df's
df_mi$GEO_ID <- substr(df_mi$GEO_ID,start = 10,stop = nchar(df_mi$GEO_ID)) #cutting off beginning numbers
df_pci$GEO_ID <- substr(df_pci$GEO_ID,start = 10,stop = nchar(df_pci$GEO_ID))

#renaming ID variable
df_mi <- df_mi %>% 
  rename(GEOID10 = "GEO_ID") %>% 
  select(GEOID10,Mean_HH_Income)
df_pci <- df_pci %>% 
  rename(GEOID10 = "GEO_ID") %>% 
  select(GEOID10,PerCap_Income)

## Race & Ethnicity ##
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

# cominging df's
#merge with sj df
df_sj_crb_1 <- merge(df_sj_crb,df_are,by="GEOID10") # 7763 obs
df_sj_crb_2 <- merge(df_sj_crb_1,df_mi,by="GEOID10") # 7756 obs
df_sj_crb_3 <- merge(df_sj_crb_2,df_pci,by="GEOID10") # 7756 obs
df_sj_crb_4 <- merge(df_sj_crb_3,df_re,by="GEOID10")
# lost observations from tracts with zero population
test <- anti_join(df_sj_crb,df_sj_crb_4,by="GEOID10")
# these obs have no population but are near "neighbors" who are disadvantaged and have expected building or ag loss

# convert area from square meters to acres (*this is not working I tried all scenarios for what unit green space is but non of them make sense)
df_sj_crb_4 <- df_sj_crb_4 %>% 
  mutate(Total_sqm = ALAND10+AWATER10,
         Total_Acres = Total_sqm*0.000247105, #converting squ meters to acres
         imperv_acres = imperv_surface*0.000247105, #converting squ meters to acres
         imperv_surface_pct = imperv_acres/Total_Acres) #creating percent imperv surface


# ---- Creating pop density variable ----
df_sj_crb_4 <- df_sj_crb_4 %>% 
  mutate(pop_dens = population/Total_Acres)

# ---- Final Data Set ----
#creating three race variables: white, Indian, and people of color
df <- df_sj_crb_4 %>% 
  mutate(pct_RestRace = pct_asian+pct_haw+pct_2race+pct_other) %>% 
  select("GEOID10","County Name","State/Territory",
         "population","Total_Acres","Total_Acres","pct_RestRace","pct_hisp","pct_white","pct_indian", #white assumed to be white regardless of hispanic ethnicity
         "no_hs_deg","imperv_surface","imperv_acres","imperv_surface_pct","imperv_surface_norm","pm2","pm2_norm",
         "exp_ag_loss","flood_risk",
         "fire_risk","energy_burd","no_plumb","no_plumb_norm",
         "leak_ust","leak_ust_norm","pct_tribal_area","Mean_HH_Income","PerCap_Income",
         "pop_dens","hisp_nw","ind_hnh","white_hnh","hisp_wnw",
         "white_nh","ind_nh","black_nh") %>% 
  mutate(Mean_HH_Income = as.numeric(Mean_HH_Income),
         hisp_wnw_pct = hisp_wnw/population,
         ind_nh_pct = ind_nh/population,
         black_nh_pct = black_nh/population,
         white_nh_pct = white_nh/population)

df <- replace(df, is.na(df), 0) #replacing NA's with 0

# df for pop dens
df_pop <- df %>% 
  mutate(scaled_popdens = pop_dens / 1000) %>% 
  select("GEOID10","pop_dens","scaled_popdens")

write_xlsx(df_pop,"pop_dens_df_5-24.xlsx")

# ---- Correcting Green Space Var ----
# Convert numbers to characters and pad with leading zeros
# Use mutate and str_pad to pad the numbers
df <- df %>%
  mutate(imperv_surface_padded = str_pad(imperv_surface, width = 4, pad = "0"),
         imperv_surface_pct = paste0(substr(imperv_surface_padded, 1, 2), ".", substr(imperv_surface_padded, 3, 4)),
         imperv_surface_pct = as.numeric(imperv_surface_pct))

df_40below <- df_40below %>%
  mutate(imperv_surface_padded = str_pad(imperv_surface, width = 4, pad = "0"),
         imperv_surface_pct = paste0(substr(imperv_surface_padded, 1, 2), ".", substr(imperv_surface_padded, 3, 4)),
         imperv_surface_pct = as.numeric(imperv_surface_pct))

df_50above <- df_50above %>%
  mutate(imperv_surface_padded = str_pad(imperv_surface, width = 4, pad = "0"),
         imperv_surface_pct = paste0(substr(imperv_surface_padded, 1, 2), ".", substr(imperv_surface_padded, 3, 4)),
         imperv_surface_pct = as.numeric(imperv_surface_pct))


# ---- Scaling Per Cap Income and Pop Density ----
df <- df %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)
df_40below <- df_40below %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)
df_50above <- df_50above %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)
df_urban <- df_urban %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)
df_rural <- df_rural %>% 
  mutate(scaled_MeanIncome = Mean_HH_Income / 10,000,
         scaled_PopDens = pop_dens / 1000)














# ---- Converting Plumb variable to percent ----
df <- df %>% 
  mutate(no_plumb_pct = 100*no_plumb)


# ---- Summary Statistics ----
sumstats <- df %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb_pct),
            plumb_mean = mean(no_plumb_pct),
            plumb_med = median(no_plumb_pct),
            plumb_max = max(no_plumb_pct),
            plumb_sd = sd(no_plumb_pct),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hisp_min = min(hisp_wnw_pct),
            hisp_mean = mean(hisp_wnw_pct),
            hisp_med = median(hisp_wnw_pct),
            hisp_max = max(hisp_wnw_pct),
            hisp_sd = sd(hisp_wnw_pct),
            ind_min = min(ind_nh_pct),
            ind_mean = mean(ind_nh_pct),
            ind_med = median(ind_nh_pct),
            ind_max = max(ind_nh_pct),
            ind_sd = sd(ind_nh_pct),
            black_min = min(black_nh_pct),
            black_mean = mean(black_nh_pct),
            black_med = median(black_nh_pct),
            black_max = max(black_nh_pct),
            black_sd = sd(black_nh_pct),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income),
            pop_min = min(pop_dens),
            pop_mean = mean(pop_dens),
            pop_med = median(pop_dens),
            pop_max = max(pop_dens),
            pop_sd = sd(pop_dens)) 

sumstats_long <- sumstats %>% #rearranging sumstats to longer format
  pivot_longer(
    cols = everything(), # Pivot all columns
    names_to = c(".value", "variable"), # Separate column names into two parts
    names_pattern = "^(\\w+)_([a-z]+)$" # Regex pattern to capture column names
  ) 

sumstats_long <- sumstats %>%
  pivot_longer(
    cols = everything(),            # Pivot all columns
    names_to = c(".value", "variable"),  # Separate column names into two parts
    names_pattern = "^(\\w+)_(\\w+)$"     # Regex pattern to capture column names
  ) %>% 
  select(-variable)

#exporting to excel
write_xlsx(sumstats_long, path = "Output/sumstats_5-26.xlsx")

# <40 & >50
sumstats <- df_50above %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hisp_min = min(hisp_wnw),
            hisp_mean = mean(hisp_wnw),
            hisp_med = median(hisp_wnw),
            hisp_max = max(hisp_wnw),
            hisp_sd = sd(hisp_wnw),
            ind_min = min(ind_nh),
            ind_mean = mean(ind_nh),
            ind_med = median(ind_nh),
            ind_max = max(ind_nh),
            ind_sd = sd(ind_nh),
            black_min = min(black_nh),
            black_mean = mean(black_nh),
            black_med = median(black_nh),
            black_max = max(black_nh),
            black_sd = sd(black_nh),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income),
            pop_min = min(population),
            pop_mean = mean(population),
            pop_med = median(population),
            pop_max = max(population),
            pop_sd = sd(population)) 

sumstats_long <- sumstats %>% #rearranging sumstats to longer format
  pivot_longer(
    cols = everything(), # Pivot all columns
    names_to = c(".value", "variable"), # Separate column names into two parts
    names_pattern = "^(\\w+)_([a-z]+)$" # Regex pattern to capture column names
  ) 

sumstats_long <- sumstats %>%
  pivot_longer(
    cols = everything(),            # Pivot all columns
    names_to = c(".value", "variable"),  # Separate column names into two parts
    names_pattern = "^(\\w+)_(\\w+)$"     # Regex pattern to capture column names
  ) %>% 
  select(-variable)

#exporting to excel
write_xlsx(sumstats_long, path = "Output/sumstats_50above_5-21.xlsx")

# exploring plumbing variable
df_plumb <- df %>% 
  mutate(plumb_dec = no_plumb/100)

# ---- Correlation Matrix ----
df_corr <- df %>% 
  rename(`Lack Green Space` = "imperv_surface_pct",
         `IHWA` = "no_plumb",
         `Leaky UST` = "leak_ust",
         `DPIA` = "pm2",
         Hispanic = "hisp_wnw_pct",
         `White (Non-Hisp)` = "white_nh_pct",
         `American Indian (Non-Hisp)` = "ind_nh_pct",
         `Black (Non-Hisp)` = "black_nh_pct",
         `Mean Income` = "Mean_HH_Income",
         `No HS Degree` = "no_hs_deg",
         `Population Density` = "pop_dens") %>% 
  select("Hispanic",`American Indian (Non-Hisp)`,`Black (Non-Hisp)`,`White (Non-Hisp)`,
         `Mean Income`,`No HS Degree`,
         `Lack Green Space`,`IHWA`,`Leaky UST`,
         `DPIA`,
         `Population Density`)

#creating correlation matrix (running with no grouping variables) @ tract level
corr <- round(cor(df_corr), 2)
head(corr[, 1:6])

#Visualizing
ggcorrplot(corr,
           lab = TRUE,
           type = "lower",
           outline.color = "white",
           insig = "blank")
# ---- Total CRB Percentages ----
df_percent_ust <- df %>% 
  filter(leak_ust > 0)

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

write_xlsx(df_percent_race,"output/race_percents.xlsx")

# Plumbing percentages (Updated 9/2)
df_hh <- df_hh %>% 
  rename(GEOID10 = "GEO_ID")

#fixing geo id format 
df_hh$GEOID10 <- substr(df_hh$GEOID10,start = 10,stop = nchar(df_hh$GEOID10))

df_plumb <- merge(df,df_hh,by="GEOID10")

df_plumb <- df_plumb %>% 
  mutate(no_plumb_hh = B25001_001E * (no_plumb))

## 7-4 Additions: Creating percentages of nonwhite based on counts, categorizing, and sumstats
df_nw <- df %>% 
  mutate(ct_nw = black_nh+hisp_wnw+ind_nh) %>% 
  mutate(pct_nw = ct_nw/population) %>% 
  mutate(pct_nw = ifelse(is.na(pct_nw), 0, pct_nw)) %>% 
  mutate(nw_categories = case_when(pct_nw <= .11 ~  "0.00 - 0.11",
                                   pct_nw > .11 & pct_nw <= .24 ~ "0.12 - 0.24",
                                   pct_nw > .24 & pct_nw <= .40 ~ "0.25 - 0.40",
                                   pct_nw > .40 & pct_nw <= .62 ~ "0.41 - 0.62",
                                   pct_nw > .62 ~ "0.63 - 1.00"))

df_11 <- df_nw %>% 
  filter(nw_categories == "0.00 - 0.11")
# 939 obs

df_24 <- df_nw %>% 
  filter(nw_categories == "0.12 - 0.24")
# 1754 obs

df_40 <- df_nw %>% 
  filter(nw_categories == "0.25 - 0.40")
# 1488 obs

df_62 <- df_nw %>% 
  filter(nw_categories == "0.41 - 0.62")
# 1377 obs

df_100 <- df_nw %>% 
  filter(nw_categories == "0.63 - 1.00")
# 2198 obs

sumstats_11 <- df_11 %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income))

sumstats_24 <- df_24 %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income))

sumstats_40 <- df_40 %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income))

sumstats_62 <- df_62 %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income))

sumstats_100 <- df_100 %>% 
  summarise(green_min = min(imperv_surface_pct),
            green_mean = mean(imperv_surface_pct),
            green_med = median(imperv_surface_pct),
            green_max = max(imperv_surface_pct),
            green_sd = sd(imperv_surface_pct),
            plumb_min = min(no_plumb),
            plumb_mean = mean(no_plumb),
            plumb_med = median(no_plumb),
            plumb_max = max(no_plumb),
            plumb_sd = sd(no_plumb),
            ust_min = min(leak_ust),
            ust_mean = mean(leak_ust),
            ust_med = median(leak_ust),
            ust_max = max(leak_ust),
            ust_sd = sd(leak_ust),
            pm2_min = min(pm2),
            pm2_mean = mean(pm2),
            pm2_med = median(pm2),
            pm2_max = max(pm2),
            pm2_sd = sd(pm2),
            hs_min = min(no_hs_deg),
            hs_mean = mean(no_hs_deg),
            hs_med = median(no_hs_deg),
            hs_max = max(no_hs_deg),
            hs_sd = sd(no_hs_deg),
            inc_min = min(Mean_HH_Income),
            inc_mean = mean(Mean_HH_Income),
            inc_med = median(Mean_HH_Income),
            inc_max = max(Mean_HH_Income),
            inc_sd = sd(Mean_HH_Income))

write_xlsx(sumstats_11, "nw_sumstats_11.xlsx")
write_xlsx(sumstats_24, "nw_sumstats_24.xlsx")
write_xlsx(sumstats_40, "nw_sumstats_40.xlsx")
write_xlsx(sumstats_62, "nw_sumstats_62.xlsx")
write_xlsx(sumstats_100, "nw_sumstats_100.xlsx")

# for subsets of top and bottom 20% pop density
df_percent_race_top20 <- df_top20 %>% 
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

df_percent_race_bottom20 <- df_bottom20 %>% 
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







# ---- Exploring pop dens ----
# plotting relationship
ggplot(df, aes(x = pop_dense, y = Mean_HH_Income)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Mean Income", 
       x = "Population Density", 
       y = "Mean Household Income") +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme_minimal()

ggplot(df, aes(x = pop_dens, y = imperv_surface_pct)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Lack Green Space", 
       x = "Population Density", 
       y = "Impervious Surface Or Cropland %") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme_minimal()

ggplot(df, aes(x = pop_dens, y = no_plumb)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Incomplete Plumbing", 
       x = "Population Density", 
       y = "Households with Incomplete Plumbing %") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme_minimal()

ggplot(df, aes(x = pop_dens, y = leak_ust)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Leaky USTs", 
       x = "Population Density", 
       y = "Density of Leaky USTs") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme_minimal()

ggplot(df, aes(x = pop_dens, y = pm2)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Air Pollution", 
       x = "Population Density", 
       y = "PM2.5 in the Air") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme_minimal()

ggplot(df, aes(x = pop_dens, y = no_plumb, color = ind_nh)) +
  geom_point() +  # Add points with graduated colors based on American Indian population
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Incomplete Plumbing", 
       x = "Population Density", 
       y = "Households with Incomplete Plumbing %",
       color = "American Indian Population") +  # Label for the color legend
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_color_gradient(low = "blue", high = "red") +  # Graduated color scale from blue to red
  theme_minimal() 

ggplot(df, aes(x = pop_dens, y = pm2, color = ind_nh)) +
  geom_point() +  # Add points with graduated colors based on American Indian population
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Air Pollution", 
       x = "Population Density", 
       y = "PM2.5 in the Air",
       color = "American Indian Population") +  # Label for the color legend
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_color_gradient(low = "blue", high = "red") +  # Graduated color scale from blue to red
  theme_minimal() 

ggplot(df, aes(x = pop_dens, y = imperv_surface_pct, color = ind_nh)) +
  geom_point() +  # Add points with graduated colors based on American Indian population
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Lack Green Space", 
       x = "Population Density", 
       y = "% Impervious Surface or Cropland",
       color = "American Indian Population") +  # Label for the color legend
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_color_gradient(low = "blue", high = "red") +  # Graduated color scale from blue to red
  theme_minimal() 

ggplot(df, aes(x = pop_dens, y = imperv_surface_pct, color = Mean_HH_Income)) +
  geom_point() +  # Add points with graduated colors based on American Indian population
  geom_smooth(method = "lm", se = FALSE, color = "green") +  # Add linear regression line
  labs(title = "Scatter Plot of Pop Density vs Lack Green Space", 
       x = "Population Density", 
       y = "% Impervious Surface or Cropland",
       color = "Mean HH Income") +  # Label for the color legend
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_color_gradient(low = "blue", high = "red") +  # Graduated color scale from blue to red
  theme_minimal() 

ggplot(df, aes(x = imperv_surface_pct, y = Mean_HH_Income)) +
  geom_point(color = "blue") +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add linear regression line
  labs(title = "Scatter Plot of Income vs Lack Green Space", 
       x = "Impervious Surface Or Cropland %", 
       y = "Income") +
  scale_y_continuous(labels = scales::dollar) +
  theme_minimal()


# ---- Exploring further Scatter Plots ----
#creating categorical variable for predominant race group
df <- df %>% 
  mutate(maj_w = ifelse(white_nh_pct >= .5, 1,0),
         maj_h = ifelse(hisp_wnw_pct >= .5,1,0),
         maj_b = ifelse(black_nh_pct >= .5,1,0),
         maj_i = ifelse(ind_nh_pct >= .5, 1,0),
         maj_o = ifelse(white_nh_pct < .5 & hisp_wnw_pct < .5 & black_nh_pct < .5 & ind_nh_pct < .5,1,0)) %>% 
  mutate(maj_dem = case_when(
    maj_w == 1 ~ "majority White",
    maj_h == 1 ~ "majority Hispanic",
    maj_b == 1 ~ "majority Black",
    maj_i == 1 ~ "majority American Indian",
    maj_o == 1 ~ "no majority"))

# Define custom colors for each category level
maj_dem_colors <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9932CC")

# Define quadrant thresholds
x_thresholds <- c(.78, 1.56)  
y_thresholds <- c(10, 50)  

plot1 <- ggplot(df, aes(x = pop_dens, y = imperv_surface_pct, color = factor(maj_dem))) +
  geom_point() +  # Add points
  labs(title = "Scatter Plot of Pop Density vs Lack Green Space",
       subtitle = "Color - majority demographic",
       x = "Population Density", 
       y = "Impervious Surface Or Cropland %") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_color_manual(values = maj_dem_colors) +  # Assign custom colors
  theme_minimal() +
  theme(legend.title = element_text(size = 14, face = "bold"),  # Increase font size and bold legend title
        legend.text = element_text(size = 12)) +  # Increase font size of legend text
  guides(color = guide_legend(title = "Demographic Category"))  # Adjust size of legend symbols))  # Rename legend title for color
print(plot1)

plot2 <- plot1 +
  facet_zoom(xlim = c(0, .78)) +  # Zoom in on the region 0 to .78
  geom_point(aes(size = .45)) +  # Increase dot size in this zoomed-in region
  facet_zoom(xlim = c(.78, 1.56)) # Zoom in on the region 30 to 70
print(plot2)

ggplot(df, aes(x = pop_dens, y = imperv_surface_pct)) +
  geom_point() +  # Add points
  labs(title = "Scatter Plot of Pop Density & Lack Natural/Green Space",
       x = "Population Density", 
       y = "Impervious Surface Or Cropland %") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  theme(plot.title = element_text(color = "#000000", size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),  # Increase font size and bold legend title
        legend.text = element_text(size = 12))  # Increase font size of legend text
 
ggplot(df, aes(x = pop_dens, y = Mean_HH_Income)) +
  geom_point() +  # Add points
  labs(title = "Scatter Plot of Pop Density & Income",
       x = "Population Density", 
       y = "Mean Household Income") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_y_continuous(labels = scales::dollar) +
  theme(plot.title = element_text(color = "#000000", size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),  # Increase font size and bold legend title
        legend.text = element_text(size = 12))  # Increase font size of legend text

ggplot(df, aes(x = pop_dens, y = Mean_HH_Income, color = imperv_surface_pct)) +
  geom_point() +  # Add points with dif colors based on lack of green space
  labs(title = "Scatter Plot of Pop Density & Income",
       x = "Population Density", 
       y = "Mean Household Income",
       color = "Impervious Surface or Cropland %") +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  scale_y_continuous(labels = scales::dollar) +
  scale_color_gradient(low = "green", high = "black") +
  theme(plot.title = element_text(color = "#000000", size = 20, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14, face = "bold"),  # Increase font size and bold legend title
        legend.text = element_text(size = 12))  # Increase font size of legend text



# ---- Histogram of pop density (looking for natural breaks) ----
# Create frequency table
frequency_table <- df %>%
  count(pop_dens, name = "frequency")

# Calculate cumulative distribution
cumulative_table <- frequency_table %>%
  arrange(pop_dens) %>%
  mutate(cumulative_freq = cumsum(frequency),
         cumulative_percent = 100 * cumulative_freq / sum(frequency))

# Plot histogram
ggplot(df, aes(x = pop_dens)) +
  geom_histogram(binwidth = 2, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram of Population Density', x = 'Population Density', y = 'Frequency') +
  theme_minimal()

# Plot cumulative distribution
ggplot(cumulative_table, aes(x = pop_dens, y = cumulative_percent)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 150, by = 10)) +
  labs(title = 'Cumulative Distribution of Population Density', x = 'Population Density', y = 'Cumulative Percentage') +
  theme_minimal()

#now with scaled pop density (scaled to person per 1000 acres)
# Create frequency table
frequency_table <- df %>%
  count(scaled_PopDens, name = "frequency")

# Calculate cumulative distribution
cumulative_table <- frequency_table %>%
  arrange(scaled_PopDens) %>%
  mutate(cumulative_freq = cumsum(frequency),
         cumulative_percent = 100 * cumulative_freq / sum(frequency))

# Plot histogram
ggplot(df, aes(x = scaled_PopDens)) +
  geom_histogram(binwidth = .001, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram of Scaled Population Density', x = 'Population Density (Scaled 1000)', y = 'Frequency') +
  theme_minimal()

# Plot cumulative distribution
ggplot(cumulative_table, aes(x = scaled_PopDens, y = cumulative_percent)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, .15, by = .01)) +
  labs(title = 'Cumulative Distribution of Scaled Population Density', x = 'Population Density (Scaled 1000)', y = 'Cumulative Percentage') +
  theme_minimal()



# ---- Creating subset based on Pop Density ----
# Calculate the 75th percentile of the population density
threshold <- quantile(df$pop_dens, 0.75)

# Filter the data frame to keep only rows with population density in the top 20%
df_top25 <- df %>%
  filter(pop_dens >= threshold)

# Calculate the 30th percentile of the population density
threshold <- quantile(df$pop_dens, 0.30)

# Filter the data frame to keep only rows with population density in the bottom 30%
df_bottom30 <- df %>%
  filter(pop_dens <= threshold)

# subsetting for census tracts with pop dense < 40 and > 50
df_40below <- df %>% 
  filter(pop_dens <= 40)

df_50above <- df %>% 
  filter(pop_dens >= 50)

df_urban <- df %>% 
  filter(pop_dens >= .78)

df_rural <- df %>% 
  filter(pop_dens < .78)

# ---- Creating dummy variable for tracts >.07 pop dens ----
df <- df %>% 
  mutate(
    urban_dum = ifelse(scaled_PopDens >= 0.07, 1, 0),
    urban_med_dum = ifelse(scaled_PopDens >= 0.06 & scaled_PopDens < 0.07, 1, 0),
    urban_low_dum = ifelse(scaled_PopDens >= 0.05 & scaled_PopDens < 0.06, 1, 0),
    suburb_dum = ifelse(scaled_PopDens >= 0.04 & scaled_PopDens < 0.05, 1, 0),
    rural_high_dum = ifelse(scaled_PopDens >= 0.03 & scaled_PopDens < 0.04, 1, 0),
    rural_med_dum = ifelse(scaled_PopDens >= 0.02 & scaled_PopDens < 0.03, 1, 0),
    rural_low_dum = ifelse(scaled_PopDens >= 0.01 & scaled_PopDens < 0.02, 1, 0),
    sparse_dum = ifelse(scaled_PopDens < 0.01, 1, 0)
  )

#creating dummy for bottom 30%
df <- df %>% 
  mutate(rural_dum = ifelse(pop_dens <= 4.8, 1, 0))
df_poor <- df_poor %>% 
  mutate(rural_dum = ifelse(pop_dens <= 4.8, 1, 0))


# ---- Subsetting Mean Income ----
df_income <- df %>% 
  select("GEOID10","Mean_HH_Income")

write_xlsx(df_income, "Income_df_5-25.xlsx")

# ---- Histogram of Mean Income ----
# Create frequency table
frequency_table <- df %>%
  count(Mean_HH_Income, name = "frequency")

# Calculate cumulative distribution
cumulative_table <- frequency_table %>%
  arrange(Mean_HH_Income) %>%
  mutate(cumulative_freq = cumsum(frequency),
         cumulative_percent = 100 * cumulative_freq / sum(frequency))

# Plot histogram
ggplot(df, aes(x = Mean_HH_Income)) +
  geom_histogram(binwidth = 10000, fill = 'blue', color = 'black', alpha = 0.7) +
  labs(title = 'Histogram of Mean Income', x = 'Mean Household Income', y = 'Frequency') +
  theme_minimal()

# Plot cumulative distribution
ggplot(cumulative_table, aes(x = Mean_HH_Income, y = cumulative_percent)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  scale_x_continuous(breaks = seq(0, 500000, by = 25000)) +
  labs(title = 'Cumulative Distribution of Mean Income', x = 'Mean Household Income', y = 'Cumulative Percentage') +
  theme_minimal()


# ---- Creating Income Dummy Poor < 62,500 ----
df <- df %>% 
  mutate(poor_dum = ifelse(Mean_HH_Income <= 62500, 1, 0))

df_top25 <- df_top25 %>% 
  mutate(poor_dum = ifelse(Mean_HH_Income <= 62500, 1, 0))

df_bottom30 <- df_bottom30 %>% 
  mutate(poor_dum = ifelse(Mean_HH_Income <= 62500, 1, 0))


# ---- Creating subset for poor tracts ----
df_poor <- df %>% 
  filter(Mean_HH_Income <= 62500)


# ---- Exploring quantile regression ----
library(pacman)
p_load(dplyr,janitor, ggplot2, quantreg,sjPlot, plotly)

plot(imperv_surface_pct  ~ pop_dens, data = df) 
abline(lm(imperv_surface_pct  ~ pop_dens, data = df), col="blue", lwd = 3) # using mean, standard ols
abline(rq(imperv_surface_pct  ~ pop_dens, tau=0.5, data = df), col="red",  lwd = 4) # using median

#comparing models based on quantiles
ggplot(df, aes(pop_dens, imperv_surface_pct))+
  geom_point()+
  geom_smooth(method = lm, se = F, color = "darkturquoise")+ #linear model
  geom_quantile(color = "red", quantiles = 0.5)+
  geom_quantile(color = "black", alpha = 0.2, #Quantreg based on median
                quantiles = seq(.05, .95, by = 0.05)) #multiple models from the 5th percentile to 95th percentile

#running for different simple models, green space regressed on pop density alone
ols <-  rq(imperv_surface_pct~scaled_PopDens+scaled_MeanIncome, data = df) #Similar to lm()
quant_reg_med  <- rq(imperv_surface_pct~scaled_PopDens+scaled_MeanIncome,tau = 0.5, data = df)
quant_reg_first  <- rq(imperv_surface_pct~scaled_PopDens+scaled_MeanIncome,tau = 0.1, data = df)
quant_reg_last  <- rq(imperv_surface_pct~scaled_PopDens+scaled_MeanIncome,tau = 0.95, data = df)

# plotting coefficients from four different models to compare
plot_models(ols, quant_reg_med, quant_reg_first, quant_reg_last,
            show.values = TRUE,
            m.labels = c("OLS", "Median", "10th percentile",
                         "95th percentile",
                         legend.title = "Model")
)





# ---- Models: Green Space ----
#green <- lm(imperv_surface_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
#            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
#summary(green)

#bptest(green)
#green_se <- coeftest(green, vcov = vcovHC(green, type = "HC3"))
#coeftest(green, vcov = vcovHC(green, type = "HC3"))

#calculating VIF
vif_green <- vif(green)
vif_green

#Non percentile model with percent green space
green <- lm(imperv_surface_pct ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(green)

bptest(green)
green_se <- coeftest(green, vcov = vcovHC(green, type = "HC3"))
coeftest(green, vcov = vcovHC(green, type = "HC3"))

##### Model with urban dummy ####$
green_urb <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + urban_dum + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(green_urb)

bptest(green_urb)
green_urb_se <- coeftest(green_urb, vcov = vcovHC(green_urb, type = "HC3"))
coeftest(green_urb, vcov = vcovHC(green_urb, type = "HC3"))

green_urb1 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + urban_dum + factor(`State/Territory`), data = df) #including state FE
summary(green_urb1)

bptest(green_urb1)
green_urb1_se <- coeftest(green_urb1, vcov = vcovHC(green_urb1, type = "HC3"))
coeftest(green_urb1, vcov = vcovHC(green_urb1, type = "HC3"))

green_popdums <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + urban_dum + urban_med_dum + urban_low_dum 
                    + suburb_dum + rural_high_dum + rural_med_dum + rural_low_dum 
                    + factor(`State/Territory`), data = df) #including state FE
summary(green_popdums)

bptest(green_popdums)
green_popdums_se <- coeftest(green_popdums, vcov = vcovHC(green_popdums, type = "HC3"))
coeftest(green_popdums, vcov = vcovHC(green_popdums, type = "HC3"))


##### Models with < 40 and > 50 pop dens subsets ####$
green_40below <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + factor(`State/Territory`), data = df_40below) #including state FE
summary(green_40below)

bptest(green_40below)
green_40below_se <- coeftest(green_40below, vcov = vcovHC(green_40below, type = "HC3"))
coeftest(green_40below, vcov = vcovHC(green_40below, type = "HC3"))

green_50above_pd <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df_50above) #including state FE
summary(green_50above_pd)

bptest(green_50above_pd)
#green_50above_pd_se <- coeftest(green_50above_pd, vcov = vcovHC(green_50above_pd, type = "HC3"))
#coeftest(green_50above_pd, vcov = vcovHC(green_50above_pd, type = "HC3"))

green_50above <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                       + scaled_MeanIncome + factor(`State/Territory`), data = df_50above) #including state FE
summary(green_50above)

bptest(green_50above)
#green_50above_se <- coeftest(green_50above, vcov = vcovHC(green_50above, type = "HC3"))
#coeftest(green_50above, vcov = vcovHC(green_50above, type = "HC3"))


##### Model with interaction ####$
green_int <- lm(imperv_surface_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome*scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(green_int)

bptest(green_int)
green_int_se <- coeftest(green_int, vcov = vcovHC(green_int, type = "HC3"))
coeftest(green_int, vcov = vcovHC(green_int, type = "HC3"))

## Model with dif combos of explanatory vars
# without pop and hs degree
greenpct1 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + factor(`State/Territory`), data = df) #including state FE
summary(greenpct1)

bptest(greenpct1)
greenpct1_se <- coeftest(greenpct1, vcov = vcovHC(greenpct1, type = "HC3"))
coeftest(greenpct1, vcov = vcovHC(greenpct1, type = "HC3"))

# without pop dens
greenpct2 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + factor(`State/Territory`), data = df) #including state FE
summary(greenpct2)

bptest(greenpct2)
greenpct2_se <- coeftest(greenpct2, vcov = vcovHC(greenpct2, type = "HC3"))
coeftest(greenpct2, vcov = vcovHC(greenpct2, type = "HC3")) 

# without hs degree
greenpct3 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(greenpct3)

bptest(greenpct3)
greenpct3_se <- coeftest(greenpct3, vcov = vcovHC(greenpct3, type = "HC3"))
coeftest(greenpct3, vcov = vcovHC(greenpct3, type = "HC3"))

# ---- Models: Green (Pop Subsets: top 25 & bottom 30 with poor dummy) ----
green_top25 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + factor(`State/Territory`), data = df_top25) #including state FE
summary(green_top25)

bptest(green_top25)
green_top25_se <- coeftest(green_top25, vcov = vcovHC(green_top25, type = "HC3"))
coeftest(green_top25, vcov = vcovHC(green_top25, type = "HC3"))

green_bottom30 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(green_bottom30)

bptest(green_bottom30)
green_bottom30_se <- coeftest(green_bottom30, vcov = vcovHC(green_bottom30, type = "HC3"))
coeftest(green_bottom30, vcov = vcovHC(green_bottom30, type = "HC3"))


##### With Poor Dummy
green_top25_1 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + poor_dum + factor(`State/Territory`), data = df_top25) #including state FE
summary(green_top25_1)

bptest(green_top25_1)
green_top25_1_se <- coeftest(green_top25_1, vcov = vcovHC(green_top25_1, type = "HC3"))
coeftest(green_top25_1, vcov = vcovHC(green_top25_1, type = "HC3"))

green_bottom30_1 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                     + scaled_MeanIncome + poor_dum + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(green_bottom30_1)

bptest(green_bottom30_1)
green_bottom30_1_se <- coeftest(green_bottom30_1, vcov = vcovHC(green_bottom30_1, type = "HC3"))
coeftest(green_bottom30_1, vcov = vcovHC(green_bottom30_1, type = "HC3"))

# ---- Models: Green (Poor subsets with rural dummy) ----
green_poor <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                 + scaled_PopDens + factor(`State/Territory`), data = df_poor) #including state FE
summary(green_poor)

bptest(green_poor)
green_poor_se <- coeftest(green_poor, vcov = vcovHC(green_poor, type = "HC3"))
coeftest(green_poor, vcov = vcovHC(green_poor, type = "HC3"))

green_poor1 <- lm(imperv_surface_pct ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                 + scaled_PopDens + rural_dum + factor(`State/Territory`), data = df_poor) #including state FE
summary(green_poor1)

bptest(green_poor1)
green_poor1_se <- coeftest(green_poor1, vcov = vcovHC(green_poor1, type = "HC3"))
coeftest(green_poor1, vcov = vcovHC(green_poor1, type = "HC3"))
# ---- Models: Lack of Plumbing ----
#plumb <- lm(no_plumb_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
#            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
#summary(plumb)

#bptest(plumb)
#plumb_se <- coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))
#coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))

vif_plumb <- vif(plumb)
vif_plumb

#Non percentile model with percent households incomplete plumb
plumb <- lm(no_plumb ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(plumb)

bptest(plumb)
plumb_se <- coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))
coeftest(plumb, vcov = vcovHC(plumb, type = "HC3"))

##### Models with pop dum ####$
plumb_urb <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + urban_dum + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(plumb_urb)

bptest(plumb_urb)
plumb_urb_se <- coeftest(plumb_urb, vcov = vcovHC(plumb_urb, type = "HC3"))
coeftest(plumb_urb, vcov = vcovHC(plumb_urb, type = "HC3"))

plumb_urb1 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + urban_dum + factor(`State/Territory`), data = df) #including state FE
summary(plumb_urb1)

bptest(plumb_urb1)
plumb_urb1_se <- coeftest(plumb_urb1, vcov = vcovHC(plumb_urb1, type = "HC3"))
coeftest(plumb_urb1, vcov = vcovHC(plumb_urb1, type = "HC3"))

plumb_popdums <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + urban_dum + urban_med_dum + urban_low_dum 
                  + suburb_dum + rural_high_dum + rural_med_dum + rural_low_dum 
                  + factor(`State/Territory`), data = df) #including state FE
summary(plumb_popdums)

bptest(plumb_popdums)
plumb_popdums_se <- coeftest(plumb_popdums, vcov = vcovHC(plumb_popdums, type = "HC3"))
coeftest(plumb_popdums, vcov = vcovHC(plumb_popdums, type = "HC3"))


##### Models with < 40 and > 50 pop dens subsets ####$
plumb_40below <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + factor(`State/Territory`), data = df_40below) #including state FE
summary(plumb_40below)

bptest(plumb_40below)
plumb_40below_se <- coeftest(plumb_40below, vcov = vcovHC(plumb_40below, type = "HC3"))
coeftest(plumb_40below, vcov = vcovHC(plumb_40below, type = "HC3"))

plumb_50above_pd <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                       + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df_50above) #including state FE
summary(plumb_50above_pd)

bptest(plumb_50above_pd)
#plumb_50above_pd_se <- coeftest(plumb_50above_pd, vcov = vcovHC(plumb_50above_pd, type = "HC3"))
#coeftest(plumb_50above_pd, vcov = vcovHC(plumb_50above_pd, type = "HC3"))

plumb_50above <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + factor(`State/Territory`), data = df_50above) #including state FE
summary(plumb_50above)

bptest(plumb_50above)
#plumb_50above_se <- coeftest(plumb_50above, vcov = vcovHC(plumb_50above, type = "HC3"))
#coeftest(plumb_50above, vcov = vcovHC(plumb_50above, type = "HC3"))

##### Models with interaction ####$
plumb_int <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome*scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(plumb_int)

bptest(plumb_int)
plumb_int_se <- coeftest(plumb_int, vcov = vcovHC(plumb_int, type = "HC3"))
coeftest(plumb_int, vcov = vcovHC(plumb_int, type = "HC3"))


## Models with dif exp vars to compare
# without pop and hs degree
plumb1 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
             + scaled_MeanIncome + factor(`State/Territory`), data = df) #including state FE)
summary(plumb1)

bptest(plumb1)
plumb1_se <- coeftest(plumb1, vcov = vcovHC(plumb1, type = "HC3"))
coeftest(plumb1, vcov = vcovHC(plumb1, type = "HC3"))

# without pop
plumb2 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
             + scaled_MeanIncome + factor(`State/Territory`), data = df) #including state FE)
summary(plumb2)

bptest(plumb2)
plumb2_se <- coeftest(plumb2, vcov = vcovHC(plumb2, type = "HC3"))
coeftest(plumb2, vcov = vcovHC(plumb2, type = "HC3"))

# without hs degree
plumb3 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
             + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE)
summary(plumb3)

bptest(plumb3)
plumb3_se <- coeftest(plumb3, vcov = vcovHC(plumb3, type = "HC3"))
coeftest(plumb3, vcov = vcovHC(plumb3, type = "HC3"))

# ---- Models: Lack of Plumbing (Pop Subset: bottom 30 with poor dummy) ----
plumb_bottom30 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(plumb_bottom30)

bptest(plumb_bottom30)
plumb_bottom30_se <- coeftest(plumb_bottom30, vcov = vcovHC(plumb_bottom30, type = "HC3"))
coeftest(plumb_bottom30, vcov = vcovHC(plumb_bottom30, type = "HC3"))

plumb_bottom30_1 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                     + scaled_MeanIncome + poor_dum + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(plumb_bottom30_1)

bptest(plumb_bottom30_1)
plumb_bottom30_1_se <- coeftest(plumb_bottom30_1, vcov = vcovHC(plumb_bottom30_1, type = "HC3"))
coeftest(plumb_bottom30_1, vcov = vcovHC(plumb_bottom30_1, type = "HC3"))


# ---- Models: Lack of Plumbing (Poor subsets with rural dummy) ----
plumb_poor <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                 + scaled_PopDens + factor(`State/Territory`), data = df_poor) #including state FE
summary(plumb_poor)

bptest(plumb_poor)
plumb_poor_se <- coeftest(plumb_poor, vcov = vcovHC(plumb_poor, type = "HC3"))
coeftest(plumb_poor, vcov = vcovHC(plumb_poor, type = "HC3"))

plumb_poor1 <- lm(no_plumb ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                 + scaled_PopDens + rural_dum + factor(`State/Territory`), data = df_poor) #including state FE
summary(plumb_poor1)

bptest(plumb_poor1)
plumb_poor1_se <- coeftest(plumb_poor1, vcov = vcovHC(plumb_poor1, type = "HC3"))
coeftest(plumb_poor1, vcov = vcovHC(plumb_poor1, type = "HC3"))

# ---- Models: Leaky UST ----
#ust <- lm(leak_ust_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
#            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
#summary(ust)

#bptest(ust)
#ust_se <- coeftest(ust, vcov = vcovHC(ust, type = "HC3"))
#coeftest(ust, vcov = vcovHC(ust, type = "HC3"))

vif_ust <- vif(ust)
vif_ust

#Non percentile model with percent leaky USTs
ust <- lm(leak_ust ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(ust)

bptest(ust)
ust_se <- coeftest(ust, vcov = vcovHC(ust, type = "HC3"))
coeftest(ust, vcov = vcovHC(ust, type = "HC3"))

##### Models with urban dummy ####$
ust_urb <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome + urban_dum + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(ust_urb)

bptest(ust_urb)
ust_urb_se <- coeftest(ust_urb, vcov = vcovHC(ust_urb, type = "HC3"))
coeftest(ust_urb, vcov = vcovHC(ust_urb, type = "HC3"))

ust_urb1 <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
              + scaled_MeanIncome + urban_dum + factor(`State/Territory`), data = df) #including state FE
summary(ust_urb1)

bptest(ust_urb1)
ust_urb1_se <- coeftest(ust_urb1, vcov = vcovHC(ust_urb1, type = "HC3"))
coeftest(ust_urb1, vcov = vcovHC(ust_urb1, type = "HC3"))

ust_popdums <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + urban_dum + urban_med_dum + urban_low_dum 
                  + suburb_dum + rural_high_dum + rural_med_dum + rural_low_dum 
                  + factor(`State/Territory`), data = df) #including state FE
summary(ust_popdums)

bptest(ust_popdums)
ust_popdums_se <- coeftest(ust_popdums, vcov = vcovHC(ust_popdums, type = "HC3"))
coeftest(ust_popdums, vcov = vcovHC(ust_popdums, type = "HC3"))


##### Models with < 40 and > 50 pop dens subsets ####$
ust_40below <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + factor(`State/Territory`), data = df_40below) #including state FE
summary(ust_40below)

bptest(ust_40below)
ust_40below_se <- coeftest(ust_40below, vcov = vcovHC(ust_40below, type = "HC3"))
coeftest(ust_40below, vcov = vcovHC(ust_40below, type = "HC3"))

ust_50above_pd <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                       + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df_50above) #including state FE
summary(ust_50above_pd)

bptest(ust_50above_pd)
#ust_50above_pd_se <- coeftest(ust_50above_pd, vcov = vcovHC(ust_50above_pd, type = "HC3"))
#coeftest(ust_50above_pd, vcov = vcovHC(ust_50above_pd, type = "HC3"))

ust_50above <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + factor(`State/Territory`), data = df_50above) #including state FE
summary(ust_50above)

bptest(ust_50above)
#ust_50above_se <- coeftest(ust_50above, vcov = vcovHC(ust_50above, type = "HC3"))
#coeftest(ust_50above, vcov = vcovHC(ust_50above, type = "HC3"))

##### Model with interaction ####$
ust_int <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome*scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(ust_int)

bptest(ust_int)
ust_int_se <- coeftest(ust_int, vcov = vcovHC(ust_int, type = "HC3"))
coeftest(ust_int, vcov = vcovHC(ust_int, type = "HC3"))


## Models with dif exp vars to compare
# without pop and hs degree
ust1 <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
           + scaled_MeanIncome + factor(`State/Territory`), data = df)
summary(ust1)

bptest(ust1)
ust1_se <- coeftest(ust1, vcov = vcovHC(ust1, type = "HC3"))
coeftest(ust1, vcov = vcovHC(ust1, type = "HC3"))

# without pop
ust2 <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
           + scaled_MeanIncome + factor(`State/Territory`), data = df)
summary(ust2)

bptest(ust2)
ust2_se <- coeftest(ust2, vcov = vcovHC(ust2, type = "HC3"))
coeftest(ust2, vcov = vcovHC(ust2, type = "HC3"))

# without hs degree
ust3 <- lm(leak_ust ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
           + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df)
summary(ust3)

bptest(ust3)
ust3_se <- coeftest(ust3, vcov = vcovHC(ust3, type = "HC3"))
coeftest(ust3, vcov = vcovHC(ust3, type = "HC3"))

# ---- Models: Air pollution ----
#pm2 <- lm(pm2_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
#          + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
#summary(pm2)

#bptest(pm2)
#pm2_se <- coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))
#coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))

vif_pm2 <- vif(pm2)
vif_pm2

#Non percentile model with air pollution single value for each tract
pm2 <- lm(pm2 ~ hisp_wnw_pct + ind_nh_pct + black_nh_pct + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(pm2)

bptest(pm2)
pm2_se <- coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))
coeftest(pm2, vcov = vcovHC(pm2, type = "HC3"))

##### Model with urban dummy
pm2_urb <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome + urban_dum + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(pm2_urb)

bptest(pm2_urb)
pm2_urb_se <- coeftest(pm2_urb, vcov = vcovHC(pm2_urb, type = "HC3"))
coeftest(pm2_urb, vcov = vcovHC(pm2_urb, type = "HC3"))

pm2_urb1 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
              + scaled_MeanIncome + urban_dum + factor(`State/Territory`), data = df) #including state FE
summary(pm2_urb1)

bptest(pm2_urb1)
pm2_urb1_se <- coeftest(pm2_urb1, vcov = vcovHC(pm2_urb1, type = "HC3"))
coeftest(pm2_urb1, vcov = vcovHC(pm2_urb1, type = "HC3"))

pm2_popdums <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                    + scaled_MeanIncome + urban_dum + urban_med_dum + urban_low_dum 
                    + suburb_dum + rural_high_dum + rural_med_dum + rural_low_dum 
                    + factor(`State/Territory`), data = df) #including state FE
summary(pm2_popdums)

bptest(pm2_popdums)
pm2_popdums_se <- coeftest(pm2_popdums, vcov = vcovHC(pm2_popdums, type = "HC3"))
coeftest(pm2_popdums, vcov = vcovHC(pm2_popdums, type = "HC3"))


##### Models with < 40 and > 50 pop dens subsets ####$
pm2_40below <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + factor(`State/Territory`), data = df_40below) #including state FE
summary(pm2_40below)

bptest(pm2_40below)
pm2_40below_se <- coeftest(pm2_40below, vcov = vcovHC(pm2_40below, type = "HC3"))
coeftest(pm2_40below, vcov = vcovHC(pm2_40below, type = "HC3"))

pm2_50above_pd <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                     + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df_50above) #including state FE
summary(pm2_50above_pd)

bptest(pm2_50above_pd)
#pm2_50above_pd_se <- coeftest(pm2_50above_pd, vcov = vcovHC(pm2_50above_pd, type = "HC3"))
#coeftest(pm2_50above_pd, vcov = vcovHC(pm2_50above_pd, type = "HC3"))

pm2_50above <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_MeanIncome + factor(`State/Territory`), data = df_50above) #including state FE
summary(pm2_50above)

bptest(pm2_50above)
#pm2_50above_se <- coeftest(pm2_50above, vcov = vcovHC(pm2_50above, type = "HC3"))
#coeftest(pm2_50above, vcov = vcovHC(pm2_50above, type = "HC3"))

##### Model with interaction
pm2_int <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome*scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
summary(pm2_int)

bptest(pm2_int)
pm2_int_se <- coeftest(pm2_int, vcov = vcovHC(pm2_int, type = "HC3"))
coeftest(pm2_int, vcov = vcovHC(pm2_int, type = "HC3"))


## Models with dif exp vars to compare
# without pop and hs degree
pm2_1 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + factor(`State/Territory`), data = df)
summary(pm2_1)

bptest(pm2_1)
pm2_1_se <- coeftest(pm2_1, vcov = vcovHC(pm2_1, type = "HC3"))
coeftest(pm2_1, vcov = vcovHC(pm2_1, type = "HC3"))

# without pop
pm2_2 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + factor(`State/Territory`), data = df)
summary(pm2_2)

bptest(pm2_2)
pm2_2_se <- coeftest(pm2_2, vcov = vcovHC(pm2_2, type = "HC3"))
coeftest(pm2_2, vcov = vcovHC(pm2_2, type = "HC3"))

# without hs degree
pm2_3 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh #ecluding white (Non-Hisp) and other groups
            + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df)
summary(pm2_3)

bptest(pm2_3)
pm2_3_se <- coeftest(pm2_3, vcov = vcovHC(pm2_3, type = "HC3"))
coeftest(pm2_3, vcov = vcovHC(pm2_3, type = "HC3"))


#asthma <- lm(asthma_norm ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
#          + scaled_MeanIncome + scaled_PopDens + factor(`State/Territory`), data = df) #including state FE
#summary(asthma)

# ---- Models: Air pollution (Pop Subsets: top 25 & bottom 30 with poor dummy) ----
pm2_top25 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
          + scaled_MeanIncome + factor(`State/Territory`), data = df_top25) #including state FE
summary(pm2_top25)

bptest(pm2_top25)
pm2_top25_se <- coeftest(pm2_top25, vcov = vcovHC(pm2_top25, type = "HC3"))
coeftest(pm2_top25, vcov = vcovHC(pm2_top25, type = "HC3"))

pm2_top25_1 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + poor_dum + factor(`State/Territory`), data = df_top25) #including state FE
summary(pm2_top25_1)

bptest(pm2_top25_1)
pm2_top25_1_se <- coeftest(pm2_top25_1, vcov = vcovHC(pm2_top25_1, type = "HC3"))
coeftest(pm2_top25_1, vcov = vcovHC(pm2_top25_1, type = "HC3"))

pm2_bottom30 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                + scaled_MeanIncome + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(pm2_bottom30)

bptest(pm2_bottom30)
pm2_bottom30_se <- coeftest(pm2_bottom30, vcov = vcovHC(pm2_bottom30, type = "HC3"))
coeftest(pm2_bottom30, vcov = vcovHC(pm2_bottom30, type = "HC3"))

pm2_bottom30_1 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                   + scaled_MeanIncome + poor_dum + factor(`State/Territory`), data = df_bottom30) #including state FE
summary(pm2_bottom30_1)

bptest(pm2_bottom30_1)
pm2_bottom30_1_se <- coeftest(pm2_bottom30_1, vcov = vcovHC(pm2_bottom30_1, type = "HC3"))
coeftest(pm2_bottom30_1, vcov = vcovHC(pm2_bottom30_1, type = "HC3"))

# ---- Models: Air pollution (Poor subsets with rural dummy) ---- 
pm2_poor <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                 + scaled_PopDens + factor(`State/Territory`), data = df_poor) #including state FE
summary(pm2_poor)

bptest(pm2_poor)
pm2_poor_se <- coeftest(pm2_poor, vcov = vcovHC(pm2_poor, type = "HC3"))
coeftest(pm2_poor, vcov = vcovHC(pm2_poor, type = "HC3"))

pm2_poor1 <- lm(pm2 ~ hisp_wnw + ind_nh + black_nh + no_hs_deg #ecluding white (Non-Hisp) and other groups
                  + scaled_PopDens + rural_dum + factor(`State/Territory`), data = df_poor) #including state FE
summary(pm2_poor1)

bptest(pm2_poor1)
pm2_poor1_se <- coeftest(pm2_poor1, vcov = vcovHC(pm2_poor1, type = "HC3"))
coeftest(pm2_poor1, vcov = vcovHC(pm2_poor1, type = "HC3"))


# ---- Plot of models ----
#Green & pm2 rural subset comparisons (no poor dummy)
plot_models(green_top25, green_bottom30,
            show.values = TRUE,
            m.labels = c("Lack Green Space (Pop Dens Top 25%)", "Lack Green Space (Pop Dens Bottom 30%)",
                         legend.title = "Model"),
            rm.terms = c("factor(State/Territory)California", "factor(State/Territory)Colorado",
                         "factor(State/Territory)Nevada","factor(State/Territory)New Mexico",
                         "factor(State/Territory)Utah","factor(State/Territory)Wyoming"),
            title = "Lack Green Space - Pop Dens Subsets",
            axis.lim = c(-2,8)
)

#Green rural subset comparisons (with poor dummy)
plot_models(green_top25_1, green_bottom30_1,
            show.values = TRUE,
            m.labels = c("Lack Green Space (Pop Dens Top 25%)", "Lack Green Space (Pop Dens Bottom 30%)",
                         legend.title = "Model"),
            rm.terms = c("factor(State/Territory)California", "factor(State/Territory)Colorado",
                         "factor(State/Territory)Nevada","factor(State/Territory)New Mexico",
                         "factor(State/Territory)Utah","factor(State/Territory)Wyoming"),
            title = "Lack Green Space - Pop Dens Subsets (with Poor Dummy)",
            axis.lim = c(-2,8)
)
# ---- Visualizing Models ----
tidy_green <- tidy(green)
write_xlsx(tidy_green, path = "Output/green_model_4-16.xlsx")

tidy_plumb <- tidy(plumb)
write_xlsx(tidy_plumb, path = "Output/plumb_model_4-16.xlsx")

tidy_ust <- tidy(ust)
write_xlsx(tidy_ust, path = "Output/ust_model_4-16.xlsx")

tidy_pm2 <- tidy(pm2)
write_xlsx(tidy_pm2, path = "Output/pm2_model_4-16.xlsx")

tidy_asthma <- tidy(asthma)
write_xlsx(tidy_asthma, path = "Output/asthma_model_4-16.xlsx")

tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_4-21.xlsx")

tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_4-21.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_4-21.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_4-21.xlsx")

tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_4-29.xlsx")

tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_4-29.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_4-29.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_4-29.xlsx")

#combos 5-1
tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_5-1.xlsx")

tidy_plumb_se <- tidy(plumb1_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model1_5-1.xlsx")

tidy_plumb_se <- tidy(plumb2_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model2_5-1.xlsx")

tidy_plumb_se <- tidy(plumb3_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model3_5-1.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_5-1.xlsx")

tidy_ust_se <- tidy(ust1_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model1_5-1.xlsx")

tidy_ust_se <- tidy(ust2_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model2_5-1.xlsx")

tidy_ust_se <- tidy(ust3_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model3_5-1.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_5-1.xlsx")

tidy_pm2_se <- tidy(pm2_1_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model1_5-1.xlsx")

tidy_pm2_se <- tidy(pm2_2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model2_5-1.xlsx")

tidy_pm2_se <- tidy(pm2_3_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model3_5-1.xlsx")

tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_5-1.xlsx")

# Models 5-17
tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_5-17.xlsx")

tidy_plumb_se <- tidy(plumb_int_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_int_model_5-17.xlsx")

tidy_plumb_se <- tidy(plumb_top20)
write_xlsx(tidy_plumb_se, path = "Output/plumb_top20_model_5-17.xlsx")

tidy_plumb_se <- tidy(plumb_bottom20_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_bottom20_model_5-17.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_5-17.xlsx")

tidy_ust_se <- tidy(ust_int_se)
write_xlsx(tidy_ust_se, path = "Output/ust_int_model_5-17.xlsx")

tidy_ust_se <- tidy(ust_top20)
write_xlsx(tidy_ust_se, path = "Output/ust_top20_model_5-17.xlsx")

tidy_ust_se <- tidy(ust_bottom20_se)
write_xlsx(tidy_ust_se, path = "Output/ust_bottom20_model_5-17.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_5-17.xlsx")

tidy_pm2_se <- tidy(pm2_int_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_int_model_5-17.xlsx")

tidy_pm2_se <- tidy(pm2_top20)
write_xlsx(tidy_pm2_se, path = "Output/pm2_top20_model_5-17.xlsx")

tidy_pm2_se <- tidy(pm2_bottom20_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_bottom20_model_5-17.xlsx")

tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_5-17.xlsx")

tidy_green_se <- tidy(green_int_se)
write_xlsx(tidy_green_se, path = "Output/green_int_model_5-17.xlsx")

tidy_green_se <- tidy(green_top20)
write_xlsx(tidy_green_se, path = "Output/green_top20_model_5-17.xlsx")

tidy_green_se <- tidy(green_bottom20_se)
write_xlsx(tidy_green_se, path = "Output/green_bottom20_model_5-17.xlsx")

# Models 5-21
tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_5-21.xlsx")

tidy_plumb_se <- tidy(plumb_40below_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_40below_5-21.xlsx")

tidy_plumb_se <- tidy(plumb_50above_pd)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_50above_pd_5-21.xlsx")

tidy_plumb_se <- tidy(plumb_50above)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_50above_5-21.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_5-21.xlsx")

tidy_ust_se <- tidy(ust_40below_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_40below_5-21.xlsx")

tidy_ust_se <- tidy(ust_50above_pd)
write_xlsx(tidy_ust_se, path = "Output/ust_model_50above_pd_5-21.xlsx")

tidy_ust_se <- tidy(ust_50above)
write_xlsx(tidy_ust_se, path = "Output/ust_model_50above_5-21.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_5-21.xlsx")

tidy_pm2_se <- tidy(pm2_40below_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_40below_5-21.xlsx")

tidy_pm2_se <- tidy(pm2_50above_pd)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_50above_pd_5-21.xlsx")

tidy_pm2_se <- tidy(pm2_50above)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_50above_5-21.xlsx")

tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_5-21.xlsx")

tidy_green_se <- tidy(green_40below_se)
write_xlsx(tidy_green_se, path = "Output/green_model_40below_5-21.xlsx")

tidy_green_se <- tidy(green_50above_pd)
write_xlsx(tidy_green_se, path = "Output/green_model_50above_pd_5-21.xlsx")

tidy_green_se <- tidy(green_50above)
write_xlsx(tidy_green_se, path = "Output/green_model_50above_5-21.xlsx")

# Models 5-24
tidy_plumb_se <- tidy(plumb_urb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_urb_model_5-24.xlsx")

tidy_plumb_se <- tidy(plumb_urb1_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_urb1_model_5-24.xlsx")

tidy_ust_se <- tidy(ust_urb_se)
write_xlsx(tidy_ust_se, path = "Output/ust_urb_model_5-24.xlsx")

tidy_ust_se <- tidy(ust_urb1_se)
write_xlsx(tidy_ust_se, path = "Output/ust_urb1_model_5-24.xlsx")

tidy_pm2_se <- tidy(pm2_urb_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_urb_model_5-24.xlsx")

tidy_pm2_se <- tidy(pm2_urb1_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_urb1_model_5-24.xlsx")

tidy_green_se <- tidy(green_urb_se)
write_xlsx(tidy_green_se, path = "Output/green_urb_model_5-24.xlsx")

tidy_green_se <- tidy(green_urb1_se)
write_xlsx(tidy_green_se, path = "Output/green_urb1_model_5-24.xlsx")
# ---- Visualizing Models 5-25 ----
tidy_green_se <- tidy(green_top25_se)
write_xlsx(tidy_green_se, path = "Output/green_top25_model_5-25.xlsx")

tidy_green_se <- tidy(green_top25_1_se)
write_xlsx(tidy_green_se, path = "Output/green_top25_1_model_5-25.xlsx")

tidy_green_se <- tidy(green_bottom30_se)
write_xlsx(tidy_green_se, path = "Output/green_bottom30_model_5-25.xlsx")

tidy_green_se <- tidy(green_bottom30_1_se)
write_xlsx(tidy_green_se, path = "Output/green_bottom30_1_model_5-25.xlsx")

tidy_green_se <- tidy(green_poor_se)
write_xlsx(tidy_green_se, path = "Output/green_poor_model_5-25.xlsx")

tidy_green_se <- tidy(green_poor1_se)
write_xlsx(tidy_green_se, path = "Output/green_poor1_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_top25_se)
write_xlsx(tidy_pm2, path = "Output/pm2_top25_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_top25_1_se)
write_xlsx(tidy_pm2, path = "Output/pm2_top25_1_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_bottom30_se)
write_xlsx(tidy_pm2, path = "Output/pm2_bottom30_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_bottom30_1_se)
write_xlsx(tidy_pm2, path = "Output/pm2_bottom30_1_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_poor_se)
write_xlsx(tidy_pm2, path = "Output/pm2_poor_model_5-25.xlsx")

tidy_pm2 <- tidy(pm2_poor1_se)
write_xlsx(tidy_pm2, path = "Output/pm2_poor1_model_5-25.xlsx")

# ---- Visualizing Models 5-26 ----
tidy_green_se <- tidy(green_se)
write_xlsx(tidy_green_se, path = "Output/green_model_5-26.xlsx")

tidy_plumb_se <- tidy(plumb_se)
write_xlsx(tidy_plumb_se, path = "Output/plumb_model_5-26.xlsx")

tidy_ust_se <- tidy(ust_se)
write_xlsx(tidy_ust_se, path = "Output/ust_model_5-26.xlsx")

tidy_pm2_se <- tidy(pm2_se)
write_xlsx(tidy_pm2_se, path = "Output/pm2_model_5-26.xlsx")
# ---- Graphic for Models 6-20 ----
# Air pollution model
pm2_tidy <- tidy(pm2_se, conf.int = T)
pm2_tidy <- pm2_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = pm2_tidy$estimate,
  lower = pm2_tidy$conf.low,
  upper = pm2_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Update labels with significance stars and rounded values
#coef_data$label_with_stars <- sapply(coef_data$mean, custom_format)


# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: DPIA") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             
                
# Leaky UST Model
ust_tidy <- tidy(ust_se, conf.int = T)
ust_tidy <- ust_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = ust_tidy$estimate,
  lower = ust_tidy$conf.low,
  upper = ust_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: Leaky USTs") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             

# Green Space Model
green_tidy <- tidy(green_se, conf.int = T)
green_tidy <- green_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = green_tidy$estimate,
  lower = green_tidy$conf.low,
  upper = green_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, size = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: Lack Green Space") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))  # Adjust y-axis label alignment             

# Plumbing Model
plumb_tidy <- tidy(plumb_se, conf.int = T)
plumb_tidy <- plumb_tidy[1:7, ]

# Prepare data for forest plot
coef_data <- data.frame(
  label = c("(Intercept", "Hispanic", "American Indian", "Black",
            "No HS Degree", "Mean Income (Scaled $10,000)", 
            "Population Density (Scaled 100 acres)"),
  mean  = plumb_tidy$estimate,
  lower = plumb_tidy$conf.low,
  upper = plumb_tidy$conf.high
)

# Custom function to round to two decimal places only if non-zero
custom_format <- function(x) {
  if (abs(x) < 0.01 && x != 0) {
    return(format(round(x, digits = 4), nsmall = 4, scientific = FALSE))
  } else {
    return(format(round(x, 2), nsmall = 2))
  }
}

# Function to add stars to significant estimates
add_stars <- function(mean, lower, upper) {
  if (lower > 0 | upper < 0) {
    return(paste0(custom_format(mean), "***"))
  } else {
    return(custom_format(mean))
  }
}

# Update labels with significance stars
coef_data$label_with_stars <- mapply(add_stars, coef_data$mean, coef_data$lower, coef_data$upper)

# Plotting with ggplot2 (flipped axes)
ggplot(coef_data, aes(x = mean, y = label)) +
  geom_point(size = 3, color = "#1E90FF") +  # Point estimates
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = .1, linewidth = .75, color = "#000080") +  # Horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +  # Zero reference line
  geom_text(aes(label = label_with_stars, x = mean, y = label), vjust = -1.35, hjust = -0.21, size = 5, color = "black") +  # Labels with stars
  labs(x = "Coefficient Estimate", y = "Variable",
       title = "Dependent Variable: IHWA") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 0, size = 12),  # Adjust y-axis label alignment and size
        axis.text.x = element_text(size = 12),             # Adjust x-axis text size
        axis.title = element_text(size = 14),              # Adjust axis titles size
        plot.title = element_text(size = 18, face = "bold"),  # Adjust plot title size and make it bold
        panel.grid = element_blank(),
        axis.line.x = element_line(color = "black"),
        plot.margin = unit(c(2, 1, 10, 1), "lines"))       # Adjust plot margins (top, right, bottom, left))             





