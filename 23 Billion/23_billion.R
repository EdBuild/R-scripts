### 23 Billion

## Disclaimer: 
##  Since the original release of this data product, EdBuild's master datasets have undergone revisions and corrections.
##  Therefore, the data in this script may not match up perfectly to the data published in the report. 
##  This script is primarily meant for users to gain more understanding of EdBuild's methods used in this product
##  and to recreate the analysis in future years. 

setwd("~/Box Sync/Data/Final Repos/23 Billion")
library(edbuildr)
library(dplyr)
library(magrittr)
library(stringr)
library(tidyr)

options(scipen=999) ### disable scientific notation

## Purpose ##
## to examine school district revenues based on racial and socioeconomic characteristics at the national and state level

## Methodology ##
## categorize districts as predominately nonwhite (>75% nonwhite enrollment) or predominately white (>75% white enrollment)
## categorize districts as high-poverty (>20% student poverty rate) or low-poverty (<20% student poverty rate)
## nationally: for all states, compare the average, cost-adjusted total revenue between 1) nonwhite and white districts, 
## 2) high-poverty nonwhite and high-poverty white districts and 3) high poverty nonwhite districts and low-poverty white districts
## states: to be included in the state analysis must have at least 5 districts in each category OR at least 2% of total enrollment in each cateogry

## Dataset used for analysis ##
## 2016 EdBuild master dataset using the standard exclusions for finance analysis (see description of finance exclusion at data.edbuild.org)
## additionally, school districts that intersect with BIA schools were remioved from this dataset

###### APPLYING EXCLUSIONS AND CATEGORIZING DISTRICTS #######
## read in dataset created by EdBuild that categorizes districts that contain BIA schools, Reservation Schools, or intersect with a reservation
BIA <- read.csv("SchoolDistricts_UE_2014_ReservationSchools.csv") %>% 
  mutate(GEOID = str_pad(GEOID, width = 7, pad = "0")) 

BIA_exclude <- BIA %>% ## excluding districts that contatin a BIA school or intersect with a reservation
  filter(BIA_School == "Contains BIA School only" | BIA_School == "Intersects Reservation")

#### create project master dataset
master_16 <- masterpull("2016", data_type = "fin") %>%
  anti_join(BIA_exclude, by = c("NCESID" = "GEOID")) %>% ## excluding districts that contatin a BIA school or intersect with a reservation
  mutate(race_type = case_when(round2(pctNonwhite, 2) > .75 ~ "nonwhite",
                               round2(pctNonwhite, 2) < .25 ~ "white",
                               TRUE ~ "not concentrated"),
         pov_type = case_when(round2(StPovRate, 2) > .2 ~ "high_pov",
                              round2(StPovRate, 2) <= .2 ~ "low_pov",
                              TRUE ~ "NA")) %>%
  select(NCESID, State, race_type, pov_type, pctNonwhite, StPovRate, LRPP_cola,
         SRPP_cola, SLRPP_cola, LR, SR, SLR, LRPP, SRPP, SLRPP, dEnroll_district, ENROLL,
         MHI, MPV) %>%
  mutate(LR_cola = LRPP_cola*ENROLL,
         SR_cola = SRPP_cola*ENROLL,
         SLR_cola = SLRPP_cola*ENROLL)

#### NATIONAL SUMMARY ####
national_race <- master_16 %>%
  group_by(race_type) %>%
  summarise(State = "National",
            count = n(),
            students = sum(dEnroll_district, na.rm = TRUE),
            `Average enrollment` = mean(dEnroll_district, na.rm = TRUE),
            `Average poverty rate` = mean(StPovRate, na.rm = TRUE),
            `State and local revenue, per pupil, cola` = mean(SLRPP_cola, na.rm = TRUE),
            `Local revenue, per pupil, cola` = mean(LRPP_cola, na.rm = TRUE),
            `State revenue, per pupil, cola` = mean(SRPP_cola, na.rm = TRUE),
            `State and local revenue, cola` = sum(SLR_cola, na.rm = TRUE),
            `Local revenue, cola` = sum(LR_cola, na.rm = TRUE),
            `State revenue, cola` = sum(SR_cola, na.rm = TRUE),
            `Average MHI` = mean(MHI, na.rm = TRUE),
            `Average MPV` = mean(MPV, na.rm = TRUE)) %>%  
  mutate(`Percent enrollment` = students/sum(master_16$dEnroll_district, na.rm = TRUE),
         `Percent districts` = count/count(master_16)$n,
         `Percent local revenue` = `Local revenue, cola`/sum(master_16$LR_cola, na.rm = TRUE),
         `Percent state revenue` = `State revenue, cola`/sum(master_16$SR_cola, na.rm = TRUE),
         `Percent state & local revenue` = `State and local revenue, cola`/sum(master_16$SLR_cola, na.rm = TRUE)) %>% 
  select(State, race_type, count, `Percent districts`, students, `Percent enrollment`, 
         `Average enrollment`, `Average poverty rate`:`State revenue, per pupil, cola`, 
         `State and local revenue, cola`:`State revenue, cola`, `Percent state & local revenue`, 
         `Percent local revenue`, `Percent state revenue`, `Average MHI`, `Average MPV`) 

national_race_long <- national_race %>% ## making data long to find differences between white and nonwhite districts
  gather(var, value, -State, -race_type) %>%
  spread(race_type, value) %>%
  mutate(`Difference, White districts - Nonwhite districts` = white - nonwhite) %>%
  mutate_at(vars(nonwhite:`Difference, White districts - Nonwhite districts`), ~(round2(.,2))) 
#### the total revenue for predominantly white and predominantly nonwhite districts can be directly compared because 
#### they education approxiately the same number of students - each education ~ 12 million students

national_race_pov <- master_16 %>%
  group_by(race_type, pov_type) %>%
  filter(pov_type != "NA") %>%
  summarise(State = "National",
            count = n(),
            students = sum(dEnroll_district, na.rm = TRUE),
            `Average enrollment` = mean(dEnroll_district, na.rm = TRUE),
            `Average poverty rate` = mean(StPovRate, na.rm = TRUE),
            `State and local revenue, per pupil, cola` = mean(SLRPP_cola, na.rm = TRUE),
            `Local revenue, per pupil, cola` = mean(LRPP_cola, na.rm = TRUE),
            `State revenue, per pupil, cola` = mean(SRPP_cola, na.rm = TRUE),
            `State and local revenue, cola` = sum(SLR_cola, na.rm = TRUE),
            `Local revenue, cola` = sum(LR_cola, na.rm = TRUE),
            `State revenue, cola` = sum(SR_cola, na.rm = TRUE),
            `Average MHI` = mean(MHI, na.rm = TRUE),
            `Average MPV` = mean(MPV, na.rm = TRUE)) %>%  
  mutate(`Percent enrollment` = students/sum(master_16$dEnroll_district, na.rm = TRUE),
         `Percent districts` = count/count(master_16)$n,
         `Percent local revenue` = `Local revenue, cola`/sum(master_16$LR_cola, na.rm = TRUE),
         `Percent state revenue` = `State revenue, cola`/sum(master_16$SR_cola, na.rm = TRUE),
         `Percent state & local revenue` = `State and local revenue, cola`/sum(master_16$SLR_cola, na.rm = TRUE)) %>% 
  select(State, race_type, pov_type, count, `Percent districts`, students, `Percent enrollment`, 
         `Average enrollment`, `Average poverty rate`:`State revenue, per pupil, cola`, 
         `State and local revenue, cola`:`State revenue, cola`, `Percent state & local revenue`, 
         `Percent local revenue`, `Percent state revenue`, `Average MHI`, `Average MPV`) 

national_race_pov_long <- national_race_pov %>% ## making data long to find differences between white and nonwhite districts
  gather(var, value, -State, -race_type, -pov_type) %>%
  unite(type, race_type, pov_type) %>%
  spread(type, value) %>%
  mutate(`Difference, high poverty, white districts - high poverty, nonwhite districts` = white_high_pov - nonwhite_high_pov,
         `Difference, low poverty, white districts - high poverty, nonwhite districts` = white_low_pov - nonwhite_high_pov) %>%
  mutate_at(vars(nonwhite_high_pov:`Difference, low poverty, white districts - high poverty, nonwhite districts`), ~(round2(.,2)))
#### the total revenue for predominantly white and predominantly nonwhite districts, broken out by high and low poverty CANNOT be directly
#### compared because they do not education the same number of students - the values are included here for illustrative purposes only


#### STATE SUMMARIES ####
# to be included in the state analysis must have at least 5 districts in each category OR at least 2% of total enrollment in each cateogry
state_exclude_race <- master_16 %>%
  group_by(State) %>%
  mutate(state_enroll = sum(dEnroll_district, na.rm = TRUE)) %>%
  group_by(State, race_type) %>%
  summarise(count = n(),
            pct_enrollment = sum(dEnroll_district, na.rm = TRUE)/mean(state_enroll, na.rm= TRUE)) %>%
  filter(count < 5 & pct_enrollment < 0.02) %>%
  select(State)

state_race <- master_16 %>%
  anti_join(state_exclude_race, by = "State") %>% # excluding states that do not have at least 5 districts in each category or at least 2% enrollment
  group_by(State, race_type) %>%
  summarise(count = n(),
            students = sum(dEnroll_district, na.rm = TRUE),
            `Average enrollment` = mean(dEnroll_district, na.rm = TRUE),
            `Average poverty rate` = mean(StPovRate, na.rm = TRUE),
            `State and local revenue, per pupil, cola` = mean(SLRPP_cola, na.rm = TRUE),
            `Local revenue, per pupil, cola` = mean(LRPP_cola, na.rm = TRUE),
            `State revenue, per pupil, cola` = mean(SRPP_cola, na.rm = TRUE),
            `State and local revenue, cola` = sum(SLR_cola, na.rm = TRUE),
            `Local revenue, cola` = sum(LR_cola, na.rm = TRUE),
            `State revenue, cola` = sum(SR_cola, na.rm = TRUE),
            `Average MHI` = mean(MHI, na.rm = TRUE),
            `Average MPV` = mean(MPV, na.rm = TRUE)) %>%  
  mutate(`Percent enrollment` = students/sum(master_16$dEnroll_district, na.rm = TRUE),
         `Percent districts` = count/count(master_16)$n,
         `Percent local revenue` = `Local revenue, cola`/sum(master_16$LR_cola, na.rm = TRUE),
         `Percent state revenue` = `State revenue, cola`/sum(master_16$SR_cola, na.rm = TRUE),
         `Percent state & local revenue` = `State and local revenue, cola`/sum(master_16$SLR_cola, na.rm = TRUE)) %>% 
  select(State, race_type, count, `Percent districts`, students, `Percent enrollment`, 
         `Average enrollment`, `Average poverty rate`:`State revenue, per pupil, cola`, 
         `State and local revenue, cola`:`State revenue, cola`, `Percent state & local revenue`, 
         `Percent local revenue`, `Percent state revenue`, `Average MHI`, `Average MPV`) 

state_race_long <- state_race %>% ## making data long to find differences between white and nonwhite districts
  gather(var, value, -State, -race_type) %>%
  spread(race_type, value) %>%
  mutate(`Difference, White districts - Nonwhite districts` = white - nonwhite) %>%
  mutate_at(vars(nonwhite:`Difference, White districts - Nonwhite districts`), ~(round2(.,2)))
#### the total revenue for predominantly white and predominantly nonwhite districts can only be compared
#### if they educate the same number of students - the values are included here for illustrative purposes only


state_exclude_race_pov <- master_16 %>%
  filter(pov_type != "NA") %>%
  group_by(State) %>%
  mutate(state_enroll = sum(dEnroll_district, na.rm = TRUE)) %>%
  group_by(State, race_type, pov_type) %>%
  summarise(count = n(),
            pct_enrollment = sum(dEnroll_district, na.rm = TRUE)/mean(state_enroll, na.rm= TRUE)) %>%
  filter(count < 5 & pct_enrollment < 0.02) %>%
  select(State, race_type, pov_type) 

state_race_pov <- master_16 %>%
  filter(pov_type != "NA") %>%
  anti_join(state_exclude_race_pov, by = c("State", "race_type", "pov_type")) %>% # excluding categories in states that do not have at least 5 districts or at least 2% enrollment
  group_by(State, race_type, pov_type) %>%
  summarise(count = n(),
            students = sum(dEnroll_district, na.rm = TRUE),
            `Average enrollment` = mean(dEnroll_district, na.rm = TRUE),
            `Average poverty rate` = mean(StPovRate, na.rm = TRUE),
            `State and local revenue, per pupil, cola` = mean(SLRPP_cola, na.rm = TRUE),
            `Local revenue, per pupil, cola` = mean(LRPP_cola, na.rm = TRUE),
            `State revenue, per pupil, cola` = mean(SRPP_cola, na.rm = TRUE),
            `State and local revenue, cola` = sum(SLR_cola, na.rm = TRUE),
            `Local revenue, cola` = sum(LR_cola, na.rm = TRUE),
            `State revenue, cola` = sum(SR_cola, na.rm = TRUE),
            `Average MHI` = mean(MHI, na.rm = TRUE),
            `Average MPV` = mean(MPV, na.rm = TRUE)) %>%  
  mutate(`Percent enrollment` = students/sum(master_16$dEnroll_district, na.rm = TRUE),
         `Percent districts` = count/count(master_16)$n,
         `Percent local revenue` = `Local revenue, cola`/sum(master_16$LR_cola, na.rm = TRUE),
         `Percent state revenue` = `State revenue, cola`/sum(master_16$SR_cola, na.rm = TRUE),
         `Percent state & local revenue` = `State and local revenue, cola`/sum(master_16$SLR_cola, na.rm = TRUE)) %>% 
  select(State, race_type, pov_type, count, `Percent districts`, students, `Percent enrollment`, 
         `Average enrollment`, `Average poverty rate`:`State revenue, per pupil, cola`, 
         `State and local revenue, cola`:`State revenue, cola`, `Percent state & local revenue`, 
         `Percent local revenue`, `Percent state revenue`, `Average MHI`, `Average MPV`) 

state_race_pov_long <- state_race_pov %>% ## making data long to find differences between white and nonwhite districts
  gather(var, value, -State, -race_type, -pov_type) %>%
  unite(type, race_type, pov_type) %>%
  spread(type, value) %>%
  mutate(`Difference, high poverty, white districts - high poverty, nonwhite districts` = white_high_pov - nonwhite_high_pov,
         `Difference, low poverty, white districts - high poverty, nonwhite districts` = white_low_pov - nonwhite_high_pov) %>%
  mutate_at(vars(nonwhite_high_pov:`Difference, low poverty, white districts - high poverty, nonwhite districts`), ~(round2(.,2)))
