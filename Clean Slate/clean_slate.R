### Clean Slate

## Disclaimer: 
##  Since the original release of this data product, EdBuild's master datasets may have undergone revisions and corrections.
##  Therefore, the data in this script may not match up perfectly to the data published in the report. 
##  This script is primarily meant for users to gain more understanding of EdBuild's methods used in this product
##  and to recreate the analysis in future years. 

setwd("~/Box Sync/Data/Final Repos/Clean Slate")
library(devtools)
install_github("EdBuild/edbuildr") ## need the edbuildr on github for this to use big_borders below
library(edbuildmapr)
library(dplyr)
library(magrittr)
library(stringr)

## Purpose ##
## to explore the percent of students who would receive equal or greater funding if local revenue were pooled at the county or state level

## Methodology ##
## pool local revenue at the county level and the state level for all distrcits in the country 
## calcuate the difference between current local revenue and pooled local revenue (for both state and county)
## calcuate percent of students, FRL students, and nonwhite students who get equal or greater funding under both systems
## determine which pooling level (county or state) is best based on percentages above
## neighbor-pooling: found local revenue per pupil if all neighboring districts were to share local revenue

## Dataset used for analysis ##
## 2017 EdBuild master dataset using the standard exclusions for geography exclusion (see description of geogrpahy exclusion at data.edbuild.org)
## Vermont was excluded because school district boundaries were changing due to Act 46
## districts missing F33 data were excluded

##### COUNTY LEVEL REVENUE POOLING ######
county <- big_borders("2017", pooling_level = "county") ## uses edbuild's geogrpahy exclusion to pool local revenue at the county level

## collecting FRL data for specific states from previous years since it is missing in 2017 data
frl17 <- masterpull(data_year = "2017", data_type = "geo") %>% 
  select(NCESID, dFRL, FRL_rate, dWhite) 

frl16 <- masterpull(data_year = "2016", data_type = "geo") %>% 
  filter(State == "Delaware" | State == "Tennessee" | State == "District of Columbia") %>% ## DE and TN and DC have FRL in 2016
  select(NCESID, dFRL, FRL_rate) %>% 
  rename(frl = dFRL,
         frlrate = FRL_rate)

frl15 <- masterpull(data_year = "2015", data_type = "geo") %>% 
  filter(State == "Massachusetts") %>% ## MA has FRL in 2015
  select(NCESID, dFRL, FRL_rate) %>% 
  rename(frl = dFRL,
         frlrate = FRL_rate)

frl <- frl17 %>%
  left_join(frl16, by = "NCESID") %>%
  mutate(dFRL = if_else(is.na(dFRL), frl, dFRL),
         FRL_rate = if_else(is.na(FRL_rate), frlrate, FRL_rate)) %>% ## adding 2016 frl for DE and TN and DC
  select(-frl, -frlrate) %>%
  left_join(frl15, by = "NCESID") %>% ## adding 2015 frl for MA
  mutate(dFRL = if_else(is.na(dFRL), frl, dFRL),
         FRL_rate = if_else(is.na(FRL_rate), frlrate, FRL_rate)) %>%
  select(-frl,  -frlrate)

county_outcomes <- county %>%
  left_join(frl, by = "NCESID") %>%
  mutate(nonwhite = dEnroll_district - dWhite) %>%
  group_by(State) %>%
  mutate(enroll_state = sum(dEnroll_district, na.rm = TRUE),
         nonwhite_state = enroll_state - sum(dWhite, na.rm = TRUE),
         frl_state = sum(dFRL, na.rm = TRUE)) %>%
  group_by(State, outcome) %>%
  summarise(percent_students = sum(dEnroll_district, na.rm =TRUE) / mean(enroll_state), ## finding percent of students, frl students and nonwhite students by outcome
            percent_frl = sum(dFRL, na.rm =TRUE)/ mean(frl_state),
            percent_nonwhite = sum(nonwhite, na.rm =TRUE)/mean(nonwhite_state),
            net_pct_county = percent_students + percent_frl + percent_nonwhite) %>%
  filter(State != "Vermont")

##### STATE LEVEL REVENUE POOLING
state <- big_borders("2017", pooling_level = "state") ## uses edbuild's geogrpahy exclusion to pool local revenue at the state level

state_outcomes <- state %>%
  left_join(frl, by = "NCESID") %>%
  mutate(nonwhite = dEnroll_district - dWhite) %>%
  group_by(State) %>%
  mutate(enroll_state = sum(dEnroll_district, na.rm = TRUE),
         nonwhite_state = enroll_state - sum(dWhite, na.rm = TRUE),
         frl_state = sum(dFRL, na.rm = TRUE)) %>%
  group_by(State, outcome) %>%
  summarise(percent_students = sum(dEnroll_district, na.rm =TRUE) / mean(enroll_state), ## finding percent of students, frl students and nonwhite students by outcome
         percent_frl = sum(dFRL, na.rm =TRUE)/ mean(frl_state),
         percent_nonwhite = sum(nonwhite, na.rm =TRUE)/mean(nonwhite_state),
         net_pct_state = percent_students + percent_frl + percent_nonwhite) %>%
  filter(State != "Vermont")

##### COMPARING OUTCOMES TO DETERMINE BEST POOLING METHOD IN EACH STATE ######
best <- select(state_outcomes, State, outcome, net_pct_state) %>%
  left_join(select(county_outcomes, State, outcome, net_pct_county), by = c("State", "outcome")) %>%
  filter(outcome == "same or more") %>%
  mutate(county = if_else(net_pct_county > net_pct_state, 1, 0),
         state = if_else(net_pct_county < net_pct_state, 1, 0),
         pooling_level = if_else(county == 1, "county", "state"))

###### MAKING FULL DATASET BASED ON BEST POOLING METHOD ######
best_county <- best %>%
  filter(pooling_level == "county") %>%
  select(State, pooling_level) %>%
  left_join(county, by = "State") %>%
  rename(LRPP_pool = LRPP_cnty,
         SRPP_pool = SRPP_cnty,
         SLRPP_pool = SLRPP_cnty)

best_state <- best %>%
  filter(pooling_level== "state") %>%
  select(State, pooling_level) %>%
  left_join(state, by = "State") %>%
  rename(LRPP_pool = LRPP_state,
         SRPP_pool = SRPP_state,
         SLRPP_pool = SLRPP_state)

full_data <- bind_rows(best_county, best_state)

####### NEIGHBOR POOLING ##########
districts <- masterpull("2017", data_type = "geo") %>%
  dplyr::select(NCESID, State, NAME, County, STATE_FIPS, CONUM, LR, ENROLL, SR, SRPP, LRPP, SLRPP, dEnroll_district, dWhite, dFRL, StPop, StPov)

master_names <- names(districts) ## making vector of names in master to apply below with either _1 or _2

districts1 <- districts %>%
  set_colnames(paste(master_names, "_1", sep = ""))  %>% ## making a master file where variables have _1 after name
  dplyr::mutate(NCESID_1 = stringr::str_pad(NCESID_1, width = 7, pad = "0")) %>%
  mutate(nonwhite_1 = dEnroll_district_1 - dWhite_1)

districts2 <- districts %>%
  set_colnames(paste(master_names, "_2", sep = "")) %>% ## making a master file where variables have _2 after name
  dplyr::mutate(NCESID_2 = stringr::str_pad(NCESID_2, width = 7, pad = "0")) %>%
  mutate(nonwhite_2 = dEnroll_district_2 - dWhite_2)

pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1617.csv"
pairs <- read.csv(file = pairs_url, stringsAsFactors = FALSE) %>%
  dplyr::mutate(GEOID = as.character(GEOID),
                GEOID = stringr::str_pad(GEOID, width = 7, pad = "0"),
                GEOID.1 = as.character(GEOID.1),
                GEOID.1 = stringr::str_pad(GEOID.1, width = 7, pad = "0")) %>%
  dplyr::select(-u_id)

pairs_pooled <- pairs %>%
  dplyr::left_join(districts1, by = c("GEOID" = "NCESID_1")) %>%
  dplyr::left_join(districts2, by = c("GEOID.1" = "NCESID_2")) %>%
  dplyr::rename(NCESID_1 = GEOID,
                NCESID_2= GEOID.1) %>%
  dplyr::filter(State_1 == State_2,      ## must be neighbors in same state
                length >= 152.4,
                !is.na(LRPP_2),
                !is.na(LRPP_1)) %>%  ## filter so districts are only neighbors if they share more than 500ft of border line
  dplyr::group_by(NCESID_1) %>%
  dplyr::mutate(LR_neigh = sum(LR_2, na.rm = TRUE), ### adding lr of all neighbors plus own lr and dividing by enrollment of all neighbors plus self
                enroll_f33 = sum(ENROLL_2, na.rm = TRUE),
                LRPP_neigh = (LR_neigh+LR_1)/(enroll_f33+ENROLL_1),
                enroll_neigh = sum(dEnroll_district_2, na.rm = TRUE) + dEnroll_district_1,
                frl_neigh = sum(dFRL_2, na.rm = TRUE) + dFRL_1,
                pov_neigh = sum(StPov_2, na.rm = TRUE) + StPov_1,
                nonwhite_neigh = sum(nonwhite_2, na.rm=TRUE) + nonwhite_1,
                pct_frl_neigh = frl_neigh/enroll_neigh,
                pct_pov_neigh = pov_neigh/enroll_neigh,
                pct_nw_neigh = nonwhite_neigh/enroll_neigh,
                districts = n()+1) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(SLRPP_diff_dist = round2(LRPP_neigh - LRPP_1, 0),
                SLRPP_diff_neigh = round2(LRPP_neigh - LRPP_2, 0)) %>% ### calculate difference in local revenue between current and pool system
  dplyr::filter(!is.na(SLRPP_diff_dist)) %>%
  dplyr::mutate(outcome_dist = case_when(SLRPP_diff_dist < 0 ~ "lose",
                                         SLRPP_diff_dist >= 0 ~ "win",
                                         TRUE ~ as.character(NA)),
                outcome_neigh = case_when(SLRPP_diff_neigh < 0 ~ "lose",
                                          SLRPP_diff_neigh >= 0 ~ "win",
                                          TRUE ~ as.character(NA))) %>%
  dplyr::select(NCESID_1, NCESID_2, STATE_FIPS_1, CONUM_1, State_1, County_1, NAME_1, NAME_2, LRPP_1, LRPP_neigh, LRPP_2,
                SLRPP_diff_dist, SLRPP_diff_neigh, outcome_dist, outcome_neigh, enroll_neigh, pov_neigh, pct_pov_neigh,
                nonwhite_neigh, pct_nw_neigh, 
                frl_neigh, dEnroll_district_1, dEnroll_district_2, dFRL_1, dFRL_2, StPov_1, StPov_2,
                nonwhite_1, nonwhite_2, districts) %>%
  filter(State_1 != "Vermont")

