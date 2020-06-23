### DIVIDING LINES - GATED SCHOOL DISTRICTS

## Disclaimer: 
##  Since the original release of this data product, EdBuild's master datasets have undergone revisions and corrections.
##  Additionally, the script below uses data for the 2017-18 school year
##  Therefore, the data in this script may not match up perfectly to the data published in the report. 
##  This script is primarily meant for users to gain more understanding of EdBuild's methods used in this product
##  and to recreate the analysis in future years. 

setwd("~/Box Sync/Data/Final Repos/Islands")
library(edbuildmapr)
library(dplyr)
library(magrittr)
library(stringr)
library(lwgeom)

## Purpose ##
## to highlight island school districts, those that are entirely surrounded by single districts

## Methodology ##
## spatial analysis of all districts in the country to identify neighbor pairs that share at least a 500 feet land border
## districts with only 1 neighbor pair are considered islands

## Dataset used for analysis ##
## 2018 EdBuild master dataset using the standard exclusions for geography exclusion (see description of geogrpahy exclusion at data.edbuild.org)

### FINDING ALL DISTRICTS WITH ONLY 1 NEIGHBOR ###
borders <- borders(shapefile = "2018", id = "GEOID", export = "dataframe") # note- this function takes ~15 mins to run
  
borders_1 <- borders %>%
  mutate(state_1 = substr(id1, 0,2),
         state_2 = substr(id2, 0, 2)) %>%
  filter(state_1 == state_2,    ## remove pairs that cross state lines
        length >= 152.4)    ## filter so districts are only neighbors if they share more than 500ft of border line
  
dists_one <- borders_1 %>% ## finding districts in group id1 that only have 1 neighbor
  group_by(id1) %>%
  mutate(neighbors = n()) %>%
  filter(neighbors == 1) 

neighs_one <- borders_1 %>% ## finding districts in group id2 only have 1 neighbor
  group_by(id2) %>%
  mutate(neighbors_2 = n()) %>%
  select(id1, id2, neighbors_2)

dists_one %<>% ## making sure districts in id1 that only had one neighbor don't have any neighbors from group id2
  left_join(neighs_one, by = c("id1" = "id2")) %>% 
  filter(is.na(neighbors_2)) %>%
  select(u_id, id1, id2, length)

### FINDING ISLANDS ###
## a district is an island if it is entirely surrounded by another district
perimeter <- sd_shapepull("2018") 

perimeter_1 <- perimeter %>%
  mutate(border = st_cast(geometry, "MULTILINESTRING"),
         length = st_length(border)) 

perimeter_df <- as.data.frame(perimeter_1) %>%
  select(-geometry, -border) 

compare_length <- dists_one %>%
  left_join(perimeter_df, by = c("id1"= "GEOID")) %>%
  rename(neigh_length = length.x,
         border_length = length.y) %>%
  mutate(border_length = round2(as.numeric(gsub(" [m] ", "", border_length)), 0),
         ratio = neigh_length/border_length) ## finding ratio of full border length to length of border shared by neighbor

## pull in master data to join to list of islands
master_18 <- masterpull("2018", data_type = "geo") 

islands <- compare_length %>%
  filter(ratio > .5)  %>%## filtering only for districts whose full border length is at least 50% as the border it shares with it's 1 neighbor
  select(id1, id2, ratio) %>%
  left_join(select(master_18, NCESID, State, NAME), by = c("id1" = "NCESID")) %>%
  rename(Island_district = NAME) %>%
  left_join(select(master_18, NCESID, NAME), by = c("id2" = "NCESID")) %>%
  rename(Neighbor = NAME,
         Island_id = id1,
         Neighbor_id = id2) 

### check the districts with ratio < 1 to decide whether they are actual islands, or school districts with only one neighbor

  
  
