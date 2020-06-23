### Fault Lines: America's Most Segregating School District Borders

## Disclaimer: 
##  Since the original release of this data product, EdBuild's master datasets have undergone revisions and corrections.
##  Therefore, the data in this script may not match up perfectly to the data published in the report. 
##  This script is primarily meant for users to gain more understanding of EdBuild's methods used in this product
##  and to recreate the analysis in future years. 

setwd("~/Box Sync/Data/Final Repos/Fault Lines")
library(edbuildr)
library(edbuildmapr)
library(dplyr)
library(magrittr)
library(sf)
library(stringr)

## Purpose ##
## to compare student poverty rates between neighboring school districts in order to identify the most segregating school districts in the country

## Methodology ##
## spatial analysis of all unified, secondary and elementary districts in the country to identify neighbor pairs that share at least a 500 feet land border
## pairs joined to 2017 CCD, ACS, SAIPE data 
## difference in student poverty rates between neighboring school district pairs calculated and ranked

## Dataset used for analysis ##
## 2017 EdBuild master dataset using the standard exclusions for geography exclusion (see description of geogrpahy exclusion at data.edbuild.org)
## Additional criteria for school districts to be included in this analysis:
## type similar school districts (unified to unified, secondary to secondary, and elementary to elementary)
## districts intersecting with at least 1/4 square mile of Native American Reservations where at least 25% of students are Native American were removed
## districts will less than 200 students were removed
## districts where the enrollment is less than 25% of the SAIPE estimated student population were removed
## districts with density of equal to or less than 1 student per square mile were removed
## Alaska, Nevada, and Wyoming were removed because over 2/3 of their students were removed based on the above 

######## FINDING AND RANKING SCHOOL DISTRICT NEIGHBOR PAIRS BASED ON STUDENT POVERTY RATE ##########
## here we use neigh_diff, an edbuildr function, to find all like-type school district neighbor pairs in the country
## that share at least a 500 feet land border, then calculate the difference in student poverty rate between
## these pairs and rank from largest to smallest difference
## neigh_diff uses EdBuild's master dataset with the geography exclusion

pov_rank <- neigh_diff(data_year = "2017", diff_var = "Percentage Point Difference in Poverty Rate", type = "like")

## if you want to use a different dataset to recreate these results use the edbuildmapr function borders() as seen below
## this function imports any shapefile and exports a dataframe of neighbors, ranked on the diff_var you select

# pov_rank <- borders(shapefile = "path name", id = "unique_id", diff_var = "StPovRate", export = "dataframe")

##### Created Fault Lines master dataset based on the dataset definition above ####
## read in reservation lands shapefile- the intersection of reservation lands with school districts
res_lands <- st_read("sds_indianlands_intersection.shp")

res_lands$area <- st_area(res_lands) ## finding the area of each intersection

res_lands %<>%
  mutate(area = round(as.numeric(area)*3.86102e-7,2)) %>% # convert to square miles
  group_by(GEOID, NAME) %>%
  mutate(full_area = sum(area, na.rm=TRUE)) ## summing up each school district's area that intersects with reservation land

res_lands_df <- as.data.frame(res_lands) %>%
  select(GEOID, NAME,full_area) %>%
  distinct() %>%
  filter(full_area >= .25)  # getting the full list of each school district which intersects with at least 1/4 mile of a reservation

## pulling in EdBuild master data to get percent of Native American students in each district
na_students <- masterpull("2017", data_type = "geo") %>%
  select(NCESID, dEnroll_district, dAmIndian_Aknative) %>%
  mutate(pct_na = dAmIndian_Aknative/dEnroll_district)

res_lands_df %<>% 
  left_join(na_students, by = c("GEOID" = "NCESID")) %>% ## joining percent of native students
  filter(round2(pct_na,2) >= .25) ## filtering districts where at least 25% of students are native

pov_rank_excl <- pov_rank %>%
  mutate(NCESID_1 = str_pad(as.character(NCESID_1), width = 7, pad = "0"),
         NCESID_2 = str_pad(as.character(NCESID_2), width = 7, pad = "0")) %>%
  filter(`District Student Population` >= 200 & `Neighbor Student Population` >= 200) %>% # student pop exclusion
  anti_join(res_lands_df, by = c("NCESID_1" = "GEOID")) %>% #reservation land exclusion for district 1
  anti_join(res_lands_df, by = c("NCESID_2" = "GEOID")) %>% #reservation land exclusion for district 2
  filter(`District Enrollment, CCD` >= .25*`District Student Population` 
         & `District Enrollment, CCD` >= .25*`District Student Population`) %>% # remove districts where the enrollment is less than 25% of the SAIPE estimated student population
  filter(`District Students per Square Mile` >= 1 & `Neighbor Students per Square Mile` >= 1) %>% # student per square mile exclusion
  filter(State != "Alaska" & State !="Nevada" & State != "Wyoming")  %>% ## state exclusions
  filter(u_id != "0613360_0624030" & u_id != "2309930_2314240" & u_id != "2600012_2622230" & ### Removing all pairs that aren't actually neighbors (islands and such)
           u_id != "3624690_3609900" & u_id != "3624690_3609900" & u_id != "4815270_4821780" &
           u_id != "4815270_4824180" & u_id != "4817070_4809450" & u_id != "4820280_4808190") %>%
  mutate(`Rank_Percentage Point Difference in Poverty Rate` = row_number()) # recreating rank once excluded districts are removed

##### SELECTING VARIABLES OF INTEREST ########
fl_table <- pov_rank_excl %>%
  select(NCESID_1, NCESID_2, State, `Rank_Percentage Point Difference in Poverty Rate`,
         `District Name`, `Neighbor Name`,
         `District Poverty Rate`, `Neighbor Poverty Rate`, `Percentage Point Difference in Poverty Rate`, 
         `District Enrollment, CCD`, `Neighbor Enrollment, CCD`, `District Urbanicity`, `Neighbor Urbanicity`, 
         `District Local Revenue PP`, `Neighbor Local Revenue PP`, 
         `District State Revenue PP`, `Neighbor State Revenue PP`, 
         `District Total Revenue PP`, `Neighbor Total Revenue PP`, 
         `District Percent Nonwhite`, `Neighbor Percent Nonwhite`, 
         `District MHI`, `Neighbor MHI`, 
         `District MPV`, `Neighbor MPV`,
         `District Level`, `Neighbor Level`)
  