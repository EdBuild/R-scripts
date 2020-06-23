### Dismissed: America's Most Divisive School District Borders

## Disclaimer: 
##  Since the original release of this data product, EdBuild's master datasets have undergone revisions and corrections.
##  Therefore, the data in this script may not match up perfectly to the data published in the report. 
##  This script is primarily meant for users to gain more understanding of EdBuild's methods used in this product
##  and to recreate the analysis in future years. 

setwd("~/Box Sync/Data/Final Repos/Dismissed")
library(edbuildr)
library(edbuildmapr)
library(dplyr)
library(magrittr)
library(stringr)

## Purpose ##
## to compare funding and racial demographics between neighboring school districts in order to identify the most divisive school districts in the country

## Methodology ##
## spatial analysis of all unified districts in the country to identify neighbor pairs that share at least a 500 feet land border
## pairs joined to 2017 F33, CCD, ACS, SAIPE data 
## district neighbor pairs with at least a 25 percentage point gap in percent nonwhite enrollment and at least a 10% gap in total revenue are divisive  
## district neighbor pairs with at least a 50 percentage point gap in percent nonwhite enrollment and at least a 20% gap in total revenue are  deeply divisive  
## revenue gaps consider local revenue, state revenue, and impact aid

## Dataset used for analysis ##
## 2017 EdBuild master dataset using the standard exclusions for finance analysis (see description of finance exclusion at data.edbuild.org)
## Additional schopol districts removed for this analysis:
## only unified school district neighbor pairs - all elementary and secondary districts removed
## all districts with the urbanicity category equal to 'rural, remote' removed
## all districts with density of equal to or less than 1 student per square mile were removed

######## FINDING SCHOOL DISTRICT NEIGHBOR PAIRS ##########
## here we use neigh_diff, an edbuildr function, to find all like-type school district neighbor pairs in the country (both districts are the same type, 
## for examples, both are unified) that share at least a 500 feet land border, then calculate the difference in percent nonwhite enrollment between these pairs
## neigh_diff uses EdBuild's master dataset with the geography exclusion

pairs <- neigh_diff(data_year = "2017", diff_var = "Percentage Point Difference in Percent Nonwhite Enrollment", type = "like")

## if you want to use a different dataset to recreate these results use the edbuildmapr function borders() as seen below
## this function imports any shapefile and exports a dataframe of neighbors, ranked on the diff_var you select
## for all ~13,000 school districts in the United States, this function takes at least 15 minutes to run

# pov_rank <- borders(shapefile = "path name", id = "unique_id", diff_var = "pctNonwhite", export = "dataframe")

##### Created Dismissed master dataset based on the dataset definition above ####
master_fin <- masterpull("2017", data_type = "fin") %>% ## pull in master for finance exclusion 
  select(NCESID)

pairs_excl <- pairs %>%
  mutate(NCESID_1 = str_pad(as.character(NCESID_1), width = 7, pad = "0"),
         NCESID_2 = str_pad(as.character(NCESID_2), width = 7, pad = "0")) %>%
  filter(`District Level` == "uni" & `Neighbor Level` == "uni") %>% ## district type exclusion
  filter(!is.na(`District Total Revenue PP`) & !is.na(`Neighbor Total Revenue PP`)) %>%
  filter(`District Urbanicity` != "43-Rural: Remote" & `Neighbor Urbanicity` != "43-Rural: Remote") %>% ## urbanicity exclusion
  filter(`District Students per Square Mile` > 1 & `Neighbor Students per Square Mile` > 1) %>% # student per square mile exclusion 
  inner_join(master_fin, by = c("NCESID_1"= "NCESID")) %>% # only selecting districts that meet finance exlcusion
  inner_join(master_fin, by = c("NCESID_2"= "NCESID"))  %>% # only selecting neighbors that meet finance exlcusion
  filter(u_id != "0613360_0624030" & u_id != "2309930_2314240" & u_id != "2600012_2622230" & ### Removing all pairs that aren't actually neighbors (islands and such)
           u_id != "3624690_3609900" & u_id != "3624690_3609900" & u_id != "4815270_4821780" &
           u_id != "4815270_4824180" & u_id != "4817070_4809450" & u_id != "4820280_4808190")
  
#### This resulted in a dataset that contains 18,948 pairs of district neighbors ####

###### FINDING DIVISIVE BORDERS #######
impact <- f33pull("2017", additional_var = "IMPACT_AID") %>% ## pulling in 2017 F33 data on impact aid
  select(NCESID, IMPACT_AID)

divisive <- pairs_excl %>%
  left_join(impact, by = c("NCESID_1" = "NCESID")) %>%
  rename(impact_1 = IMPACT_AID) %>% # district impact aid
  left_join(impact, by = c("NCESID_2" = "NCESID")) %>%
  rename(impact_2 = IMPACT_AID) %>% # neighbor impact aid
  mutate(IAPP_1 = impact_1/`District Enrollment, F33`,
         IAPP_2 = impact_2/`Neighbor Enrollment, F33`,
         trpp_ia_1 = `District Total Revenue PP` + IAPP_1, 
         trpp_ia_2 = `Neighbor Total Revenue PP` + IAPP_2,
         pct_diff_trpp_ia = (trpp_ia_1 - trpp_ia_2)/trpp_ia_2,
         pct_diff_rev = (`District Total Revenue PP` - `Neighbor Total Revenue PP`)/`Neighbor Total Revenue PP`,
         pct_diff_nw = `District Percent Nonwhite` - `Neighbor Percent Nonwhite`) %>%
  filter(round2(pct_diff_nw, 2) >= .25 
         & round2(pct_diff_trpp_ia, 2) <= -.1
         & round2(pct_diff_rev, 2) <= -.1)


####### FINDING DEEPLY DIVISIVE BORDERS ########
deep_divisive <- pairs_excl %>%
  left_join(impact, by = c("NCESID_1" = "NCESID")) %>%
  rename(impact_1 = IMPACT_AID) %>% # district impact aid
  left_join(impact, by = c("NCESID_2" = "NCESID")) %>%
  rename(impact_2 = IMPACT_AID) %>% # neighbor impact aid
  mutate(IAPP_1 = impact_1/`District Enrollment, F33`,
         IAPP_2 = impact_2/`Neighbor Enrollment, F33`,
         trpp_ia_1 = `District Total Revenue PP` + IAPP_1, 
         trpp_ia_2 = `Neighbor Total Revenue PP` + IAPP_2,
         pct_diff_trpp_ia = (trpp_ia_1 - trpp_ia_2)/trpp_ia_2,
         pct_diff_rev = (`District Total Revenue PP` - `Neighbor Total Revenue PP`)/`Neighbor Total Revenue PP`,
         pct_diff_nw = `District Percent Nonwhite` - `Neighbor Percent Nonwhite`) %>%
  filter(round2(pct_diff_nw, 2) >= .5 
         & round2(pct_diff_trpp_ia, 2) <= -.2
         & round2(pct_diff_rev, 2) <= -.2)

