master_fin17 <- masterpull("2017", data_type = "fin")

pairs_og <- read.xlsx("~/Box Sync/Data/Dismissed/Data/Working/final/all_pairs_with_b10_2017.xlsx")

pairs_og_small <- pairs_og %>%
  filter(`Percent.Non-White.Difference` > 0) %>%
  filter(urbanicity_1 != "43-Rural: Remote" & urbanicity_2 != "43-Rural: Remote") %>%
  left_join(select(master_fin17, student_per_sq_mile, NCESID), by = c("ncesid_1" = "NCESID")) %>%
  rename(spsqmi_1 = student_per_sq_mile) %>%
  left_join(select(master_fin17, student_per_sq_mile, NCESID), by = c("ncesid_2" = "NCESID")) %>%
  rename(spsqmi_2 = student_per_sq_mile) %>%
  filter(spsqmi_1 > 1 & spsqmi_2 > 1) %>%
  rename(u_id = uid) %>%
  filter(u_id != "0613360_0624030" & u_id != "2309930_2314240" & u_id != "2600012_2622230" & ### Removing all pairs that aren't actually neighbors (islands and such)
           u_id != "3624690_3609900" & u_id != "3624690_3609900" & u_id != "4815270_4821780" &
           u_id != "4815270_4824180" & u_id != "4817070_4809450" & u_id != "4820280_4808190")

#### 18,857!

pairs_url = "https://s3.amazonaws.com/data.edbuild.org/public/Processed+Data/Pairs/pairs_1617.csv"
pairs_new <- read.csv(file = pairs_url, stringsAsFactors = FALSE) %>%
  mutate(GEOID = str_pad(as.character(GEOID), width = 7, pad = "0"),
         GEOID.1 = str_pad(as.character(GEOID.1), width = 7, pad = "0"))

master_names <- names(master_fin17) ## making vector of names in master to apply below with either _1 or _2

master1 <- master_fin17 %>%
  magrittr::set_colnames(paste(master_names, "_1", sep = "")) ## making a master file where variables have _1 after name

master2 <- master_fin17 %>%
  magrittr::set_colnames(paste(master_names, "_2", sep = "")) # making a master file where variables have _2 after name

pair_data <- pairs_new %>% ## Combine variables + neighbors
  dplyr::left_join(master1, by = c("GEOID" = "NCESID_1")) %>% ## First, join variables to neighbor list (base) - ncesid_1
  dplyr::left_join(master2, by = c("GEOID.1" = "NCESID_2")) %>% ## Then, join variables to neighbor list (pair) - the neighbor pair
  dplyr::filter(State_1 == State_2,      ## filtering out pairs where they are across state lines
                length >= 152.4) %>%             ## filter so districts are only neighbors if they share more than 500ft of border line
  dplyr::select(-STATE_FIPS_2, -State_2, -CONUM_1, -CONUM_2, -SRPP_cola_1, -LRPP_cola_1, -SLRPP_cola_1,
                -SRPP_cola_2, -LRPP_cola_2, -SLRPP_cola_2) %>%
  dplyr::rename(State = State_1,
                FIPS = STATE_FIPS_1,
                `border_length (meters)` = length) %>%
  mutate(pctnonwhite_diff = pctNonwhite_1 - pctNonwhite_2) %>%
  filter(pctnonwhite_diff > 0) %>%
  filter(dUrbanicity_1 != "43-Rural: Remote" & dUrbanicity_2 != "43-Rural: Remote") %>%
  filter(student_per_sq_mile_1 > 1 & student_per_sq_mile_2 >1) %>%
  filter(sd_type_1 == "uni" & sd_type_2 == "uni") %>%
  filter(u_id != "0613360_0624030" & u_id != "2309930_2314240" & u_id != "2600012_2622230" & ### Removing all pairs that aren't actually neighbors (islands and such)
           u_id != "3624690_3609900" & u_id != "3624690_3609900" & u_id != "4815270_4821780" &
           u_id != "4815270_4824180" & u_id != "4817070_4809450" & u_id != "4820280_4808190")

## 18,951!! 

missing <- anti_join(pair_data, pairs_og_small, by = "u_id") ## 137 are in neigh_diff but not OG
missing2 <- anti_join(pairs_og_small, pair_data, by = "u_id") ## 43 are in OG but not neigh_diff

### so there is something different about the pair data (probbaly the way we found pairs in dismissed) vs how we do now
### in OG dismissed we used a slightly differnt method of getting pairs and it ended with a different list than neigh_diff
### to replicate the exact dismissed data we need to start with the original pairs list
### the way we found pairs in dismissed is here: ~/Box Sync/Data/Dismissed/Data/Scripts/final/1_finding pairs.R
