library(tidyverse)
library(lubridate)
library(COVID19)
library(usethis)
library(leaflet)


usethis::use_git_config(user.name = "ardomingo", user.email = "andres.domingo@outlook.com")
token <- "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"

credentials::set_github_pat("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")



new_dat_us <- covid19(country = c("US"), level = 3)

glimpse(new_dat_us)

by_county_us <- 
  new_dat_us %>% 
    group_by(administrative_area_level_2, administrative_area_level_3) %>% 
      summarise(cases = sum(confirmed), deaths = sum(deaths), hosp = sum(hosp))

by_county_us_last7 <- 
  new_dat_us %>% filter(date >= today()-7) %>% 
    group_by(administrative_area_level_2, administrative_area_level_3) %>% 
      summarise(cases7 = sum(confirmed), deaths7 = sum(deaths), hosp7 = sum(hosp))

by_county_us_prior7 <- 
  new_dat_us %>% filter(date >= today()-14 & date < today()-7) %>% 
  group_by(administrative_area_level_2, administrative_area_level_3) %>% 
  summarise(cases14 = sum(confirmed), deaths14 = sum(deaths), hosp14 = sum(hosp))

by_county_us_2wks7 <- 
  new_dat_us %>% filter(date >= today()-21 & date < today()-14) %>% 
  group_by(administrative_area_level_2, administrative_area_level_3) %>% 
  summarise(cases21 = sum(confirmed), deaths21 = sum(deaths), hosp21 = sum(hosp))


by_county_us_7DA_inc <- 
  left_join(by_county_us_last7, by_county_us_prior7) %>% left_join(by_county_us_2wks7) %>% 
  mutate(case_avginc7 = ((cases7 - cases14) / 7) - ((cases14 - cases21) /7),
         death_avginc7 = ((deaths7 - deaths14) / 7) - ((deaths14 - deaths21) /7),
         hosp_avginc7 = ((hosp7 - hosp14) / 7) - ((hosp14 - hosp21) /7),
         case_pct7 = ((cases14-cases21) / (cases7-cases14)))

glimpse(by_county_us_7DA_inc)
rvsd <- filter(by_county_us_7DA_inc, administrative_area_level_2 == "California" & 
                 administrative_area_level_3 == "Riverside")
rvsd

sbdno <- filter(by_county_us_7DA_inc, administrative_area_level_2 == "California" & 
                  administrative_area_level_3 == "San Bernardino")
sbdno

oc <- filter(by_county_us_7DA_inc, administrative_area_level_2 == "California" &
               administrative_area_level_3 == "Orange")
oc

sac <- filter(by_county_us_7DA_inc, administrative_area_level_2 == "California" &
               administrative_area_level_3 == "Sacramento")
sac


formattable::formattable(by_county_us_7DA_inc)

dc <- filter(by_county_us_7DA_inc, administrative_area_level_2 == "District of Columbia") 






by_state_us <- 
  new_dat_us %>% 
  group_by(administrative_area_level_2) %>% 
  summarise(cases = sum(confirmed), deaths = sum(deaths), hosp = sum(hosp))

by_state_us_last7 <- 
  new_dat_us %>% filter(date >= today()-7) %>% 
  group_by(administrative_area_level_2) %>% 
  summarise(cases7 = sum(confirmed), deaths7 = sum(deaths), hosp7 = sum(hosp))

by_state_us_prior7 <- 
  new_dat_us %>% filter(date >= today()-14 & date < today()-7) %>% 
  group_by(administrative_area_level_2) %>% 
  summarise(cases14 = sum(confirmed), deaths14 = sum(deaths), hosp14 = sum(hosp))

by_state_us_2wks7 <- 
  new_dat_us %>% filter(date >= today()-21 & date < today()-14) %>% 
  group_by(administrative_area_level_2) %>% 
  summarise(cases21 = sum(confirmed), deaths21 = sum(deaths), hosp21 = sum(hosp))


by_state_us_7DA_inc <- 
  left_join(by_state_us_last7, by_state_us_prior7) %>% left_join(by_state_us_2wks7) %>% 
  mutate(case_avginc7 = ((cases7 - cases14) / 7) - ((cases14 - cases21) /7),
         death_avginc7 = ((deaths7 - deaths14) / 7) - ((deaths14 - deaths21) /7),
         hosp_avginc7 = ((hosp7 - hosp14) / 7) - ((hosp14 - hosp21) /7),
         case_pct7 = ((cases14-cases21) / (cases7-cases14)))
