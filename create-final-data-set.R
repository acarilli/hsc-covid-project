library(tidyverse)
library(RCurl) 

# OxCGRT index

OxCGRT <-
  read_csv("https://raw.githubusercontent.com/OxCGRT/USA-covid-policy/master/data/OxCGRT_US_latest.csv") %>% 
  janitor::clean_names() %>% 
  filter(jurisdiction == "STATE_WIDE") %>% 
  select(
    date,
    state = region_code, 
    starts_with("confirmed"), 
    ends_with("index"), 
    -contains("legacy")
  ) %>% 
  mutate(
    date = lubridate::ymd(date),
    state = str_sub(state, -2),
  ) %>% 
  group_by(state) %>% 
  mutate(
    deaths_new = confirmed_deaths - dplyr::lag(confirmed_deaths, 1),
    cases_new = confirmed_cases - dplyr::lag(confirmed_cases, 1),
    deaths_ma = zoo::rollmean(deaths_new, 7, NA, "right"),
    cases_ma = zoo::rollmean(cases_new, 7, NA, "right")
  ) %>% 
  ungroup() %>% 
  rename(cases_confirmed = confirmed_cases,
         deaths_confirmed = confirmed_deaths)
    
         

# latest date with index observations for all states

last_date <-
  OxCGRT %>%
  group_by(state) %>%
  filter(!is.na(stringency_index)) %>%
  summarize(date = max(date)) %>%
  summarize(date = min(date)) %>% 
  pull(date)



# social capital project data

state_index <- 
here::here("data", "social-capital-project-social-capital-index-data .xlsx") %>% 
  readxl::read_xlsx(sheet = "State Index", skip = 2) %>% 
  janitor::clean_names() %>% 
  select(state = state_abbreviation, 6, 10:16)

state_index_indicators <- 
here::here("data", "social-capital-project-social-capital-index-data .xlsx") %>% 
  readxl::read_xlsx(sheet = "State Index Indicators", skip = 2) %>% 
  janitor::clean_names() %>% 
  select(
    state = state_abbreviation, 
    average_number_of_close_friends, 
    percent_neighbors_who_do_favors_at_least_once_mo, 
    percent_who_trust_all_most_neighbors, 
    membership_organizations_p_1_000, 
    non_religious_non_profits_plus_religious_congregations_p_1_000, 
    percent_participated_in_demonstration, 
    percent_who_volunteered_for_a_group, 
    percent_attended_public_meeting, 
    percent_worked_with_neighbors_to_improve_fix_something, 
    percent_served_on_committee_or_as_group_officer,
    percent_have_some_great_confidence_in_media,
    violent_crimes_p_100_000, 
    percent_made_25_contribution_to_charitable_cause
  )

state_benchmarks <-
  here::here("data",
             "social-capital-project-social-capital-index-data .xlsx") %>%
  readxl::read_xlsx(sheet = "State Benchmarks", skip = 2) %>%
  janitor::clean_names()  %>%
  select(
    state = state_abbreviation,
    scp_version_of_penn_state_index,
    putnam_index,
    alesina_la_ferrara_social_capital_group,
    family_prosperity_index,
    unemployment_rate,
    median_household_income,
    poverty_rate,
    gini_coefficient,
    top_5_percent_share_of_hh_income,
    percent_adults_graduated_high_school,
    percent_adults_with_ba,
    percent_in_fair_or_poor_health,
    premature_mortality_rate,
    percent_diabetic,
    percent_obese,
    percent_who_smoke,
    percent_without_health_insurance,
    percent_non_hispanic_white,
    percent_black,
    percent_hispanic,
    percent_american_indian_ak_native,
    percent_asian,
    percent_hawaiian_pacific_islander,
    percent_other,
    percent_multiracial,
    black_white_segregation,
    population,
    density,
    percent_rural,
    average_temperature,
    percent_65,
    percent_under_18_years_old
  )  
  

# Governors political party

governors <-
  read_csv("data/state_politics.csv", skip = 2) %>%
  slice(2:52) %>%
  mutate(
    state = state.abb[match(Location, state.name)],
    state = ifelse(Location == "District of Columbia", "DC", state),
    .before = Location
  ) %>%
  janitor::clean_names() %>%
  select(state, governor_political_affiliation)

# Join data to create covid_data_final.csv

OxCGRT %>% 
  left_join(mobility, by = c("date", "state")) %>% 
  left_join(governors, by = "state") %>% 
  left_join(state_benchmarks, by = "state") %>% 
  left_join(state_index, by = "state") %>% 
  left_join(state_index_indicators, by = "state") %>% 
  left_join(ltc, by = "state") %>% 
  mutate(
    stringency_dummy = ifelse(stringency_index > 50, 1, 0),
    state_level_dummy = ifelse(state_level_index > 0, 1, 0),
    family_unity_dummy = ifelse(family_unity > 0, 1, 0),
    family_interaction_dummy = ifelse(family_interaction > 0, 1, 0),
    social_support_dummy = ifelse(social_support > 0, 1, 0),
    community_health_dummy = ifelse(community_health > 0, 1, 0),
    institutional_health_dummy = ifelse(institutional_health > 0, 1, 0),
    collective_efficacy_dummy = ifelse(collective_efficacy > 0, 1, 0),
    philanthropic_health_dummy = ifelse(philanthropic_health > 0, 1, 0),
    cases_new = 1000000 * cases_new / population,
    cases_ma = 1000000 * cases_ma / population,
    cases_confirmed_pc = 1000000 * cases_confirmed / population,
    deaths_new = 1000000 * deaths_new / population,
    deaths_ma = 1000000 * deaths_ma / population,
    deaths_confirmed_pc = 1000000 * deaths_confirmed / population,
  ) %>% 
  group_by(state) %>% 
  mutate(stringency_index_lag = dplyr::lag(stringency_index, 14),
         neighbor_dummy = if_else(percent_who_trust_all_most_neighbors > 50, 1, 0)) %>% 
  ungroup() %>% 
  filter(date >= "03-01-2020" & date <= last_date) %>% 
  write_csv(here::here("data", "covid_data_final.csv"))
