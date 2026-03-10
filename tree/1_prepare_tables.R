library(tidyverse)

setwd("/Users/rujalshrestha/Projects/nz-tourism/tree")

survey <- read_csv("../data/survey_main_header.csv")
decision <- read_csv("../data/decision_making_process.csv")
satisfaction <- read_csv("../data/visitor_satisfaction.csv")
accommodation <- read_csv("../data/accommodation.csv")
activities <- read_csv("../data/activities.csv")
transport <- read_csv("../data/transport_methods.csv")
self_transport <- read_csv("../data/self_transport.csv")
poor_exp <- read_csv("../data/poor_experiences.csv")
environment <- read_csv("../data/environment.csv")
itinerary <- read_csv("../data/itinerary.csv")
travel_party <- read_csv("../data/travel_party.csv")
expenditure <- read_csv("../data/expenditure_by_industry.csv")
ease <- read_csv("../data/ease_of_organisation.csv")
maori_experience <- read_csv("../data/maori_cultural_experience.csv")
maori_sentiment <- read_csv("../data/maori_cultural_sentiment.csv")
mobility <- read_csv("../data/mobility.csv")
other_countries <- read_csv("../data/other_countries_visited.csv")


# filter only following columns
survey_clean <- survey |>
  select(
    # "year",
    # "qtr",
    # "date",
    "response_id",
    # "identifier",
    # "interview_date",
    "airport",
    "country_of_residence",
    # "country_of_residence_group",
    # "au_state_of_origin",
    "gender",
    "first_nz_trip",
    "purpose_of_visit_main",
    "purpose_subtype",
    "arrival_method",
    "arrival_date",
    "no_days_in_nz",
    # "no_days_in_nz_unknown",
    # "no_selection_itinerary",
    "package_deal",
    # "pkg_included_airfare",
    # "country_package_started",
    # "trip_start_country",
    # "incl_stay_other_country",
    # "no_nights_other_country",
    # "single_or_others",
    # "no_people_over_15",
    # "no_people_under_15",
    # "currency",
    # "other_purchase",
    "age_range",
    # "visited_ni",
    # "visited_si",
    "travel_type",
    # "main_transport_type",
    "sustainability_considered",
    "treated_spend",
    # "population_weight",
    # "psu",
    # "vem_pop_weight"
  )

# map places stayed into regions

region_mapping <- list(

  # NORTH ISLAND
  region_northland = c(
    "Far North District",
    "Whangarei District",
    "Kaipara District"
  ),
  region_auckland = c(
    "Auckland District"
  ),
  region_waikato = c(
    "Hamilton City",
    "Waikato District",
    "Matamata-Piako District",
    "Waipa District",
    "Otorohanga District",
    "South Waikato District",
    "Waitomo District",
    "Hauraki District",
    "Taupo District",
    "Thames-Coromandel District"
  ),
  region_bay_of_plenty = c(
    "Tauranga City",
    "Western Bay of Plenty District",
    "Rotorua District",
    "Whakatane District",
    "Kawerau District",
    "Opotiki District"
  ),
  region_gisborne = c(
    "Gisborne District"
  ),
  region_hawkes_bay = c(
    "Napier City",
    "Hastings District",
    "Central Hawke's Bay District",
    "Wairoa District"
  ),
  region_taranaki = c(
    "New Plymouth District",
    "Stratford District",
    "South Taranaki District"
  ),
  region_manawatu_whanganui = c(
    "Palmerston North City",
    "Manawatu District",
    "Whanganui District",
    "Rangitikei District",
    "Tararua District",
    "Horowhenua District",
    "Ruapehu District"
  ),
  region_wellington = c(
    "Wellington City",
    "Porirua City",
    "Hutt City",
    "Upper Hutt City",
    "Kapiti Coast District",
    "Masterton District",
    "Carterton District",
    "South Wairarapa District"
  ),

  # SOUTH ISLAND
  region_tasman = c(
    "Tasman District"
  ),
  region_nelson = c(
    "Nelson City"
  ),
  region_marlborough = c(
    "Marlborough District"
  ),
  region_west_coast = c(
    "Buller District",
    "Grey District",
    "Westland District"
  ),
  region_canterbury = c(
    "Christchurch City",
    "Selwyn District",
    "Waimakariri District",
    "Hurunui District",
    "Ashburton District",
    "Kaikoura District",
    "Timaru District",
    "Mackenzie District",
    "Waimate District"
  ),
  region_otago = c(
    "Dunedin City",
    "Central Otago District",
    "Clutha District",
    "Waitaki District",
    "Queenstown-Lakes District (Queenstown-Whakatipu and Arrowtown-Kawarau Ward)",
    "Queenstown-Lakes District (Wanaka-Upper Clutha Ward)"
  ),
  region_southland = c(
    "Invercargill City",
    "Gore District",
    "Southland District (Central and Eastern)",
    "Southland District (Fiordland)"
  )
)

# convert above named list to a lookup dataframe
region_lookup <- stack(region_mapping) |>
  rename(place_stayed = values, region = ind) |>
  mutate(region = as.character(region))

region_visits <- itinerary |>
  filter(place_stayed != "Other") |>
  left_join(region_lookup, by = "place_stayed") |>
  filter(!is.na(region)) |>
  mutate(value = 1) |>
  select(response_id, region, value) |>
  distinct() |>
  pivot_wider(
    names_from = region,
    values_from = value,
    values_fill = 0
  )

# region_visits contains 24,820 rows with 17 columns for regions visited
# by each respondent
dim(region_visits)

# output to file
# write_csv(region_visits, "output/region_visits_processed.csv")



#########################
# TRAVEL PARTY
#########################

travel_party_processed <- travel_party |>
  mutate(value = 1) |>
  pivot_wider(
    names_from = travelled_with,
    values_from = value,
    values_fill = 0
  ) |>
  janitor::clean_names()

# TRAVEL_PARTY$TRAVELLED_WITH VALUES:
# Child/children aged under 15
# My husband, wife or partner
# No one, I was on my own
# Other adult(s) who are not family / relatives
# Child/children aged 15 or older
# Other adult family / relative

# write_csv(travel_party_processed, "output/travel_party_processed.csv")


#########################
# TRANSPORT
#########################

transport |>
  count(transport_method) |>
  arrange(desc(n))

# 10218  Rental car
# 8847   Taxi / shuttle service
# 7607   Car or van owned by you / family / friend(s) / company
# 5934   App-based services such as Uber, Ola, etc
# 5453   Plane (within New Zealand)
# 5168   Local bus service
# 3552   The ferry between the North Island and the South Island
# 3414   Tour bus
# 2874   Other ferry
# 2497   Other boat or ship
# 2194   Bus service between towns / cities
# 1690   Rental campervan / motor-home
# 1578   Scenic trains
# 1393   Bicycle
# 1304   Helicopter
# 1282   Train
# 798    Other bus service
# 750    Limousine / car with driver included
# 615    Other type of transport
# 386    Campervan / motor-home owned by you / family / friend(s)
# 293    Hitch-hiking
# 228    Yacht
# 199    Motorcycle
# 157    Not sure

transport_oh <- transport |>
  mutate(value = 1) |>
  pivot_wider(
    names_from = transport_method,
    values_from = value,
    values_fill = 0
  ) |>
  janitor::clean_names()

self_transport |>
  count(drive_yourself) |>
  arrange(desc(n))

# Yes             13905
# No              4994
# Can’t remember  42

dim(self_transport)
# 18,941 rows

self_transport_clean <- self_transport |>
  distinct(response_id, drive_yourself) |>
  mutate(drove_themselves = as.integer(drive_yourself == "Yes")) |>
  select(response_id, drove_themselves)

dim(self_transport_clean)
# 17,786 rows

dim(transport_oh)
# 23,356 rows

transport_total <- transport_oh |>
  left_join(self_transport_clean, by = "response_id") |>
  mutate(drove_themselves = replace_na(drove_themselves, 0L))

# write_csv(transport_total, "output/transport_processed.csv")


#########################
# PURPOSE
#########################


survey |>
  select(purpose_of_visit_main) |>
  distinct()

# Visiting friends / relatives
# Holiday / vacation
# Other
# Business
# Conference / convention
# Education

survey |>
  select(purpose_subtype) |>
  distinct()

# To visit family
# To visit friends
# Just for the holiday
# To care for a family member or friend
# For a wedding, funeral, other family occasion
# For some other reason
# NA (Business)
# For some other reason (please specify)
# For your honeymoon
# To go sightseeing
# For some special event or occasion (non-sport-related)
# Related to your job or business
# To watch a sporting event
# For a working holiday
# To do other work
# As an incentive trip as a reward for good work or sales
# To take part (participate) in a sporting event
# Not related to your job or business
# For a stop-over – between flights
# To do seasonal work
# On a school education trip
# Some other educational purpose
# An exchange program

survey |>
  select(response_id, purpose_of_visit_main, purpose_subtype) |>
  filter(purpose_of_visit_main == "Other") |>
  count(purpose_subtype) |>
  arrange(desc(n))

survey_clean %>%
  count(purpose_subtype) %>%
  arrange(n)

# create new visit_purpose column using purpose_subtype
# group 2 sports related subtypes
# group sparse subtypes into 'Other'

sports_related <- c(
  "To watch a sporting event",
  "To take part (participate) in a sporting event"
)

sparse_purposes <- c(
  "To do seasonal work",
  "For a stop-over – between flights",
  "As an incentive trip as a reward for good work or sales",
  "Not related to your job or business",
  "To do other work",
  "An exchange program",
  "On a school education trip",
  "Some other educational purpose"
)

survey_clean_purpose <- survey_clean |>
  mutate(visit_purpose = case_when(
    is.na(purpose_subtype) ~ "Business",
    purpose_subtype %in% sports_related ~ "Sports related",
    purpose_subtype %in% sparse_purposes ~ "Other",
    # distinguish the two "For some other reason" groups by main purpose
    purpose_subtype == "For some other reason (please specify)" ~ "Holiday - Other",
    purpose_subtype == "For some other reason" & purpose_of_visit_main == "Visiting friends / relatives" ~ "Visiting Friends/Relatives - Other",
    purpose_subtype == "For some other reason" & purpose_of_visit_main == "Other" ~ "Other",
    TRUE ~ purpose_subtype
  )) |>
  select(-purpose_of_visit_main, -purpose_subtype)

survey_clean_purpose |>
  count(visit_purpose) |>
  arrange(desc(n))

# 7054.   Just for the holiday
# 6510.   To visit family
# 5682.   To go sightseeing
# 1454.   Business
# 1404.   To visit friends
# 1238.   Holiday - Other
# 1196.   For a wedding, funeral, other family occasion
# 957.    Other
# 950.    For your honeymoon
# 579.    For a working holiday
# 571.    To care for a family member or friend
# 533.    For some special event or occasion (non-sport-related)
# 504.    Related to your job or business
# 218.    Visiting Friends/Relatives - Other
# 150.    Sports related


#########################
# TREATED SPEND
#########################

spend_analysis <- survey |>
  select(
    response_id,
    single_or_others,
    no_people_over_15,
    no_people_under_15,
    treated_spend,
  ) |>
  left_join(expenditure, by = "response_id")

options(scipen = 999)
summary(spend_analysis$treated_spend)

#   Min.    1st Qu.     Median       Mean    3rd Qu.         Max.
#  5.338   1,701.272   3,355.218   4,853.544   6,160.786   114,675.376

# plot to see treated_spend distribution
spend_analysis |>
  ggplot(aes(x = treated_spend)) +
  geom_histogram(bins = 50) +
  scale_x_log10() +
  labs(
    title = "Distribution of treated_spend by reporting scope",
    x = "treated_spend (log scale)"
  )

spendings <- spend_analysis |>
  mutate(
    single_or_grouped = if_else(
      single_or_others == "What the visit to NZ cost just for yourself",
      "Single",
      "Grouped"
    ),
    cost_sum = cost_accomm + cost_dom_travel + cost_food_drink +
      cost_entertainment + cost_shopping + cost_other + cost_tour_package +
      cost_dom_flights + cost_car_rentals + cost_eating_out + cost_day_cruise,
    diff = round(treated_spend - cost_sum),
  ) |>
  select(
    single_or_grouped,
    no_people_under_15,
    no_people_over_15,
    treated_spend,
    cost_sum,
    diff
  )

# per head spending distribution of people who came single vs grouped

spendings |>
  group_by(single_or_grouped) |>
  summarise(
    mean = mean(treated_spend),
    median = median(treated_spend)
  )

# turns out treated spend is actually already 'treated', can use as is


#########################
# ARRIVAL DATE
#########################

survey_clean_arrival <- survey_clean_purpose |>
  filter(!is.na(arrival_date)) |> # dropped 664 rows
  mutate(
    arrival_date = as.Date(arrival_date),
    arrival_month = month(arrival_date),
    arrival_year = year(arrival_date),
    arrival_season = case_when(
      arrival_month %in% c(12, 1, 2) ~ "Summer",
      arrival_month %in% c(3, 4, 5) ~ "Autumn",
      arrival_month %in% c(6, 7, 8) ~ "Winter",
      arrival_month %in% c(9, 10, 11) ~ "Spring",
    )
  )


#########################
# PACKAGE DEAL
#########################

survey_clean_arrival |>
  count(package_deal) |>
  arrange(desc(n))

# 24479  No
# 3247   Yes
# 312    NA
# 298    Not sure

survey_clean_package <- survey_clean_arrival |>
  filter(!is.na(package_deal))


#########################
# FIRST NZ TRIP
#########################

survey_clean_first_trip <- survey_clean_package |>
  filter(!is.na(first_nz_trip))

survey_clean_first_trip |>
  count(first_nz_trip) |>
  arrange(desc(n))

# No 14293
# Yes 13723


#########################
# AGE RANGE
#########################

survey_clean_first_trip |>
  count(age_range) |>
  arrange(desc(n))

# 3325.   30 - 34
# 3135.   25 - 29
# 3037.   60 - 64
# 2661.   65 - 69
# 2635.   55 - 59
# 2414.   50 - 54
# 2281.   35 - 39
# 2000.   40 - 44
# 1943.   45 - 49
# 1643.   20 - 24
# 1514.   70 - 74
# 863.    75 or older
# 483.    Under 20
# 82.     Rather not say

dim(survey_clean_first_trip)
# 27,934 rows

survey_clean_age <- survey_clean_first_trip |>
  filter(age_range != "Rather not say") |> # 82 rows dropped
  mutate(age_range = factor(age_range, levels = c(
    "Under 20", "20 - 24", "25 - 29", "30 - 34", "35 - 39",
    "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
    "65 - 69", "70 - 74", "75 or older"
  ), ordered = TRUE))


# count NAs in each column
survey_clean_age |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "na_count"
  ) |>
  arrange(desc(na_count))

# saveRDS(survey_clean_age, "output/survey_cleaned.rds")
# write_csv(survey_clean_age, "output/survey_cleaned.csv")


#########################
# ACCOMODATION
#########################

accommodation |>
  count(accomm_type_used) |>
  arrange(desc(n))

# 9381  Hotel
# 9255  Staying with family or friends
# 8059  House/ flat/ apartment booked through an online website (including Booking.com, AirBnB, HomeSwap, HomeStay, Hotel.com, Expedia, etc)
# 4727  Motel, Motor Inn or Serviced Apartment
# 2401  Other (paid) camping ground / holiday park (where you can stay in a tent, cabin, caravan, or campervan / motorhome)
# 2055  Luxury Accommodation, 5-star Hotel, Luxury Lodge
# 1561  Bed and Breakfast
# 1545  Backpackers
# 1425  Free camping - staying in a tent, caravan, campervan / motorhome
# 1318  House / flat that you paid some rent for
# 1305  Camping at a National Park / Department of Conservation camping ground
# 1074  Youth Hostel, YMCA, YWCA
# 727   Another place where you pay to park a caravan or campervan / motorhome overnight
# 632   Farm-stay or Home-stay
# 553   In a hut at a National Park / Department of Conservation area
# 339   Other accommodation
# 275   None of these
# 259   A house/ flat/ apartment/ timeshare you own
# 237   Yacht or other boat
# 78    Not sure
# 69    Student residence
# 61    Marae

accomm_to_group <- c("Marae", "Student residence", "Other accommodation")

accommodation_processed <- accommodation |>
  filter(accomm_type_used != "Not sure") |> # 78 rows removed
  mutate(accomm_grouped = case_when(
    accomm_type_used %in% accomm_to_group ~ "Other accommodation",
    TRUE ~ accomm_type_used
  )) |>
  mutate(value = 1) |>
  select(response_id, accomm_grouped, value) |>
  pivot_wider(
    names_from = accomm_grouped,
    values_from = value,
    values_fill = 0,
    values_fn = max
  ) |>
  janitor::clean_names()

# write_csv(accommodation_processed, "output/accommodation_processed.csv")



#########################
# DECISION MAKING PROCESS
#########################

decision |>
  count(factor_for_visit) |>
  arrange(desc(n))

# 9939  Its landscapes and scenery
# 8701  I wanted to visit friends or family in New Zealand
# 5813  I've always wanted to visit
# 4568  It was somewhere new, I had never been there before
# 4001  New Zealand was a safe place to visit as it is less crowded than most other places
# 3978  The variety of outdoor and adventure activities
# 3640  Friends, family or colleagues talked about or recommended New Zealand
# 2965  Its environmentally friendly image
# 2853  The Hobbit and Lord of the Ring Movies
# 2104  I wanted to see the unique indigenous Maori culture
# 1738  A specific event brought me to New Zealand e.g. sporting or cultural event, wedding, family event, etc
# 1705  New Zealand's food and wine
# 1661  I read about New Zealand in online articles, travel forums, blogs, social networking sites, etc
# 1634  Something else
# 1443  Work / business
# 867  I saw or found a good deal on flights to New Zealand/ a good travel package to New Zealand
# 832  Sports events or activities
# 669  I was confident there would be health and hygiene measures in New Zealand to help protect against COVID-19
# 620  I saw a show or segment featuring New Zealand on TV
# 537  I read about New Zealand in a newspaper, or magazine
# 417  Other movies that were filmed in New Zealand
# 369  None of these
# 172  My travel agent talked about or recommended New Zealand

decision_processed <- decision |>
  mutate(
    factor_for_visit = case_when(
      str_detect(factor_for_visit, "^Something else") ~ "Something else",
      TRUE ~ factor_for_visit
    )
  ) |>
  mutate(value = 1) |>
  select(response_id, factor_for_visit, value) |>
  pivot_wider(
    names_from = factor_for_visit,
    values_from = value,
    values_fill = 0,
    values_fn = max
  ) |>
  janitor::clean_names()

# write_csv(decision_processed, "output/decision_process_processed.csv")



#########################
# EASE OF ORGANIZATION
#########################

ease |>
  count(organisation_ease) |>
  arrange(desc(n))

# 9972   3 - Easy
# 9172   4 - Very easy
# 2327   2 - Somewhat difficult
# 390    Not sure
# 151    1 - Very difficults

ease_cleaned <- ease |>
  filter(organisation_ease != "Not sure") |>
  mutate(organisation_ease = factor(organisation_ease,
    levels = c(
      "1 - Very difficult",
      "2 - Somewhat difficult",
      "3 - Easy",
      "4 - Very easy"
    ), ordered = TRUE
  ))

# write_csv(ease_cleaned, "output/ease_cleaned.csv")
# saveRDS(ease_cleaned, "output/ease_cleaned.rds")



#########################
# ENVIRONMENT
#########################

environment |>
  count(experience)

# Availability of public facilities (toilets, rubbish bins, etc)
# Feeling of safety
# Feeling welcomed
# Natural scenery/ wilderness
# Protection of natural resources
# Protection of wildlife (whales, penguins, albatross, kiwi, etc)
# Quality of drinking water
# Quality of flowing water (rivers, streams, sea)
# Quality of the air

environment |>
  count(rating)

environment_processed <- environment |>
  filter(rating != "Don't know / Not applicable") |>
  mutate(rating = factor(rating, levels = c(
    "1 - Very Poor",
    "2 - Poor",
    "3 - Neither good nor poor",
    "4 - Good",
    "5 - Very Good"
  ), ordered = TRUE)) |>
  pivot_wider(
    names_from = "experience",
    values_from = "rating"
  ) |>
  janitor::clean_names()

# write_csv(environment_processed, "output/environment_processed.csv")
# saveRDS(environment_processed, "output/environment_processed.rds")


##################################################
# MAORI CULTURAL EXPERIENCE
##################################################

maori_experience_processed <- maori_experience |>
  mutate(value = 1) |>
  pivot_wider(
    names_from = experience,
    values_from = value,
    values_fill = 0,
    values_fn = max
  ) |>
  janitor::clean_names()

# write_csv(maori_experience_processed, "output/maori_experience_processed.csv")


##################################################
# MAORI CULTURAL SENTIMENT
##################################################

maori_sentiment_processed <- maori_sentiment |>
  mutate(
    experience_more_maori_culture = factor(experience_more_maori_culture,
      levels = c("Disagree", "Don't know / Not sure", "Agree"),
      ordered = TRUE
    ),
    improve_maori_culture_understanding = ifelse(
      improve_maori_culture_understanding == "Don't know / Not sure",
      NA, improve_maori_culture_understanding
    ),
    improve_maori_culture_understanding = factor(
      improve_maori_culture_understanding,
      levels = c("Disagree", "Agree"),
      ordered = TRUE
    ),
    enjoy_maori_culture_experience = ifelse(
      enjoy_maori_culture_experience == "Don't know / Not sure",
      NA, enjoy_maori_culture_experience
    ),
    enjoy_maori_culture_experience = factor(enjoy_maori_culture_experience,
      levels = c("Disagree", "Agree"),
      ordered = TRUE
    )
  )

# write_csv(maori_sentiment_processed, "output/maori_sentiment_processed.csv")
# saveRDS(maori_sentiment_processed, "output/maori_sentiment_processed.rds")



##################################################
# EASE OF MOBILITY
##################################################

mobility |> count(rating)

# 170      1 - Cannot do at all
# 553      2 - A lot of difficulty
# 7195     3 - Some difficulty
# 85434    4 - No difficulty
# 1532     Prefer not to say

mobility_processed <- mobility |>
  mutate(
    rating = ifelse(rating == "Prefer not to say", NA, rating),
    rating = factor(rating, levels = c(
      "1 - Cannot do at all",
      "2 - A lot of difficulty",
      "3 - Some difficulty",
      "4 - No difficulty"
    ), ordered = TRUE)
  ) |>
  pivot_wider(
    names_from = mobility_difficulty,
    values_from = rating
  ) |>
  janitor::clean_names()

# write_csv(mobility_processed, "output/mobility_processed.csv")
# saveRDS(mobility_processed, "output/mobility_processed.rds")



##################################################
# VISITED OTHER COUNTRIES
##################################################

other_countries_processed <- other_countries |>
  group_by(response_id) |>
  summarise(
    visited_other_countries = 1L,
    no_countries_visited = n_distinct(country),
    visited_before = as.integer(any(visited_before_or_after == "Before")),
    visited_after = as.integer(any(visited_before_or_after == "After"))
  )

# write_csv(other_countries_processed, "output/other_countries_processed.csv")


##################################################
# POOR EXPERIENCES
##################################################


poor_exp |> count(frequency)

# 99309    1 - Never
# 42582    2 - Sometimes
# 3305     3 - Most of the times
# 979      4 - Always
# 7286     Don't know/ Not applicable


poor_experiences_processed <- poor_exp |>
  mutate(
    frequency = ifelse(
      frequency == "Don't know/ Not applicable", NA, frequency
    ),
    frequency = factor(frequency, levels = c(
      "1 - Never",
      "2 - Sometimes",
      "3 - Most of the times",
      "4 - Always"
    ), ordered = TRUE)
  ) |>
  pivot_wider(
    names_from = type,
    values_from = frequency
  ) |>
  janitor::clean_names()

# write_csv(poor_experiences_processed, "output/poor_experiences_processed.csv")
# saveRDS(poor_experiences_processed, "output/poor_experiences_processed.rds")
