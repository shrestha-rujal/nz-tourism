library(tidyverse)

survey <- read_csv("data/survey_main_header.csv")
satisfaction <- read_csv("data/visitor_satisfaction.csv")
accommodation <- read_csv("data/accommodation.csv")
activities <- read_csv("data/activities.csv")
transport <- read_csv("data/transport_methods.csv")
self_transport <- read_csv("data/self_transport.csv")
poor_exp <- read_csv("data/poor_experiences.csv")
environment <- read_csv("data/environment.csv")
itinerary <- read_csv("data/itinerary.csv")
travel_party <- read_csv("data/travel_party.csv")

# filter only following columns

survey_clean <- survey |>
  select(
    "year",
    "qtr",
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
    "single_or_others",
    # "no_people_over_15",
    # "no_people_under_15",
    # "currency",
    # "other_purchase",
    "age_range",
    # "visited_ni",
    # "visited_si",
    "travel_type",
    "main_transport_type",
    "sustainability_considered",
    "treated_spend",
    "population_weight",
    "psu",
    "vem_pop_weight"
  )

# count NAs in each column
survey_clean |>
  summarise(across(everything(), ~ sum(is.na(.)))) |>
  pivot_longer(
    everything(),
    names_to = "column",
    values_to = "na_count"
  ) |>
  arrange(desc(na_count))

itinerary |>
  select(place_stayed) |>
  unique() |>
  count()

itinerary |>
  filter(place_stayed == "Other") |>
  count()


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
# write_csv(region_visits, "output/region_visits.csv")

# select survey columns
# pivot travel_party wider
# left join

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
# 2497   1Other boat or ship
# 2194   1Bus service between towns / cities
# 1690   1Rental campervan / motor-home
# 1578   1Scenic trains
# 1393   1Bicycle
# 1304   1Helicopter
# 1282   1Train
# 798    1Other bus service
# 750    1Limousine / car with driver included
# 615    1Other type of transport
# 386    2Campervan / motor-home owned by you / family / friend(s)
# 293    2Hitch-hiking
# 228    2Yacht
# 199    2Motorcycle
# 157    2Not sure

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
