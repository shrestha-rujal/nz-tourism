library(tidyverse)
library(forcats)
library(scales)
library(patchwork)
library(sf)
library(corrplot)
library(fastDummies)

# setwd(paste0(getwd(), "/decision-tree"))

df <- readRDS("output/merged/model_data.rds")

output_path <- function(filename) {
  output_dir <- "results/plots/eda/"
  paste0(output_dir, filename)
}

X11()

# hotfix (same as in script 4)
df <- df |>
  mutate(country_of_residence = fct_lump_min(country_of_residence, min = 10))

# ── Common theme ───────────────────────────────────────────────────────────────
theme_ivs <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 8)),
      plot.subtitle = element_text(colour = "grey40", size = 12, margin = margin(b = 12)),
      plot.caption = element_text(colour = "grey55", size = 10, margin = margin(t = 10)),
      axis.title = element_text(size = 12),
      panel.grid.minor = element_blank()
    )
}

dir.create("results/plots/eda", recursive = TRUE, showWarnings = FALSE)


# distribution of recommend_rating

p1 <- df |>
  count(recommend_rating) |>
  mutate(pct = n / sum(n)) |>
  ggplot(aes(x = factor(recommend_rating), y = pct)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = percent(pct, accuracy = 0.1)),
    vjust = -0.4, size = 5.5
  ) +
  labs(
    title = "Recommendation Ratings Distribution (1-10 scale)",
    subtitle = "10 being extremely likely to recommend",
    x = "Recommendation Rating",
    y = "Percent of Respondants",
    caption = "Source: IVS Microdata 2022-2025"
  ) +
  theme_ivs()

# ggsave(
#   paste0(B, "recommend_rating_distribution.png"),
#   p1,
#   dpi = 300
# )


# regions visited spatial

region_visitor_counts <- df |>
  select(response_id, starts_with("itinerary_region_")) |>
  pivot_longer(
    cols      = starts_with("itinerary_region_"),
    names_to  = "region_col",
    values_to = "visited"
  ) |>
  filter(!is.na(visited), visited == 1) |>
  mutate(region_col = str_remove(region_col, "itinerary_region_")) |>
  count(region_col, name = "visitor_count")

region_name_map <- tibble(
  region_col = c(
    "auckland", "bay_of_plenty", "canterbury", "gisborne",
    "hawkes_bay", "manawatu_whanganui", "marlborough", "nelson",
    "northland", "otago", "southland", "taranaki", "tasman",
    "waikato", "wellington", "west_coast"
  ),
  region_name = c(
    "Auckland", "Bay of Plenty", "Canterbury", "Gisborne",
    "Hawke's Bay", "Manawatū-Whanganui", "Marlborough", "Nelson",
    "Northland", "Otago", "Southland", "Taranaki", "Tasman",
    "Waikato", "Wellington", "West Coast"
  )
)

region_plot_data <- region_visitor_counts |>
  left_join(region_name_map, by = "region_col")

# Load shapefile — update path to wherever you saved it
nz_regions <- sf::st_read("../data/regional_shp/regional-council-2023-generalised.shp",
  quiet = TRUE
)

# Join visitor counts to shapefile
nz_map_data <- nz_regions |>
  left_join(region_plot_data, by = c("REGC2023_1" = "region_name"))

p5 <- ggplot(nz_map_data) +
  geom_sf(aes(fill = visitor_count), colour = "black", linewidth = 0.3) +
  scale_fill_gradientn(
    colours  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    na.value = "grey85",
    labels   = comma_format(),
    name     = "Respondents\nwho visited"
  ) +
  labs(
    title = "Visitor Distribution Across NZ Regions",
    subtitle = "Number of survey respondents who stayed at least one night in each region",
    caption = "Source: IVS Microdata 2022–2025. Boundaries: Stats NZ 2023"
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 6)),
    plot.subtitle = element_text(colour = "grey40", size = 12, margin = margin(b = 10)),
    plot.caption = element_text(colour = "grey55", size = 10, margin = margin(t = 8)),
    legend.position = "right"
  )

p5

# ggsave(output_path("nz_regions_visits.png"),
#   p5,
#   dpi = 300, bg = "white"
# )

# country of hist

p6 <- df |>
  count(country_of_residence, name = "n") |>
  filter(country_of_residence != "Other") |> # fct_lump_min lumped category
  slice_max(n, n = 15) |>
  mutate(country_of_residence = fct_reorder(country_of_residence, n)) |>
  ggplot(aes(x = country_of_residence, y = n)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = comma(n)), hjust = -0.15, size = 3.5) +
  coord_flip() +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Top 15 Countries of Residence",
    x        = NULL,
    y        = "Number of Respondents",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p6

ggsave(output_path("country_of_origin.png"),
  p6,
  width = 11, height = 7, dpi = 300
)


# purpose vs satisfaction rating

p8 <- df |>
  filter(!is.na(visit_purpose), !is.na(satisfaction_rating)) |>
  group_by(visit_purpose) |>
  summarise(
    mean_reco = mean(satisfaction_rating, na.rm = TRUE),
    n         = n(),
    .groups   = "drop"
  ) |>
  mutate(
    visit_purpose = str_wrap(visit_purpose, width = 25),
    visit_purpose = fct_reorder(visit_purpose, mean_reco)
  ) |>
  ggplot(aes(x = visit_purpose, y = mean_reco)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.2f\n(n=%s)", mean_reco, comma(n))),
    hjust = -0.1, size = 3.2, lineheight = 0.9
  ) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 11), expand = expansion(mult = c(0, 0))) +
  labs(
    title    = "Mean Satisfaction Rating by Purpose of Visit",
    subtitle = "Honeymoon and holiday visitors tend to rate higher than business/VFR visitors",
    x        = NULL,
    y        = "Mean Satisfaction Rating",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p8

ggsave(output_path("purpose_vs_satisfaction.png"),
  p8,
  width = 12, height = 8, dpi = 300
)


# KDE treated_spend


p9 <- df |>
  filter(!is.na(treated_spend), treated_spend > 0) |>
  ggplot(aes(x = treated_spend)) +
  geom_density(fill = "steelblue", alpha = 0.6, colour = "steelblue4") +
  geom_vline(
    xintercept = median(df$treated_spend, na.rm = TRUE),
    colour = "#d73027", linetype = "dashed", linewidth = 0.9
  ) +
  annotate("text",
    x = median(df$treated_spend, na.rm = TRUE) * 1.15,
    y = Inf, vjust = 1.5, hjust = 0,
    label = paste0("Median: NZD ", comma(round(median(df$treated_spend, na.rm = TRUE)))),
    colour = "#d73027", size = 4
  ) +
  scale_x_log10(labels = dollar_format(prefix = "NZD ")) +
  labs(
    title    = "Distribution of Visitor Spending (KDE)",
    subtitle = "Log scale — right tail shows high-spending outliers",
    x        = "Total Treated Spend (NZD, log scale)",
    y        = "Density",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p9

ggsave(output_path("kde_treated_spend.png"),
  p9,
  width = 11, height = 6, dpi = 300, bg = "white"
)


# median spending by age group


age_levels <- c(
  "Under 20", "20 - 24", "25 - 29", "30 - 34", "35 - 39",
  "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
  "65 - 69", "70 - 74", "75 or older"
)

p10 <- df |>
  filter(!is.na(age_range), !is.na(treated_spend), treated_spend > 0) |>
  group_by(age_range) |>
  summarise(
    median_spend = median(treated_spend, na.rm = TRUE),
    mean_spend = mean(treated_spend, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  ggplot(aes(x = age_range, y = median_spend)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_point(aes(y = mean_spend),
    colour = "#d73027", size = 3,
    shape = 18
  ) +
  geom_text(aes(label = dollar(round(median_spend), prefix = "NZD ")),
    vjust = -0.5, size = 3.2
  ) +
  scale_y_continuous(
    labels = dollar_format(prefix = "NZD "),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(
    title    = "Median Visitor Spend by Age Group",
    subtitle = "Bars = median spend. Red diamond = mean spend (sensitive to outliers)",
    x        = "Age Range",
    y        = "Spend (NZD)",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p10

ggsave(output_path("spend_by_age.png"),
  p10,
  width = 13, height = 7, dpi = 300, bg = "white"
)


# median spending by country


p11 <- df |>
  filter(
    !is.na(country_of_residence), !is.na(treated_spend),
    treated_spend > 0, country_of_residence != "Other"
  ) |>
  group_by(country_of_residence) |>
  summarise(
    median_spend = median(treated_spend, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |>
  slice_max(n, n = 12) |>
  mutate(country_of_residence = fct_reorder(country_of_residence, median_spend)) |>
  ggplot(aes(x = country_of_residence, y = median_spend)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_text(aes(label = dollar(round(median_spend), prefix = "NZD ")),
    hjust = -0.1, size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = dollar_format(prefix = "NZD "),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title    = "Median Visitor Spend by Country of Origin (Top 12)",
    subtitle = "Countries ranked by number of respondents",
    x        = NULL,
    y        = "Median Spend (NZD)",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p11

ggsave(output_path("spend_by_country.png"),
  p11,
  width = 11, height = 7, dpi = 300, bg = "white"
)


# country of origin stacked by gender


top10_countries <- df |>
  count(country_of_residence) |>
  slice_max(n, n = 10) |>
  pull(country_of_residence)

p12 <- df |>
  filter(
    country_of_residence %in% top10_countries,
    gender %in% c("Male", "Female")
  ) |>
  count(country_of_residence, gender) |>
  group_by(country_of_residence) |>
  mutate(
    total = sum(n),
    pct   = n / total
  ) |>
  ungroup() |>
  mutate(country_of_residence = fct_reorder(country_of_residence, total)) |>
  ggplot(aes(x = country_of_residence, y = n, fill = gender)) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 0.1)),
    position = position_stack(vjust = 0.5),
    size = 3.2, colour = "white", fontface = "bold"
  ) +
  coord_flip() +
  scale_fill_manual(values = c("Female" = "#c1666b", "Male" = "#4a7c9e")) +
  scale_y_continuous(labels = comma_format()) +
  labs(
    title    = "Visitor Counts by Country of Origin, Split by Gender (Top 10)",
    subtitle = "Bar height = total visitors. Labels = gender % within each country.",
    x        = NULL,
    y        = "Number of Respondents",
    fill     = "Gender",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs() +
  theme(legend.position = "bottom")

p12

ggsave(output_path("country_by_gender.png"),
  p12,
  width = 19, height = 7, dpi = 300, bg = "white"
)


# days spent kde/boxplots


p13_density <- df |>
  filter(!is.na(no_days_in_nz), no_days_in_nz <= 90) |>
  ggplot(aes(x = no_days_in_nz)) +
  geom_histogram(
    binwidth = 1, fill = "steelblue", alpha = 0.8,
    colour = "white"
  ) +
  geom_vline(
    xintercept = median(df$no_days_in_nz, na.rm = TRUE),
    colour = "#d73027", linetype = "dashed", linewidth = 0.9
  ) +
  annotate("text",
    x = median(df$no_days_in_nz, na.rm = TRUE) + 1,
    y = Inf, vjust = 1.5, hjust = 0,
    label = paste0("Median: ", median(df$no_days_in_nz, na.rm = TRUE), " days"),
    colour = "#d73027", size = 4
  ) +
  scale_x_continuous(breaks = seq(0, 90, by = 7)) +
  labs(x = "Days in NZ", y = "Count") +
  theme_ivs()

p13_box <- df |>
  filter(!is.na(no_days_in_nz), no_days_in_nz <= 90) |>
  ggplot(aes(x = no_days_in_nz, y = "")) +
  geom_boxplot(fill = "steelblue", alpha = 0.6, outlier.alpha = 0.2) +
  labs(x = "Days in NZ", y = NULL) +
  theme_ivs()

p13 <- p13_density / p13_box +
  plot_layout(heights = c(4, 1)) +
  plot_annotation(
    title = "How Long Do Visitors Stay in New Zealand?",
    subtitle = "Capped at 90 days. Dashed line = median.",
    caption = "Source: IVS Microdata 2022–2025",
    theme = theme(
      plot.title    = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(colour = "grey40", size = 12),
      plot.caption  = element_text(colour = "grey55", size = 10)
    )
  )

p13

ggsave(output_path("days_in_nz.png"),
  p13,
  width = 11, height = 7, dpi = 300, bg = "white"
)


# package or not


p14 <- df |>
  filter(!is.na(travel_type)) |>
  count(travel_type) |>
  mutate(travel_type = fct_reorder(travel_type, n)) |>
  ggplot(aes(x = travel_type, y = n, fill = travel_type)) +
  geom_col(alpha = 0.85) +
  geom_text(aes(label = comma(n)), hjust = -0.1, size = 4) +
  coord_flip() +
  scale_fill_manual(values = c(
    "Independent Traveller" = "#4575b4",
    "Package"               = "#66bd63",
    "Tour Group"            = "#fc8d59"
  ), guide = "none") +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title    = "Visitor Counts by Travel Type",
    x        = NULL,
    y        = "Number of Respondents",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p14

ggsave(output_path("travel_type_counts.png"),
  p14,
  width = 10, height = 5, dpi = 300, bg = "white"
)


# sustainability consideration by country



sust_levels <- c("Never", "Rarely", "Sometimes", "Most of the time", "Always")

p15 <- df |>
  filter(
    country_of_residence %in% top10_countries,
    !is.na(sustainability_considered),
    sustainability_considered != "Not sure"
  ) |>
  mutate(sustainability_considered = factor(sustainability_considered,
    levels = sust_levels
  )) |>
  count(country_of_residence, sustainability_considered) |>
  group_by(country_of_residence) |>
  mutate(pct = n / sum(n)) |>
  ungroup() |>
  mutate(country_of_residence = fct_reorder(
    country_of_residence, pct,
    function(x) x[length(x)] # order by "Always" pct
  )) |>
  ggplot(aes(
    x = country_of_residence, y = pct,
    fill = sustainability_considered
  )) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c(
    "Never"            = "#d73027",
    "Rarely"           = "#fc8d59",
    "Sometimes"        = "#fee090",
    "Most of the time" = "#91bfdb",
    "Always"           = "#4575b4"
  )) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title    = "How Often Do Visitors Consider Sustainability?",
    subtitle = "By country of origin (top 10 by respondent count)",
    x        = NULL,
    y        = "Share of Respondents",
    fill     = "Sustainability\nConsidered",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs() +
  theme(legend.position = "right")

p15

ggsave(output_path("sustainability_by_country.png"),
  p15,
  width = 13, height = 7, dpi = 300, bg = "white"
)


# arrival by month


month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

p16 <- df |>
  filter(!is.na(arrival_month)) |>
  count(arrival_month) |>
  mutate(month_label = factor(month_labels[arrival_month],
    levels = month_labels
  )) |>
  ggplot(aes(x = month_label, y = n)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_text(aes(label = comma(n)), vjust = -0.4, size = 3.5) +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0, 0.1))
  ) +
  labs(
    title    = "Visitor Arrivals by Month",
    subtitle = "Aggregated across all years 2022–2025",
    x        = "Month",
    y        = "Number of Arrivals",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p16

ggsave(output_path("arrivals_by_month.png"),
  p16,
  width = 11, height = 6, dpi = 300, bg = "white"
)


# arrivals over the years



survey_main <- readRDS("output/survey_cleaned.rds")

p17 <- survey_main |>
  filter(!is.na(arrival_year)) |>
  count(arrival_year) |>
  ggplot(aes(x = arrival_year, y = n)) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_point(colour = "steelblue", size = 3) +
  geom_text(aes(label = comma(n)), vjust = -1, size = 4) +
  scale_x_continuous(breaks = 2022:2025) +
  scale_y_continuous(
    labels = comma_format(),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  labs(
    title   = "Visitor Arrivals Over Time (Yearly)",
    x       = "Year",
    y       = "Number of Arrivals",
    caption = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p17

ggsave(output_path("arrivals_over_time.png"),
  p17,
  width = 13, height = 6, dpi = 300, bg = "white"
)


# expenditure by industry


cost_cols <- c(
  "cost_accomm"        = "Accommodation",
  "cost_dom_travel"    = "Domestic Travel",
  "cost_food_drink"    = "Food & Drink (Groceries)",
  "cost_entertainment" = "Entertainment",
  "cost_shopping"      = "Shopping",
  "cost_dom_flights"   = "Domestic Flights",
  "cost_car_rentals"   = "Car Rentals",
  "cost_eating_out"    = "Eating Out",
  "cost_day_cruise"    = "Day Cruise"
)

p18 <- df |>
  select(all_of(names(cost_cols))) |>
  pivot_longer(everything(), names_to = "industry", values_to = "spend") |>
  filter(!is.na(spend), spend > 0) |>
  mutate(industry = recode(industry, !!!cost_cols)) |>
  group_by(industry) |>
  summarise(
    mean_spend = mean(spend, na.rm = TRUE),
    median_spend = median(spend, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(industry = fct_reorder(industry, median_spend)) |>
  ggplot(aes(x = industry, y = median_spend)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_text(aes(label = dollar(round(median_spend), prefix = "NZD ")),
    hjust = -0.1, size = 3.5
  ) +
  coord_flip() +
  scale_y_continuous(
    labels = dollar_format(prefix = "NZD "),
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title    = "Median Spend by Industry Category",
    subtitle = "Bars = mean. Red diamond = median (among those who spent > 0 in that category)",
    x        = NULL,
    y        = "Spend (NZD)",
    caption  = "Source: IVS Microdata 2022–2025"
  ) +
  theme_ivs()

p18

# ggsave(output_path("spend_by_industry.png"),
#   p18,
#   width = 12, height = 7, dpi = 300, bg = "white"
# )


# places stayed and spend most days - cholorpleths

# ── load data ──────────────────────────────────────────────────────────────────
itinerary <- read_csv("../data/itinerary.csv")
ta_shp <- sf::st_read("../data/ta_shp/territorial-authority-local-board-2023-clipped-generalised.shp",
  quiet = TRUE
)

# ── normalise itinerary place names ───────────────────────────────────────────
normalise_place <- function(x) {
  case_when(
    str_detect(x, "Queenstown-Lakes") ~ "Queenstown-Lakes District",
    str_detect(x, "Southland District") ~ "Southland District",
    x == "Hutt City" ~ "Lower Hutt City",
    x == "Auckland District" ~ "Auckland",
    TRUE ~ x
  )
}

# ── clean shapefile ────────────────────────────────────────────────────────────
# dissolve Auckland local boards into one polygon
# drop Chatham Islands and Area Outside TA (no visitors in data)
ta_shp_clean <- ta_shp |>
  mutate(
    join_name = case_when(
      str_detect(TALB2023_2, "Local Board Area") ~ "Auckland",
      TALB2023_2 == "Area Outside Territorial Authority" ~ NA_character_,
      TALB2023_2 == "Chatham Islands Territory" ~ NA_character_,
      TRUE ~ TALB2023_2
    ),
    # normalise macrons on shapefile side to match itinerary names
    join_name = str_replace_all(join_name, "ū", "u")
  ) |>
  filter(!is.na(join_name)) |>
  group_by(join_name) |>
  summarise(geometry = sf::st_union(geometry), .groups = "drop")

# ── visitor counts per TA ─────────────────────────────────────────────────────
ta_visitor_counts <- itinerary |>
  filter(place_stayed != "Other") |>
  mutate(ta_name = normalise_place(place_stayed)) |>
  distinct(response_id, ta_name) |>
  count(ta_name, name = "visitor_count")

# ── mean nights per visitor per TA ────────────────────────────────────────────
ta_avg_nights <- itinerary |>
  filter(place_stayed != "Other", !is.na(nights)) |>
  mutate(ta_name = normalise_place(place_stayed)) |>
  group_by(ta_name) |>
  summarise(
    mean_nights = mean(nights, na.rm = TRUE),
    n_visitors = n_distinct(response_id),
    .groups = "drop"
  )

# ── join to cleaned shapefile ─────────────────────────────────────────────────
ta_map_visitors <- ta_shp_clean |>
  left_join(ta_visitor_counts, by = c("join_name" = "ta_name"))

ta_map_nights <- ta_shp_clean |>
  left_join(ta_avg_nights, by = c("join_name" = "ta_name"))

# ── check unmatched ───────────────────────────────────────────────────────────
unmatched_v <- ta_map_visitors |>
  filter(is.na(visitor_count)) |>
  pull(join_name)
unmatched_n <- ta_map_nights |>
  filter(is.na(mean_nights)) |>
  pull(join_name)

if (length(unmatched_v) > 0) message("Unmatched (visitors): ", paste(unmatched_v, collapse = ", "))
if (length(unmatched_n) > 0) message("Unmatched (nights):   ", paste(unmatched_n, collapse = ", "))


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 1 — unique visitor count per TA
# ══════════════════════════════════════════════════════════════════════════════

p_ta_visitors <- ggplot(ta_map_visitors) +
  geom_sf(aes(fill = visitor_count), colour = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    colours  = c("#f7fbff", "#c6dbef", "#6baed6", "#2171b5", "#08306b"),
    na.value = "grey90",
    labels   = comma_format(),
    name     = "Visitors",
    trans    = "log10"
  ) +
  labs(
    title    = "Visitor Distribution Across NZ Territorial Authorities",
    subtitle = "Number of unique survey respondents who stayed at least one night",
    caption  = "Source: IVS Microdata 2022–2025. Boundaries: Stats NZ 2023. Log colour scale."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 6)),
    plot.subtitle   = element_text(colour = "grey40", size = 12, margin = margin(b = 10)),
    plot.caption    = element_text(colour = "grey55", size = 10, margin = margin(t = 8)),
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = NA)
  )


# ══════════════════════════════════════════════════════════════════════════════
# PLOT 2 — mean nights per visitor per TA
# ══════════════════════════════════════════════════════════════════════════════

p_ta_nights <- ggplot(ta_map_nights) +
  geom_sf(aes(fill = mean_nights), colour = "white", linewidth = 0.2) +
  scale_fill_gradientn(
    colours  = c("#fff7ec", "#fdd49e", "#fc8d59", "#d7301f", "#7f0000"),
    na.value = "grey90",
    labels   = function(x) paste0(round(x, 1), " nights"),
    name     = "Avg Nights"
  ) +
  labs(
    title    = "Where Do Visitors Stay the Longest?",
    subtitle = "Average nights per visitor by territorial authority",
    caption  = "Source: IVS Microdata 2022–2025. Boundaries: Stats NZ 2023."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 6)),
    plot.subtitle   = element_text(colour = "grey40", size = 12, margin = margin(b = 10)),
    plot.caption    = element_text(colour = "grey55", size = 10, margin = margin(t = 8)),
    legend.position = "right",
    plot.background = element_rect(fill = "white", colour = NA)
  )

p_ta_visitors
p_ta_nights


# ── save ───────────────────────────────────────────────────────────────────────
# ggsave(output_path("ta_choropleth_visitors.png"),
#   p_ta_visitors,
#   width = 9, height = 12, dpi = 300, bg = "white"
# )

# ggsave(output_path("ta_choropleth_avg_nights.png"),
#   p_ta_nights,
#   width = 9, height = 12, dpi = 300, bg = "white"
# )



# ══════════════════════════════════════════════════════════════════════════════
# CORRELATION ANALYSIS
# ══════════════════════════════════════════════════════════════════════════════
cor_matrix <- df |>
  select(where(is.numeric)) |>
  mutate(across(everything(), as.numeric)) |>
  drop_na() |>
  cor(use = "pairwise.complete.obs")

high_cor_vars <- cor_matrix |>
  as.data.frame() |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "r") |>
  filter(var1 != var2, abs(r) > 0.65) |>
  pull(var1) |>
  unique()

# step 3: subset matrix to only those variables
cor_subset <- cor_matrix[high_cor_vars, high_cor_vars]

# shorten column names before plotting
colnames(cor_subset) <- abbreviate(colnames(cor_subset), minlength = 15)
rownames(cor_subset) <- abbreviate(rownames(cor_subset), minlength = 15)

png(output_path("corr_plot.png"), width = 2400, height = 2000, res = 150)

corrplot(
  cor_subset,
  method      = "color",
  type        = "lower",
  addCoef.col = "black",
  number.cex  = 1.6,
  tl.cex      = 1.6,
  tl.col      = "black",
  tl.srt      = 45,
  col         = colorRampPalette(c("#C0392B", "white", "#2471A3"))(200),
  title       = "Highly Correlated Variables (|r| > 0.65)",
  mar         = c(0, 0, 2, 0)
)

dev.off()
