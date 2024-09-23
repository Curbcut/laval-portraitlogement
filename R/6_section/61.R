#### 6.1 #######################################################################

source("R/utils/startup.R")


# Process CMHC zones ------------------------------------------------------

qs::qload("data/cmhc_shp.qsm")

cmhc_zones <-
  cmhc_nbhd_2022 |> 
  filter(METCODE == "1060",
         NBHDCODE %in% c("370", "380", "390", "400", "410", "420", "430", "440", 
                         "450", "460", "470", "480")) |> 
  summarize(geometry = st_union(geometry), .by = ZONECODE) |> 
  mutate(zone = case_when(
    ZONECODE == 19 ~ "Chomedey/Sainte-Dorothée",
    ZONECODE == 20 ~ "Laval-des-Rapides",
    ZONECODE == 21 ~ "Pont-Viau",
    ZONECODE == 22 ~ "St-François/St-Vincent/Duvernay",
    ZONECODE == 23 ~ "Vimont/Auteuil",
    ZONECODE == 24 ~ "Laval-Ouest/Fabreville/Ste-Rose"), .after = ZONECODE) |> 
  rename(zone_code = ZONECODE) |> 
  st_transform(32618)

# Get 2021 dwelling counts
dwellings <- 
  get_census("CA21", regions = list(CSD = 2465005), level = "DB", 
             geo_format = "sf") |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(dwellings = Dwellings, geometry) |> 
  st_transform(32618)

# Add to cmhc_zones
cmhc_zones <- 
  cmhc_zones |> 
  mutate(dwellings = 
           sf::st_interpolate_aw(dwellings, cmhc_zones, extensive = TRUE) |> 
           pull(dwellings))

rm(cmhc_nbhd_2016, cmhc_nbhd_2017, cmhc_nbhd_2018, cmhc_nbhd_2019,
   cmhc_nbhd_2020, cmhc_nbhd_2021, cmhc_nbhd_2022, dwellings)


# 6.1.1 Dév rés récent (répartition territoriale par type de logem --------

completions_by_type <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Completions", 
      dimension = "Dwelling Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "type", "value", "date", "year", "survey", "series"))

completions_by_market <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Completions", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "market", "value", "date", "year", "survey", "series"))

#' Overall completions mostly stable over last 30 years, albeit with high 
#' levels of year-to-year variability
plot_6_1_1_overall <- 
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type == "All") |>
  mutate(value_trend = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  pivot_longer(c(value, value_trend)) |> 
  mutate(name = if_else(name == "value", "Actual", 
                        "Five-year moving average")) |> 
  ggplot(aes(year, value, group = name, linewidth = name, alpha = name)) +
  geom_line() +
  scale_y_continuous("Completions") +
  scale_x_continuous("Year") +
  scale_linewidth_manual(name = NULL, values = c(
    "Actual" = 0.4, "Five-year moving average" = 1)) +
  scale_alpha_manual(name = NULL, values = c(
    "Actual" = 0.2, "Five-year moving average" = 1)) +
  ggtitle("Annual housing completions") +
  graph_theme

# Table with 5-year aggregations
table_6_1_1_five_year <- 
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |> 
  mutate("Date Range" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(`Date Range`, type)) |> 
  pivot_wider(names_from = type, values_from = avg) |> 
  mutate(Total = Single + `Semi-Detached` + Row + Apartment, .before = Single) |> 
  gt::gt() |> 
  gt::tab_header("Average annual housing completions by dwelling type")

# Comparison of long-term residential completion trends by building type
plot_6_1_1_type <- 
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  scale_y_continuous("Completions") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Dwelling type") +
  ggtitle("Annual housing completions by dwelling type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Variation with between-type difference emphasized
plot_6_1_1_type_facet <-
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  ggplot(aes(year, value, group = type)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Completions") +
  scale_x_continuous("Year") +
  facet_wrap(~type) +
  ggtitle(
    "Annual housing completions by dwelling type (five-year moving average)") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Map of housing completions by five-year chunk
completions_by_type |> 
  filter(year >= 2019) |> 
  filter(type == "All") |> 
  filter(!is.na(zone)) |> 
  summarize(value = sum(value), .by = zone) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  ggplot(aes(fill = value)) +
  geom_sf()

map_6_1_1_annual_1 <- 
  completions_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "All") |> 
  mutate(date = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(date, zone)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  select(zone, date, avg, geometry) |> 
  ggplot(aes(fill = avg)) +
  geom_sf(colour = "white", lwd = 0.5) +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_viridis_b("Annual completions") +
  theme_void() +
  theme(legend.position = "bottom")
  
map_6_1_1_annual_2 <- 
  completions_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "All") |> 
  mutate(date = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(date, zone)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  mutate(density = avg / dwellings * 1000) |> 
  select(zone, date, density, geometry) |> 
  ggplot(aes(fill = density)) +
  geom_sf(colour = "white", lwd = 0.5) +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_viridis_b("Annual completions per 1000 dwellings") +
  theme_void() +
  theme(legend.position = "bottom")

map_6_1_1_annual <- 
  patchwork::wrap_plots(map_6_1_1_annual_1, map_6_1_1_annual_2)


# 6.1.2 Mises en chantier par typologie -----------------------------------

starts_by_type <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Dwelling Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "type", "value", "date", "year", "survey", "series"))

#' Total housing starts are trending down, with an exception of an apparently
#' one-time surge in 2017/2018.
starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type == "All") |>
  # mutate(value_trend = slider::slide_dbl(value, mean, .before = 0), .by = type) |>
  ggplot(aes(year, value)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

#' Housing starts are overwhelmingly apartment.
starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  mutate(value_trend = slider::slide_dbl(value, mean, .before = 0), .by = type) |>
  ggplot(aes(year, colour = type)) +
  geom_line(aes(y = value_trend), lwd = 1) +
  theme_minimal()

# Pct of annual starts which are apartment
starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  summarize(apart_pct = value[type == "Apartment"] / sum(value), .by = year) |>
  ggplot(aes(year, apart_pct)) +
  geom_line() +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_minimal()

# Housing starts mostly in Chomedey/Sainte-Dorothée, but on a downward trend
starts_by_type |> 
  filter(!is.na(zone)) |>
  filter(type != "All") |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(~zone, scales = "free_y") +
  theme_minimal()

starts_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "All") |>
  summarize(chom_pct = value[zone == "Chomedey/Sainte-Dorothée"] / sum(value), 
            .by = year) |>
  ggplot(aes(year, chom_pct)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  theme_minimal()


# 6.1.3 Mises en chantier par mode d'occupation ---------------------------

starts_by_market <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "market", "value", "date", "year", "survey", "series"))

starts_by_market |> 
  filter(!is.na(`Survey Zones`)) |> 
  filter(`Intended Market` != "All") |> 
  ggplot(aes(Year, Value, colour = `Intended Market`)) +
  geom_line() +
  facet_wrap(~`Survey Zones`, scales = "free_y") +
  theme_minimal()



# R Markdown --------------------------------------------------------------

qs::qsavem(cmhc_zones, completions_by_type, completions_by_market, 
           plot_6_1_1_overall, table_6_1_1_five_year, plot_6_1_1_type, 
           plot_6_1_1_type_facet, map_6_1_1_annual, 
           file = "data/section_6_1.qsm")
