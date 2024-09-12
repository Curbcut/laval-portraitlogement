#### 6.1 #######################################################################

source("R/utils/startup.R")


# 6.1.1 Dév rés récent (répartition territoriale par type de logem --------


# 6.1.2 Mises en chantier par typologie -----------------------------------

starts_by_type <- 
  map(2010:2023, \(x) {
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
  map(2010:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows()

starts_by_market |> 
  filter(!is.na(`Survey Zones`)) |> 
  filter(`Intended Market` != "All") |> 
  ggplot(aes(Year, Value, colour = `Intended Market`)) +
  geom_line() +
  facet_wrap(~`Survey Zones`, scales = "free_y") +
  theme_minimal()
