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

# Comparison of long-term residential completion trends by intended market
plot_6_1_1_market <- 
  completions_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "All") |>
  ggplot(aes(year, value, colour = market)) +
  geom_line() +
  scale_y_continuous("Completions") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Intended market") +
  ggtitle("Annual housing completions by intended market") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Variation with between-market difference emphasized
plot_6_1_1_market_facet <-
  completions_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "All") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = market) |>
  ggplot(aes(year, value, group = market)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Completions") +
  scale_x_continuous("Year") +
  facet_wrap(~market) +
  ggtitle(
    "Annual housing completions by intended market (five-year moving average)"
    ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Map of housing completions by five-year chunk
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

#' Overall starts
plot_6_1_2_overall <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type == "All") |>
  mutate(value_trend = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  pivot_longer(c(value, value_trend)) |> 
  mutate(name = if_else(name == "value", "Actual", 
                        "Five-year moving average")) |> 
  ggplot(aes(year, value, group = name, linewidth = name, alpha = name)) +
  geom_line() +
  scale_y_continuous("Starts") +
  scale_x_continuous("Year") +
  scale_linewidth_manual(name = NULL, values = c(
    "Actual" = 0.4, "Five-year moving average" = 1)) +
  scale_alpha_manual(name = NULL, values = c(
    "Actual" = 0.2, "Five-year moving average" = 1)) +
  ggtitle("Annual housing starts") +
  graph_theme

# Table with 5-year aggregations
table_6_1_2_five_year <- 
  starts_by_type |> 
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
  mutate(Total = Single + `Semi-Detached` + Row + Apartment, 
         .before = Single) |> 
  gt::gt() |> 
  gt::tab_header("Average annual housing starts by dwelling type")

# Comparison of long-term residential start trends by building type
plot_6_1_2_type <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  scale_y_continuous("Starts") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Dwelling type") +
  ggtitle("Annual housing starts by dwelling type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Variation with between-type difference emphasized
plot_6_1_2_type_facet <-
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  ggplot(aes(year, value, group = type, colour = type)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Starts") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Dwelling type") +
  facet_wrap(~type) +
  ggtitle(
    "Annual housing starts by dwelling type (five-year moving average)") +
  theme_minimal() +
  theme(legend.position = "none")

# Pct of annual starts which are apartment
plot_6_1_2_type_apart <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  summarize(apart_pct = value[type == "Apartment"] / sum(value), .by = year) |>
  ggplot(aes(year, apart_pct)) +
  geom_line() +
  scale_y_continuous("Starts", limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous("Year") +
  ggtitle(
    "Percentage of annual housing starts which are apartments") +
  theme_minimal()

# Map of housing starts by five-year chunk
map_6_1_2_annual_1 <- 
  starts_by_type |> 
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
  scale_fill_viridis_b("Annual starts") +
  theme_void() +
  theme(legend.position = "bottom")

map_6_1_2_annual_2 <- 
  starts_by_type |> 
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
  scale_fill_viridis_b("Annual starts per 1000 dwellings") +
  theme_void() +
  theme(legend.position = "bottom")

map_6_1_2_annual <- 
  patchwork::wrap_plots(map_6_1_2_annual_1, map_6_1_2_annual_2)


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

# Table with 5-year aggregations
table_6_1_3_five_year <- 
  starts_by_market |> 
  filter(is.na(zone)) |>
  mutate("Date Range" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(`Date Range`, market)) |> 
  pivot_wider(names_from = market, values_from = avg) |> 
  select(`Date Range`, Total = All, Homeowner:`Co-Op`) |> 
  gt::gt() |> 
  gt::tab_header("Average annual housing starts by intended market")

# Comparison of long-term residential start trends by intended market
plot_6_1_3_market <- 
  starts_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "All", market != "Unknown") |>
  ggplot(aes(year, value, colour = market)) +
  geom_line() +
  scale_y_continuous("Starts") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Intended market") +
  ggtitle("Annual housing starts by intended market") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Variation with between-type difference emphasized
plot_6_1_3_market_facet <-
  starts_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "All", market != "Unknown") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = market) |>
  ggplot(aes(year, value, group = market, colour = market)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Starts") +
  scale_x_continuous("Year") +
  scale_colour_discrete("Intended market") +
  facet_wrap(~market) +
  ggtitle(
    "Annual housing starts by intended market (five-year moving average)") +
  theme_minimal() +
  theme(legend.position = "none")

# Pct of annual starts which are rental
plot_6_1_3_market_rental <-
  starts_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "All") |>
  summarize(rental_pct = value[market == "Rental"] / sum(value), .by = year) |>
  ggplot(aes(year, rental_pct)) +
  geom_line() +
  scale_y_continuous("Starts", limits = c(0, 1), labels = scales::percent) +
  scale_x_continuous("Year") +
  ggtitle(
    "Percentage of annual housing starts which are intended for the rental market") +
  theme_minimal()


# 6.1.5 Prix des logements neufs ------------------------------------------

# Prices taken from CMHC HMIP
new_prices <-
  tribble(
    ~year, ~p20, ~p40, ~p60, ~p80, ~median, ~average, ~units,
    1998, 105000, 120000, 130000, 155000, 125000, 133508, 1010,
    1999, 115000, 130000, 145000, 160000, 135000, 143505, 1134,
    2000, 130000, 150000, 160000, 190000, 150000, 163171, 1294,
    2001, 145000, 160000, 180000, 200000, 165000, 178914, 1254,
    2002, 175000, 190000, 210000, 250000, 200000, 214559, 1489,
    2003, 195000, 220000, 250000, 280000, 240000, 242900, 1554,
    2004, 205000, 235000, 260000, 300000, 250000, 265270, 1653,
    2005, 225000, 255000, 280000, 355000, 265000, 289693, 1562,
    2006, 260000, 290000, 335000, 400000, 300000, 336209, 1096,
    2007, 270000, 300000, 361000, 425000, 330000, 355025, 1075,
    2008, 295000, 350000, 400000, 480000, 380000, 395287, 1158,
    2009, 350000, 390000, 425000, 500000, 400000, 427751, 812,
    2010, 285000, 335000, 390000, 465000, 360000, 384910, 882,
    2011, 310000, 370000, 420000, 500000, 395000, 406589, 559,
    2012, 368000, 415000, 485000, 570000, 440000, 463910, 482,
    2013, 386000, 440000, 508000, 605000, 465000, 487631, 250,
    2014, 404000, 468000, 525000, 650000, 495000, 527828, 205,
    2015, 404000, 500000, 580000, 691000, 545000, 553809, 212,
    2016, 431000, 525000, 558000, 620000, 545000, 535079, 162,
    2017, 404000, 473000, 539000, 631000, 485000, 523579, 136,
    2018, 484000, 566000, 626000, 658000, 605000, 617806, 89,
    2019, NA,     NA,     NA,     NA,     NA,     611825,	90,
    2020, 532000, 605000, 682000, 924000, 650000, 679529, 87,
    2021, 586000, 634000, 768000, 866000, 665000, 755538, 74,
    2022, 779000, 840000, 870000, 952000, 860000, 889831, 125,
    2023, 801000, 909000, 999000, 1096000, 970000, 1065369, 82) |> 
  mutate(ratio_80_20 = p80 / p20)

plot_6_1_5_percentiles <- 
  new_prices |> 
  pivot_longer(c(p20, p40, median, p60, p80)) |> 
  mutate(name = case_when(
    name == "p20" ~ "20th percentile",
    name == "p40" ~ "40th percentile",
    name == "median" ~ "Median",
    name == "p60" ~ "60th percentile",
    name == "p80" ~ "80th percentile")) |> 
  mutate(name = factor(name, levels = c("20th percentile", "40th percentile", 
                                        "Median", "60th percentile", 
                                        "80th percentile"))) |> 
  filter(year != 2019) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_line() +
  scale_y_continuous("Price", labels = scales::dollar) +
  scale_x_continuous("Year") +
  scale_colour_discrete(NULL) +
  ggtitle(
    "Average annual price for absorbed homeowner and condominimum units") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_6_1_5_units <- 
  new_prices |> 
  filter(year != 2019) |> 
  ggplot(aes(year, units)) +
  geom_line() +
  scale_y_continuous("Units", labels = scales::comma) +
  scale_x_continuous("Year") +
  ggtitle(
    "Annual absorbed homeowner and condominimum units") +
  theme_minimal()


# 6.1.12 Valeur foncière --------------------------------------------------

# Get UEF
uef <-
  read_csv("data/role-evaluation-2019.csv") |> 
  rename(FST = `ra01-code-postal-3-car`) |> 
  filter(`gen-cd-typ-util-nom-2-chifre` == "Logement") |> 
  select(-`gen-cd-typ-util-nom-2-chifre`,
         -`ra02-imp-scolaire-nom`,
         -`ra02-date-init-inscr-role`,
         -`re98-montant-immeuble-imp`,
         -`re98-montant-immeuble-ni`,
         -`re98-montant-eae-imp-scolaire`,
         -`ra01-nb-total-locaux-nr`) |> 
  set_names(c("FST", "area", "floors", "year_built", "construct_method",
              "type", "units", "value", "value_previous", "class")) |> 
  select(-construct_method) |> 
  mutate(area = as.numeric(str_replace(area, ",", "\\."))) |> 
  filter(str_detect(class, "1|2|3|4|5|6|siduelle") | 
           class == "Six logement et plus") |> 
  filter(!str_detect(class, "Agricole"),
         !is.na(type))

# Get forward sortation areas
fst <- 
  read_sf("data/lfsa000b21a_e/lfsa000b21a_e.shp") |> 
  filter(PRUID == "24") |> 
  st_make_valid() |> 
  st_transform(4326) |> 
  filter(CFSAUID %in% uef$FST) |> 
  rename(FST = CFSAUID)
  
map_6_1_12 <- 
  uef |> 
  summarize(`Per building` = mean(value),
            `Per unit` = sum(value) / sum(units),
            `Per sqft` = 880 * sum(value) / sum(area, na.rm = TRUE), 
            .by = FST) |> 
  pivot_longer(-FST) |> 
  inner_join(fst) |> 
  st_as_sf() |> 
  st_filter(laval_sectors) |> 
  ggplot(aes(fill = value)) +
  geom_sf() +
  scale_fill_viridis_b("Average assessed value", 
                       n.breaks = 6, labels = scales::dollar) +
  facet_wrap(~name) +
  ggtitle("Average assessed property value by forward sortation area (2019)") +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(60, "points"))

plot_6_1_12_boxplot <- 
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  mutate(`Per unit` = value / units) |> 
  rename(`Per building` = value) |> 
  pivot_longer(c(`Per building`, `Per unit`)) |> 
  ggplot(aes(type, value)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~name) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_discrete("Property type") +
  theme_minimal()

plot_6_1_12_year_building <-
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  summarize(n = n(), value = mean(value), .by = c(year_built, type)) |> 
  filter(year_built >= 1900) |>
  ggplot(aes(year_built, value, size = n, fill = type, 
             colour = after_scale(alpha(fill, 0.6)))) +
  geom_point() +
  gghighlight() +
  facet_wrap(~type) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_continuous("Year of construction") +
  scale_size_area("Number of properties") +
  ggtitle("Average annual per-building assessed value by property type") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_6_1_12_year_unit <- 
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  summarize(n = sum(units), value = sum(value) / sum(units), 
            .by = c(year_built, type)) |> 
  filter(year_built >= 1900) |>
  ggplot(aes(year_built, value, size = n, fill = type, 
             colour = after_scale(alpha(fill, 0.6)))) +
  geom_point() +
  gghighlight() +
  facet_wrap(~type) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_continuous("Year of construction") +
  scale_size_area("Number of properties") +
  ggtitle("Average annual per-unit assessed value by property type") +
  theme_minimal() +
  theme(legend.position = "bottom")


# R Markdown --------------------------------------------------------------

qs::qsavem(cmhc_zones, completions_by_type, completions_by_market, 
           plot_6_1_1_overall, table_6_1_1_five_year, plot_6_1_1_type, 
           plot_6_1_1_type_facet, plot_6_1_1_market, plot_6_1_1_market_facet, 
           map_6_1_1_annual, starts_by_type, plot_6_1_2_overall, 
           table_6_1_2_five_year, plot_6_1_2_type, plot_6_1_2_type_facet, 
           plot_6_1_2_type_apart, map_6_1_2_annual, starts_by_market,
           table_6_1_3_five_year, plot_6_1_3_market, plot_6_1_3_market_facet,
           plot_6_1_3_market_rental, new_prices, plot_6_1_5_percentiles,
           plot_6_1_5_units, uef, map_6_1_12, plot_6_1_12_boxplot,
           plot_6_1_12_year_building, plot_6_1_12_year_unit,
           file = "data/section_6_1.qsm")
