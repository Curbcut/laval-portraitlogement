#### 5.2.1 #####################################################################

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


# 5.2.1.1 -----------------------------------------------------------------

# library(rvest)
# 
# # Xpaths of tables
# url <- "https://www.centris.ca/en/tools/real-estate-statistics/laval"
# 
# categories <- c("total", "single_family", "condo", "plex_2_to_5_units")
# 
# get_table <- function(url) {
#   tables <- lapply(seq_along(categories), \(x) {
#     total <- 
#       read_html(url) |> 
#       html_nodes(xpath=glue::glue('//*[@id="contenuStats"]/div[{x}]/table')) %>%
#       html_table()
#     total <- as.data.frame(total[[1]])
#     total <- total[c(1,4,7,10), c(1, 8)]
#     names(total) <- c("indicateur", "last_12months")
#     total$last_12months <- gsub(",|\\$", "", total$last_12months)
#     total$last_12months <- as.numeric(total$last_12months)
#     total$category <- categories[[x]]
#     total
#   })
#   Reduce(rbind, tables)
# }
# 
# 
# # For all sectors
# sectors <- page |> 
#   html_nodes(xpath ='//*[@id="CityDistrictCommunity"]/li') |> 
#   html_attr("data-option-value")
# 
# sectors_name <- page |> 
#   html_nodes(xpath ='//*[@id="CityDistrictCommunity"]/li') |> 
#   html_nodes("a") |> 
#   html_text()
# 
# sectors <- sectors[sectors  != ""]
# 
# sectors_scrape <- 
#   sapply(paste0(url, "/", sectors), get_table, simplify = FALSE, USE.NAMES = TRUE)
# 
# sectors_name <- sectors_name[sectors_name != "All municipalities"]
# sectors_name <- gsub("(Laval \\()|(\\))", "", sectors_name)
# 
# sectors <- Reduce(rbind, mapply(\(df, name) {
#   df$sector <- name
#   df
# }, sectors_scrape, sectors_name, SIMPLIFY = FALSE)) |> 
#   tibble::as_tibble()
# 
# sectors <- sectors[sectors$indicateur %in% c("Sales", "Average selling time (days)", "Median  price"), ]
# 
# z <- sectors %>%
#   pivot_wider(
#     names_from = c(category, indicateur),
#     values_from = last_12months
#   )
# 
# prix_sur_marche_table <- 
#   gt(z[c(1,3:ncol(z))]) |> 
#   fmt_missing(
#     columns = everything(),  # Apply to all columns
#     missing_text = ""        # Replace NA with blank
#   ) |> 
#   data_color(
#     columns = c(3,5,6,8,9,11)-1,
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |> 
#   fmt(columns = c(3,5,6,8,9,11)-1, fns = convert_number) |>
#   data_color(
#     columns = c(4,7,10)-1,
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |>
#   fmt(columns = c(4,7,10)-1, fns = \(x) paste0(convert_number(x), " $")) |> 
#   # tab_spanner(
#   #   label = "Laval",
#   #   columns = c(`total_Sales`)
#   # ) |>
#   tab_spanner(
#     label = "Unifamiliale",
#     columns = c(`single_family_Sales`,
#                 `single_family_Median  price`,
#                 `single_family_Average selling time (days)`)
#   ) |>
#   tab_spanner(
#     label = "Copropriété",
#     columns = c(`condo_Sales`,
#                 `condo_Median  price`,
#                 `condo_Average selling time (days)`)
#   ) |>
#   tab_spanner(
#     label = "Plex (2 à 5 logements)",
#     columns = c(`plex_2_to_5_units_Sales`,
#                 `plex_2_to_5_units_Median  price`,
#                 `plex_2_to_5_units_Average selling time (days)`)
#   ) |>
#   cols_label(
#     sector = "Secteur",
#     # `total_Sales` = "Ventes",
#     `single_family_Sales` = "Ventes",
#     `single_family_Median  price` = "Prix médian",
#     `single_family_Average selling time (days)` = "Délai de vente moyen (jours)",
#     
#     `condo_Sales` = "Ventes",
#     `condo_Median  price` = "Prix médian",
#     `condo_Average selling time (days)` = "Délai de vente moyen (jours)",
#     
#     `plex_2_to_5_units_Sales` = "Ventes",
#     `plex_2_to_5_units_Median  price` = "Prix médian",
#     `plex_2_to_5_units_Average selling time (days)` = "Délai de vente moyen (jours)"
#   ) |>
#   tab_style(
#     style = cell_text(
#       font = font_local_name
#     ),
#     locations = cells_body()
#   ) |>
#   tab_style(
#     style = cell_text(
#       font = font_local_name
#     ),
#     locations = cells_column_labels()
#   ) |>
#   tab_options(
#     table.font.size = table_font_size,
#     row_group.font.size = table_font_size,
#     table.width = px(6 * 96)
#   )
# 
# gtsave(prix_sur_marche_table, "outputs/5/5_2_1_prixsurmarchetable.png", zoom = 1)

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

plot_5_2_1_1_percentiles <-
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
  scale_colour_manual(values = curbcut_colors$left_5$fill[2:6]) +
  ggtitle(
    "Average annual price for absorbed homeowner and condominimum units") +
  graph_theme

plot_5_2_1_1_units <-
  new_prices |> 
  ggplot(aes(year, units)) +
  geom_line() +
  scale_y_continuous("Units", labels = scales::comma) +
  scale_x_continuous("Year") +
  ggtitle(
    "Annual absorbed homeowner and condominimum units") +
    graph_theme


# 5.2.1.2 Loyer moyen des logements locatifs selon le nombre de ch --------

rent_by_bedroom <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Average Rent", 
      dimension = "Bedroom Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "date", "year", "survey", 
              "series"))

rent_by_bedroom_z <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Average Rent", 
      dimension = "Bedroom Type",
      breakdown = "Survey Zones", 
      geo_uid = "24462",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "geog", "date", "year",
              "survey", "series")) |> 
  select(-geog) |> 
  filter(zone %in% cmhc_zones$zone)

rent_by_bedroom <- 
  rent_by_bedroom |> 
  bind_rows(rent_by_bedroom_z)

plot_5_2_1_2_facet <-
  rent_by_bedroom |> 
  filter(is.na(zone)) |>
  ggplot(aes(year, value, group = bedroom)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average monthly rent", labels = scales::dollar) +
  scale_x_continuous("Year") +
  facet_wrap(~bedroom) +
  ggtitle("Average monthly rent for purpose-built rentals, by bedroom type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_2_facet.pdf"),
#                 plot = plot_5_2_1_2_facet, width = 7.5, height = 6)

plot_5_2_1_2_change_facet <-
  rent_by_bedroom |> 
    arrange(zone, bedroom, year) |> 
  mutate(value = slider::slide_dbl(value, \(x) x[2] - x[1], .before = 1, 
                                   .complete = TRUE),
         .by = zone, bedroom) |> 
  filter(is.na(zone)) |>
  filter(year >= 1991) |> 
  ggplot(aes(year, value, group = bedroom)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average monthly rent", labels = scales::dollar) +
  scale_x_continuous("Year") +
  facet_wrap(~bedroom) +
  ggtitle(
      "Year-over-year change in average monthly rent for purpose-built rentals, by bedroom type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_2_change_facet.pdf"),
#                 plot = plot_5_2_1_2_change_facet, width = 7.5, height = 6)

# Table with 5-year aggregations
table_5_2_1_2_five_year <- 
  rent_by_bedroom |> 
  filter(is.na(zone)) |>
  mutate("Date Range" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(`Date Range`, bedroom)) |> 
  pivot_wider(names_from = bedroom, values_from = avg) |> 
  relocate(Total, .before = Bachelor) |> 
  mutate(across(-`Date Range`, round)) |> 
  mutate(across(-`Date Range`, scales::dollar)) |> 
  gt::gt() |> 
  gt::tab_header("Average monthly rent for purpose-built rentals by bedroom type")

# gtsave(table_5_2_1_2_five_year, "outputs/5/table_5_2_1_2_five_year.png", zoom = 1)

# Map of average rents by five-year chunk
map_5_2_1_2_annual <-
  rent_by_bedroom |> 
  filter(!is.na(zone)) |>
  mutate(date = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2010 ~ "2009-2013")) |> 
  summarize(avg = mean(value, na.rm = TRUE), .by = c(date, zone, bedroom)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  ggplot(aes(fill = avg)) +
  gg_cc_tiles +
  geom_sf(colour = "transparent", lwd = 0) +
  facet_grid(rows = vars(bedroom), cols = vars(date)) +

  scale_fill_stepsn("Average monthly rent", labels = scales::dollar, 
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme

# ggplot2::ggsave(filename = here::here("outputs/5/map_5_2_1_2_annual.pdf"),
#                 plot = map_5_2_1_2_annual, width = 7.5, height = 6)

rent_by_construction <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Average Rent", 
      dimension = "Year of Construction",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "construction", "value", "quality", "date", "year", 
              "survey", "series"))

rent_by_construction_z <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Average Rent", 
      dimension = "Year of Construction",
      breakdown = "Survey Zones", 
      geo_uid = "24462",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "construction", "value", "quality", "geog", "date", "year",
              "survey", "series")) |> 
  select(-geog) |> 
  filter(zone %in% cmhc_zones$zone)

rent_by_construction <- 
  rent_by_construction |> 
  bind_rows(rent_by_construction_z)

plot_5_2_1_2_construction_facet <-
  rent_by_construction |> 
  filter(is.na(zone), !is.na(value)) |>
  ggplot(aes(year, value, group = construction)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average monthly rent", labels = scales::dollar) +
  scale_x_continuous("Year") +
  facet_wrap(~construction) +
  ggtitle(
    "Average monthly rent for purpose-built rentals, by year of construction") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_2_construction_facet.pdf"),
#                 plot = plot_5_2_1_2_construction_facet, width = 7.5, height = 6)

# Table with 5-year aggregations
table_5_2_1_2_construction_five_year <- 
  rent_by_construction |> 
  filter(is.na(zone), !is.na(value)) |>
  mutate("Date Range" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value), .by = c(`Date Range`, construction)) |> 
  pivot_wider(names_from = construction, values_from = avg) |> 
  relocate(Total, .after = `Date Range`) |> 
  mutate(across(-`Date Range`, round)) |> 
  mutate(across(-`Date Range`, scales::dollar)) |> 
  gt::gt() |> 
  gt::tab_header(
    "Average monthly rent for purpose-built rentals by year of construction")


# 5.2.1.3 Taux d'inoccupation ---------------------------------------------

vacancy_by_bedroom <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Bedroom Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "date", "year", "survey", 
              "series"))

vacancy_by_bedroom_z <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Bedroom Type",
      breakdown = "Survey Zones", 
      geo_uid = "24462",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "geog", "date", "year",
              "survey", "series")) |> 
  select(-geog) |> 
  filter(zone %in% cmhc_zones$zone)

vacancy_by_bedroom <- 
  vacancy_by_bedroom |> 
  bind_rows(vacancy_by_bedroom_z)

plot_5_2_1_3_facet <-
  vacancy_by_bedroom |> 
  filter(is.na(zone), !is.na(value)) |>
  ggplot(aes(year, value / 100, group = bedroom)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average vacancy rate", labels = scales::percent) +
  scale_x_continuous("Year") +
  facet_wrap(~bedroom) +
  ggtitle("Vacancy rate for purpose-built rentals, by bedroom type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_3_facet.pdf"),
#                 plot = plot_5_2_1_3_facet, width = 7.5, height = 6)

# Table with 5-year aggregations
table_5_2_1_3_five_year <- 
  vacancy_by_bedroom |> 
  filter(is.na(zone)) |>
  mutate("Date Range" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(avg = mean(value, na.rm = TRUE) / 100, 
            .by = c(`Date Range`, bedroom)) |> 
  pivot_wider(names_from = bedroom, values_from = avg) |> 
  relocate(Total, .before = Bachelor) |> 
  mutate(across(-`Date Range`, round, 3)) |>
  mutate(across(-`Date Range`, scales::percent)) |> 
  gt::gt() |> 
  gt::tab_header("Vacancy rate for purpose-built rentals by bedroom type")

# gtsave(table_5_2_1_3_five_year, "outputs/5/table_5_2_1_3_five_year.png", zoom = 1)

# Map of vacancy rate by five-year chunk
map_5_2_1_3_annual <-
  vacancy_by_bedroom |> 
  filter(!is.na(zone)) |>
  mutate(date = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2010 ~ "2009-2013")) |> 
  summarize(avg = mean(value, na.rm = TRUE) / 100, 
            .by = c(date, zone, bedroom)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  ggplot(aes(fill = avg)) +
  gg_cc_tiles +
  geom_sf(colour = "transparent", lwd = 0) +
  facet_grid(rows = vars(bedroom), cols = vars(date)) +
  scale_fill_stepsn("Vacancy rate", colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme

# ggplot2::ggsave(filename = here::here("outputs/5/map_5_2_1_3_annual.pdf"),
#                 plot = map_5_2_1_3_annual, width = 7.5, height = 6)

vacancy_by_rent <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Rent Quartiles",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "date", "year", "survey", 
              "series"))

vacancy_by_rent_z <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Rent Quartiles",
      breakdown = "Survey Zones", 
      geo_uid = "24462",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "bedroom", "value", "quality", "geog", "date", "year",
              "survey", "series")) |> 
  select(-geog) |> 
  filter(zone %in% cmhc_zones$zone)

vacancy_by_rent <- 
  vacancy_by_rent |> 
  bind_rows(vacancy_by_rent_z) |> 
  rename(quartile = bedroom) |> 
  mutate(quartile = str_remove(quartile, "Vacancy Rate \\("),
         quartile = str_remove(quartile, "\\)"))

plot_5_2_1_3_rent_facet <-
  vacancy_by_rent |> 
  filter(is.na(zone), !is.na(value)) |>
  ggplot(aes(year, value / 100, group = quartile)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average vacancy rate", labels = scales::percent) +
  scale_x_continuous("Year") +
  facet_wrap(~quartile) +
  ggtitle("Vacancy rate for purpose-built rentals, by rent quartile") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_3_rent_facet.pdf"),
#                 plot = plot_5_2_1_3_rent_facet, width = 7.5, height = 6)

# Table with 5-year aggregations
table_5_2_1_3_rent_five_year <-
  vacancy_by_rent |> 
  filter(is.na(zone)) |>
  mutate("Date Range" = case_when(
    year >= 2020 ~ "2020-2023",
    year >= 2016 ~ "2016-2019",
    year >= 2012 ~ "2012-2015")) |> 
  summarize(avg = mean(value, na.rm = TRUE) / 100, 
            .by = c(`Date Range`, quartile)) |> 
  pivot_wider(names_from = quartile, values_from = avg) |> 
  mutate(across(-`Date Range`, round, 3)) |>
  mutate(across(-`Date Range`, scales::percent)) |> 
  gt::gt() |> 
  gt::tab_header("Vacancy rate for purpose-built rentals by bedroom type")

# gtsave(table_5_2_1_3_rent_five_year, "outputs/5/table_5_2_1_3_rent_five_year.png", zoom = 1)

# Map of vacancy rate by five-year chunk
map_5_2_1_3_rent_annual <-
  vacancy_by_rent |> 
  filter(!is.na(zone)) |>
  mutate(date = case_when(
    year >= 2020 ~ "2020-2023",
    year >= 2016 ~ "2016-2019",
    year >= 2012 ~ "2012-2015")) |> 
  summarize(avg = mean(value, na.rm = TRUE) / 100, 
            .by = c(date, zone, quartile)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  ggplot(aes(fill = avg)) +
  gg_cc_tiles + 
  geom_sf(colour = "transparent", lwd = 0) +
  facet_grid(rows = vars(quartile), cols = vars(date)) +
  scale_fill_stepsn("Vacancy rate", colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme

# ggplot2::ggsave(filename = here::here("outputs/5/map_5_2_1_3_rent_annual.pdf"),
#                 plot = map_5_2_1_3_rent_annual, width = 7.5, height = 6)

vacancy_by_construction <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Year of Construction",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "construction", "value", "quality", "date", "year", "survey", 
              "series"))

vacancy_by_construction_z <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Rms",
      series = "Vacancy Rate", 
      dimension = "Year of Construction",
      breakdown = "Survey Zones", 
      geo_uid = "24462",
      year = x)}) |> 
  bind_rows() |> 
  set_names(c("zone", "construction", "value", "quality", "geog", "date", "year",
              "survey", "series")) |> 
  select(-geog) |> 
  filter(zone %in% cmhc_zones$zone)

vacancy_by_construction <- 
  vacancy_by_construction |> 
  bind_rows(vacancy_by_construction_z)

plot_5_2_1_3_construction_facet <-
  vacancy_by_construction |> 
  filter(is.na(zone), !is.na(value)) |>
  mutate(construction = factor(construction, levels = c(
    "Before 1960", "1960 - 1979", "1980 - 1999", "2000 or Later", "Total"))) |>
  ggplot(aes(year, value / 100, group = construction)) +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Average vacancy rate", labels = scales::percent) +
  scale_x_continuous("Year") +
  facet_wrap(~construction) +
  ggtitle("Vacancy rate for purpose-built rentals, by year of construction") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/5/plot_5_2_1_3_construction_facet.pdf"),
#                 plot = plot_5_2_1_3_construction_facet, width = 7.5, height = 6)


# 5.2.1.4 Valeur foncière (secteur lucratif, secteur non lucratif) --------

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

map_5_2_1_4 <- 
  uef |> 
  summarize(`Per property` = mean(value),
            `Per unit` = sum(value) / sum(units),
            `Per sqft` = 880 * sum(value) / sum(area, na.rm = TRUE), 
            .by = FST) |> 
  pivot_longer(-FST) |> 
  inner_join(fst) |> 
  st_as_sf() |> 
  st_filter(laval_sectors) |> 
  ggplot(aes(fill = value)) +
  # gg_cc_tiles +
  geom_sf() +
  scale_fill_stepsn("Average assessed value", colours = curbcut_colors$left_5$fill[2:6]) +
  facet_wrap(~name) +
  ggtitle("Average assessed property value by forward sortation area (2019)") +
  gg_cc_theme

# ggplot2::ggsave(filename = here::here("outputs/6/map_5_2_1_4.pdf"),
#                 plot = map_5_2_1_4, width = 7.5, height = 6)

plot_5_2_1_4_boxplot <- 
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  mutate(`Per unit` = value / units) |> 
  rename(`Per property` = value) |> 
  pivot_longer(c(`Per property`, `Per unit`)) |> 
  ggplot(aes(type, value)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~name) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_discrete("Property type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/6/plot_5_2_1_4_boxplot.pdf"),
#                 plot = plot_5_2_1_4_boxplot, width = 7.5, height = 6)

plot_5_2_1_4_year_property <-
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  summarize(n = n(), value = mean(value), .by = c(year_built, type)) |> 
  filter(year_built >= 1900) |>
  ggplot(aes(year_built, value, size = n, fill = type, 
             colour = after_scale(alpha(fill, 0.6)))) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(~type) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_continuous("Year of construction") +
  scale_size_area("Number of properties") +
  ggtitle("Average annual per-property assessed value by property type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/6/plot_5_2_1_4_year_property.pdf"),
#                 plot = plot_5_2_1_4_year_property, width = 7.5, height = 6)

plot_5_2_1_4_year_unit <- 
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  summarize(n = sum(units), value = sum(value) / sum(units), 
            .by = c(year_built, type)) |> 
  filter(year_built >= 1900) |>
  ggplot(aes(year_built, value, size = n, fill = type, 
             colour = after_scale(alpha(fill, 0.6)))) +
  geom_point() +
  gghighlight::gghighlight() +
  facet_wrap(~type) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_continuous("Year of construction") +
  scale_size_area("Number of properties") +
  ggtitle("Average annual per-unit assessed value by property type") +
  graph_theme

# ggplot2::ggsave(filename = here::here("outputs/6/plot_5_2_1_4_year_unit.pdf"),
#                 plot = plot_5_2_1_4_year_unit, width = 7.5, height = 6)


# Save --------------------------------------------------------------------

qs::qsavem(#prix_sur_marche_table, 
  new_prices, plot_5_2_1_1_percentiles, plot_5_2_1_1_units,
  rent_by_bedroom, plot_5_2_1_2_facet, plot_5_2_1_2_change_facet,
  table_5_2_1_2_five_year, map_5_2_1_2_annual, rent_by_construction,
  plot_5_2_1_2_construction_facet, table_5_2_1_2_construction_five_year, 
  vacancy_by_bedroom, vacancy_by_rent, 
  plot_5_2_1_3_facet, table_5_2_1_3_five_year, map_5_2_1_3_annual, 
  plot_5_2_1_3_rent_facet, table_5_2_1_3_rent_five_year, 
  map_5_2_1_3_rent_annual, plot_5_2_1_3_construction_facet, 
  uef, map_5_2_1_4, plot_5_2_1_4_boxplot, plot_5_2_1_4_year_property, 
  plot_5_2_1_4_year_unit, 
  file = "data/section_5_2_1.qsm")
