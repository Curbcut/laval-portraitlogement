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

library(rvest)

# Xpaths of tables
url <- "https://www.centris.ca/en/tools/real-estate-statistics/laval"

categories <- c("total", "single_family", "condo", "plex_2_to_5_units")
page <- rvest::read_html(url)

get_table <- function(url) {
  tables <- lapply(seq_along(categories), \(x) {
    total <-
      read_html(url) |>
      html_nodes(xpath=glue::glue('//*[@id="contenuStats"]/div[{x}]/table')) %>%
      html_table()
    total <- as.data.frame(total[[1]])
    total <- total[c(1,4,7,10), c(1, 8)]
    names(total) <- c("indicateur", "last_12months")
    total$last_12months <- gsub(",|\\$", "", total$last_12months)
    total$last_12months <- as.numeric(total$last_12months)
    total$category <- categories[[x]]
    total
  })
  Reduce(rbind, tables)
}


# For all sectors
sectors <- page |>
  html_nodes(xpath ='//*[@id="CityDistrictCommunity"]/li') |>
  html_attr("data-option-value")

sectors_name <- page |>
  html_nodes(xpath ='//*[@id="CityDistrictCommunity"]/li') |>
  html_nodes("a") |>
  html_text()

sectors <- sectors[sectors  != ""]

sectors_scrape <-
  sapply(paste0(url, "/", sectors), get_table, simplify = FALSE, USE.NAMES = TRUE)

sectors_name <- sectors_name[sectors_name != "All municipalities"]
sectors_name <- gsub("(Laval \\()|(\\))", "", sectors_name)

sectors <- Reduce(rbind, mapply(\(df, name) {
  df$sector <- name
  df
}, sectors_scrape, sectors_name, SIMPLIFY = FALSE)) |>
  tibble::as_tibble()

sectors <- sectors[sectors$indicateur %in% c("Sales", "Average selling time (days)", "Median  price"), ]

z <- sectors %>%
  pivot_wider(
    names_from = c(category, indicateur),
    values_from = last_12months
  )

prix_sur_marche_table <-
  gt(z[c(1,3:ncol(z))]) |>
  fmt_missing(
    columns = everything(),  # Apply to all columns
    missing_text = ""        # Replace NA with blank
  ) |>
  data_color(
    columns = c(3,5,6,8,9,11)-1,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(columns = c(3,5,6,8,9,11)-1, fns = convert_number) |>
  data_color(
    columns = c(4,7,10)-1,
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(columns = c(4,7,10)-1, fns = \(x) paste0(convert_number(x), " $")) |>
  # tab_spanner(
  #   label = "Laval",
  #   columns = c(`total_Sales`)
  # ) |>
  tab_spanner(
    label = "Unifamiliale",
    columns = c(`single_family_Sales`,
                `single_family_Median  price`,
                `single_family_Average selling time (days)`)
  ) |>
  tab_spanner(
    label = "Copropriété",
    columns = c(`condo_Sales`,
                `condo_Median  price`,
                `condo_Average selling time (days)`)
  ) |>
  tab_spanner(
    label = "Plex (2 à 5 logements)",
    columns = c(`plex_2_to_5_units_Sales`,
                `plex_2_to_5_units_Median  price`,
                `plex_2_to_5_units_Average selling time (days)`)
  ) |>
  cols_label(
    sector = "Secteur",
    # `total_Sales` = "Ventes",
    `single_family_Sales` = "Ventes",
    `single_family_Median  price` = "Prix médian",
    `single_family_Average selling time (days)` = "Délai de vente moyen (jours)",

    `condo_Sales` = "Ventes",
    `condo_Median  price` = "Prix médian",
    `condo_Average selling time (days)` = "Délai de vente moyen (jours)",

    `plex_2_to_5_units_Sales` = "Ventes",
    `plex_2_to_5_units_Median  price` = "Prix médian",
    `plex_2_to_5_units_Average selling time (days)` = "Délai de vente moyen (jours)"
  ) |>
  tab_style(
    style = cell_text(
      font = font_local_name
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = font_local_name
    ),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size = table_font_size,
    row_group.font.size = table_font_size,
    table.width = px(6 * 96)
  )

gt_save_word(prix_sur_marche_table, "outputs/5/11_prixsurmarchetable.docx")

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
    name == "p20" ~ "20e percentile",
    name == "p40" ~ "40e percentile",
    name == "median" ~ "Médian",
    name == "p60" ~ "60e percentile",
    name == "p80" ~ "80e percentile")) |> 
  mutate(name = factor(name, levels = c("20e percentile", "40e percentile", 
                                        "Médian", "60e percentile", 
                                        "80e percentile"))) |> 
  filter(year != 2019) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_line(size=1) +
  scale_y_continuous("Prix", labels = convert_dollar) +
  scale_x_continuous("Année") +
  scale_colour_manual(values = curbcut_colors$left_5$fill[2:6]) +
  guides(colour = guide_legend(nrow = 2)) +  # Set legend to two rows
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/5/34_prx_absorbe.pdf"), 
                plot = plot_5_2_1_1_percentiles, width = 6.5, height = 4)

plot_5_2_1_1_units <-
  new_prices |> 
  ggplot(aes(year, units)) +
  geom_line(size=1) +
  scale_y_continuous("Unités", labels = convert_number) +
  scale_x_continuous("Année") +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/5/35_absorbes.pdf"), 
                plot = plot_5_2_1_1_units, width = 6.5, height = 2.5)

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
  mutate(bedroom = case_when(
    bedroom == "Bachelor" ~ "Studio",
    bedroom == "1 Bedroom" ~ "1 chambre",
    bedroom == "2 Bedroom" ~ "2 chambres",
    bedroom == "3 Bedroom +" ~ "3+ chambres",
    bedroom == "Total" ~ "Total",
    TRUE ~ bedroom
  ),
  bedroom = factor(bedroom, levels = c("Studio", "1 chambre", "2 chambres", "3+ chambres", "Total"))
  ) |> 
  filter(is.na(zone)) |>
  ggplot(aes(year, value, group = bedroom)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Loyer mensuel moyen", labels = convert_dollar) +
  scale_x_continuous("Année") +
  facet_wrap(~bedroom) +
  graph_theme

 ggsave_pdf_png(filename = here::here("outputs/5/36_loyermoyen_rentmarket.pdf"),
      plot = plot_5_2_1_2_facet, width = 6.5, height = 5)

plot_5_2_1_2_change_facet <-
  rent_by_bedroom |> 
  arrange(zone, bedroom, year) |> 
  mutate(value = slider::slide_dbl(value, \(x) x[2] - x[1], .before = 1, 
                                   .complete = TRUE),
         .by = zone, bedroom) |> 
  mutate(bedroom = case_when(
    bedroom == "Bachelor" ~ "Studio",
    bedroom == "1 Bedroom" ~ "1 chambre",
    bedroom == "2 Bedroom" ~ "2 chambres",
    bedroom == "3 Bedroom +" ~ "3+ chambres",
    bedroom == "Total" ~ "Total",
    TRUE ~ bedroom
  ),
  bedroom = factor(bedroom, levels = c("Studio", "1 chambre", "2 chambres", "3+ chambres", "Total"))
  ) |> 
  filter(is.na(zone)) |>
  filter(year >= 1991) |> 
  ggplot(aes(year, value, group = bedroom)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Changement annuel du loyer moyen", labels = convert_dollar) +
  scale_x_continuous("Année") +
  facet_wrap(~bedroom) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/5/37_loyer_moyen_variation.pdf"),
                 plot = plot_5_2_1_2_change_facet, width = 6.5, height = 5)

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
  mutate(across(-`Date Range`, convert_dollar)) |> 
  gt::gt() |>
  gt::cols_label(
  `Date Range` = "Période",
  Total = "Total",
  Bachelor = "Studio",
  `1 Bedroom` = "1 chambre",
  `2 Bedroom` = "2 chambres",
  `3 Bedroom +` = "3+ chambres"
)

# gtsave(table_5_2_1_2_five_year, "outputs/5/table_5_2_1_2_five_year.png", zoom = 1)
gt_save_word(gt_table = table_5_2_1_2_five_year, file_path = "outputs/5/12_loyer_rentmarket.docx")

# Map of average rents by five-year chunk
map_5_2_1_2_annual <-
  rent_by_bedroom |> 
  mutate(bedroom = case_when(
    bedroom == "Bachelor" ~ "Studio",
    bedroom == "1 Bedroom" ~ "1 chambre",
    bedroom == "2 Bedroom" ~ "2 chambres",
    bedroom == "3 Bedroom +" ~ "3+ chambres",
    bedroom == "Total" ~ "Total",
    TRUE ~ bedroom
  ),
  bedroom = factor(bedroom, levels = c("Studio", "1 chambre", "2 chambres", "3+ chambres", "Total"))
  ) |> 
  filter(!is.na(zone)) |>
  mutate(date = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2010 ~ "2009-2013")) |> 
  summarize(avg = mean(value, na.rm = TRUE), .by = c(date, zone, bedroom)) |> 
  inner_join(cmhc_zones) |> 
  st_as_sf() |> 
  ggplot(aes(fill = avg)) +
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_grid(rows = vars(bedroom), cols = vars(date)) +
  scale_fill_stepsn("Loyen mensuel moyen", 
                    labels = \(x) paste(convert_number(x), "$"),
                    limits = c(400, 1200),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

 ggsave_pdf_png(filename = here::here("outputs/5/38_loyer_trend.pdf"),
                plot = map_5_2_1_2_annual, width = 7.5, height = 9)

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
rent_by_construction$construction <- 
  case_when(rent_by_construction$construction == "Before 1960" ~ "Avant 1960",
                          rent_by_construction$construction == "1960 - 1979" ~ "1960 - 1979",
                          rent_by_construction$construction == "1980 - 1999" ~ "1980 - 1999",
                          rent_by_construction$construction == "2000 or Later" ~ "Après 2000",
                          rent_by_construction$construction == "Total" ~ "Total")
rent_by_construction$construction <- factor(rent_by_construction$construction,
                                            levels = c("Avant 1960",
                                                       "1960 - 1979",
                                                       "1980 - 1999",
                                                       "Après 2000",
                                                       "Total"))

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
rent_by_construction_z$construction <- 
  case_when(rent_by_construction_z$construction == "Before 1960" ~ "Avant 1960",
            rent_by_construction_z$construction == "1960 - 1979" ~ "1960 - 1979",
            rent_by_construction_z$construction == "1980 - 1999" ~ "1980 - 1999",
            rent_by_construction_z$construction == "2000 or Later" ~ "Après 2000",
            rent_by_construction_z$construction == "Total" ~ "Total")
rent_by_construction_z$construction <- factor(rent_by_construction_z$construction,
                                            levels = c("Avant 1960",
                                                       "1960 - 1979",
                                                       "1980 - 1999",
                                                       "Après 2000",
                                                       "Total"))

rent_by_construction <- 
  rent_by_construction |> 
  bind_rows(rent_by_construction_z)

plot_5_2_1_2_construction_facet <-
  rent_by_construction |> 
  filter(is.na(zone), !is.na(value)) |>
  ggplot(aes(year, value, group = construction)) +
  geom_line(size=1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Loyer mensuel moyen", labels = convert_dollar) +
  scale_x_continuous("Année") +
  facet_wrap(~construction) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/5/39_loyer_rentmarket_age.pdf"),
                 plot = plot_5_2_1_2_construction_facet, width = 6.5, height = 5)

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
  # mutate(across(-`Date Range`, convert_dollar)) |> 
  gt::gt() |> 
  data_color(
    columns = c(2:6),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = c(427, 1380)
    )
  ) |> 
  fmt(columns = c(2:6), fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_dollar(y))) |> 
  gt::cols_label(
    `Date Range` = "Période",
  )

gt_save_word(gt_table = table_5_2_1_2_construction_five_year, file_path = "outputs/5/13_loyer_rentmarket_age.docx")

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
  mutate(bedroom = case_when(
    bedroom == "Bachelor" ~ "Studio",
    bedroom == "1 Bedroom" ~ "1 chambre",
    bedroom == "2 Bedroom" ~ "2 chambres",
    bedroom == "3 Bedroom +" ~ "3+ chambres",
    bedroom == "Total" ~ "Total",
    TRUE ~ bedroom
  ),
  bedroom = factor(bedroom, levels = c("Studio", "1 chambre", "2 chambres", "3+ chambres", "Total"))
  ) |> 
  filter(is.na(zone), !is.na(value)) |>
  ggplot(aes(year, value / 100, group = bedroom)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Taux d'inoccupation moyen", labels = convert_pct) +
  scale_x_continuous("Year") +
  facet_wrap(~bedroom) +
  graph_theme

 ggsave_pdf_png(filename = here::here("outputs/5/40_inoccupation_rentalmarket.pdf"),
                 plot = plot_5_2_1_3_facet, width = 6.5, height = 5)

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
  # mutate(across(-`Date Range`, convert_pct)) |> 
  gt::gt() |> 
  data_color(
    columns = c(2:6),
    colors = scales::col_numeric(
      palette = c("white", color_theme("pinkhealth")),
      domain = c(0,0.03), reverse = TRUE, 
      na.color = "white"
    )
  ) |> 
  fmt(columns = c(2:6), fns = convert_pct) |> 
  gt::cols_label(
    `Date Range` = "Période",
    Total = "Total",
    Bachelor = "Studio",
    `1 Bedroom` = "1 chambre",
    `2 Bedroom` = "2 chambres",
    `3 Bedroom +` = "3+ chambres"
  )

# gtsave(table_5_2_1_3_five_year, "outputs/5/table_5_2_1_3_five_year.png", zoom = 1)
gt_save_word(gt_table = table_5_2_1_3_five_year, 
      file_path = "outputs/5/14_inoccupation_rentmarket_cc.docx")

# Map of vacancy rate by five-year chunk
map_5_2_1_3_annual <-
  vacancy_by_bedroom |> 
  mutate(bedroom = case_when(
    bedroom == "Bachelor" ~ "Studio",
    bedroom == "1 Bedroom" ~ "1 chambre",
    bedroom == "2 Bedroom" ~ "2 chambres",
    bedroom == "3 Bedroom +" ~ "3+ chambres",
    bedroom == "Total" ~ "Total",
    TRUE ~ bedroom
  ),
  bedroom = factor(bedroom, levels = c("Studio", "1 chambre", "2 chambres", "3+ chambres", "Total"))
  ) |> 
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_grid(rows = vars(bedroom), cols = vars(date)) +
  scale_fill_stepsn("Taux d'innocupation", 
                    limits = c(0,0.04),
                    colours = curbcut_colors$left_5$fill[2:6], 
                    label = convert_pct) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

 ggsave_pdf_png(filename = here::here("outputs/5/41_inoccupation_rentmarket_CC.pdf"),
               plot = map_5_2_1_3_annual, width = 7.5, height = 9)

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
  geom_line(size=1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Taux d'innocupation moyen", labels = convert_pct) +
  scale_x_continuous("Année") +
  facet_wrap(~quartile) +
  graph_theme

 ggsave_pdf_png(filename = here::here("outputs/5/42_inoccupation_rentmarket_Q.pdf"),
                plot = plot_5_2_1_3_rent_facet, width = 6.5, height = 5)

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
  # mutate(across(-`Date Range`, convert_pct)) |> 
  gt::gt() |> 
  data_color(
    columns = c(2:5),
    colors = scales::col_numeric(
      palette = c("white", color_theme("pinkhealth")),
      domain = c(0,0.03), reverse = TRUE,
      na.color = "white"
    )
  ) |> 
  fmt(columns = c(2:5), fns = convert_pct) |> 
  gt::cols_label(
    `Date Range` = "Période")

# gtsave(table_5_2_1_3_rent_five_year, "outputs/5/table_5_2_1_3_rent_five_year.png", zoom = 1)
gt_save_word(gt_table = table_5_2_1_3_rent_five_year, 
             file_path = "outputs/5/15_inoccupation_Q.docx")


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
  # gg_cc_tiles + 
  geom_sf(colour = "black") +
  facet_grid(rows = vars(quartile), cols = vars(date)) +
  scale_fill_stepsn("Taux d'innocupation", colours = curbcut_colors$left_5$fill[2:6],
                    limits = c(0, .04),
                    label = convert_pct) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/43_carte_inoccupation_Q.pdf"),
              plot = map_5_2_1_3_rent_annual, width = 7.5, height = 9)

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
  mutate(construction = case_when(
    construction == "Before 1960" ~ "Avant 1960",
    construction == "2000 or Later" ~ "Après 2000",
    TRUE ~ construction  
  )) |>  
  filter(is.na(zone), !is.na(value)) |>
  mutate(construction = factor(construction, levels = c(
    "Avant 1960", "1960 - 1979", "1980 - 1999", "Après 2000", "Total"))) |>
  ggplot(aes(year, value / 100, group = construction)) +
  geom_line(size=1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Taux d'inoccupation moyen", labels = convert_pct) +
  scale_x_continuous("Année") +
  facet_wrap(~construction) +
  graph_theme

 ggsave_pdf_png(filename = here::here("outputs/5/44_inoccupation_rentkmarket_age.pdf"),
                plot = plot_5_2_1_3_construction_facet, width = 6.5, height = 5)


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

plot_5_2_1_4_boxplot <- 
  uef |> 
  mutate(type = if_else(str_detect(type, "rangée"), "En rangée", type)) |> 
  mutate(`Per unit` = value / units) |> 
  rename(`Per property` = value) |> 
  pivot_longer(c(`Per property`, `Per unit`)) |> 
  ggplot(aes(type, value)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~name, labeller = as_labeller(c(
    `Per property` = "Par propriété",
    `Per unit` = "Par unité"
  ))) +
  scale_y_continuous("Valeur foncière", labels = convert_dollar) +
  scale_x_discrete("Type de propriété") +
  graph_theme

 ggsave_pdf_png(filename = here::here("outputs/5/45_valeur_boxplot.pdf"),
                plot = plot_5_2_1_4_boxplot, width = 6.5, height = 4)

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
  scale_y_continuous("Valeur foncière", labels = convert_dollar) +
  scale_x_continuous("Année de construction") +
  scale_size_continuous(name = "Nombre de propriétés") +  # Ensure the legend title is explicitly set here
  scale_fill_manual(values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 11))

 ggsave_pdf_png(filename = here::here("outputs/5/46_valeur_type_annee.pdf"),
               plot = plot_5_2_1_4_year_property, width = 6.5, height = 5)

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
  scale_y_continuous("Valeur foncière", labels = convert_dollar) +
  scale_x_continuous("Année de construction") +
  scale_size_continuous(name = "Nombre de propriétés") +
  scale_fill_manual(values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 11))


ggsave_pdf_png(filename = here::here("outputs/5/47_valeur_type_anne_unite.pdf"),
                plot = plot_5_2_1_4_year_unit, width = 6.5, height = 5)

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
  sf::st_transform(crs = 32618) |> 
  ggplot(aes(fill = value)) +
  # gg_cc_tiles +
  geom_sf(color = "black") +
  scale_fill_stepsn("Valeur foncière moyenne", 
                    colours = curbcut_colors$left_5$fill[2:6],
                    labels = convert_dollar) +
  facet_wrap(~name, labeller = as_labeller(c(
    `Per property` = "Par propriété",
    `Per unit` = "Par unité",
    `Per sqft` = "Par pied carré"
  ))) +
  gg_cc_theme_nodistricts +
  theme(legend.text.align = 0.5) +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/48_valeur_prop_piedcarre_unite.pdf"),
               plot = map_5_2_1_4, width = 6.5, height = 3)



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
