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
      year = x,
      frequency = "Annual")}) |> 
  bind_rows() |> 
  set_names(c("zone", "type", "value", "date", "year", "survey", "series")) |> 
  mutate(type = case_when(type == "Single" ~ "Unifamilial",
                          type == "Semi-Detached" ~ "Jumelé",
                          type == "Row" ~ "En rangée",
                          type == "Apartment" ~ "Appartement",
                          type == "All" ~ "Total"
  ))

completions_by_market <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Completions", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x,
      frequency = "Annual")}) |> 
  bind_rows() |> 
  set_names(c("zone", "market", "value", "date", "year", "survey", "series")) |> 
  mutate(market = case_when(market == "Homeowner" ~ "Prop.-occ.",
                            market == "Rental" ~ "Locatif",
                            market == "Condo" ~ "Copropriété",
                            market == "Co-Op" ~ "Coopératif",
                            market == "Unknown" ~ "Non disponible",
                            market == "All" ~ "Total"
  ))

#' Overall completions mostly stable over last 30 years, albeit with high 
#' levels of year-to-year variability
plot_6_1_1_overall <- 
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type == "Total") |>
  mutate(value_trend = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  pivot_longer(c(value, value_trend)) |> 
  mutate(name = if_else(name == "value", "Réel", 
                        "Moyenne mobile sur cinq ans")) |> 
  ggplot(aes(year, value, group = name, linewidth = name, alpha = name)) +
  geom_line(size = 1) +
  scale_y_continuous("Logements achevés", labels = convert_number) +
  scale_x_continuous(NULL) +
  scale_linewidth_manual(name = NULL, values = c(
    "Réel" = 0.4, "Moyenne mobile sur cinq ans" = 1)) +
  scale_alpha_manual(name = NULL, values = c(
    "Réel" = 0.2, "Moyenne mobile sur cinq ans" = 1)) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/54_acheves.pdf"),
                plot = plot_6_1_1_overall, width = 6.5, height = 3)

# Table with 5-year aggregations
table_6_1_1_five_year <- 
  completions_by_type |>
  filter(is.na(zone)) |>
  filter(type != "All") |>
  mutate("Années" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |>
  summarize(avg = mean(value), .by = c(`Années`, type)) |>
  pivot_wider(names_from = type, values_from = avg) |>
  mutate(
    Total = Unifamilial + `Jumelé` + `En rangée` + Appartement,
    across(c(Unifamilial, `Jumelé`, `En rangée`, Appartement), 
           list(n = ~., pct = ~. / Total), .names = "{.col}_{.fn}")
  ) |>
  transmute(
    "Années" = `Années`,
    "Total (n)" = Total,
    "Unifamilial (n)" = Unifamilial_n,
    "Unifamilial (%)" = Unifamilial_pct,
    "Jumelé (n)" = `Jumelé_n`,
    "Jumelé (%)" = `Jumelé_pct`,
    "En rangée (n)" = `En rangée_n`,
    "En rangée (%)" = `En rangée_pct`,
    "Appartement (n)" = Appartement_n,
    "Appartement (%)" = Appartement_pct
  ) |>
  gt::gt() |>
  data_color(
    columns = c("Unifamilial (%)", "Jumelé (%)", "En rangée (%)", "Appartement (%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(
    columns = c("Total (n)", "Unifamilial (n)", "Jumelé (n)", 
                "En rangée (n)", "Appartement (n)"),
    fns = convert_number
  ) |>
  fmt(
    columns = c("Unifamilial (%)", "Jumelé (%)", "En rangée (%)", "Appartement (%)"),
    fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))
  ) |>
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()
  ) |>
  tab_options(table.font.size = 12)


gt_save_word(table_6_1_1_five_year, "outputs/6/17_acheve_moy_typologie.docx")

# Comparison of long-term residential completion trends by building type
plot_6_1_1_type <- 
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "Total") |>
  ggplot(aes(year, value, colour = type)) +
  geom_line(size = 1) +
  scale_y_continuous("Logements achevés", label = convert_number) +
  scale_x_continuous(NULL) +
  scale_colour_manual("Type de logement", 
                      values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  theme_minimal() +
  default_theme +
  theme(legend.position = "bottom",
        legend.title.position = "top")

ggsave_pdf_png(plot_6_1_1_type, "outputs/6/55_acheves_type.pdf", width = 6.5,
               height = 3)

# Variation with between-type difference emphasized
plot_6_1_1_type_facet <-
  completions_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "All") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  ggplot(aes(year, value, group = type)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Logements achevés") +
  scale_x_continuous(NULL) +
  facet_wrap(~type) +
  # ggtitle(
  #   "Annual housing completions by dwelling type (Moyenne mobile sur cinq ans)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave_pdf_png(plot_6_1_1_type_facet, "outputs/6/56_acheves_type_5yrrolling.pdf", 
               width = 6.5, height = 4)

# Comparison of long-term residential completion trends by intended market
plot_6_1_1_market <- 
  completions_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "Total") |>
  ggplot(aes(year, value, colour = market)) +
  geom_line(size = 1) +
  scale_y_continuous("Logements achevés", label = convert_number) +
  scale_x_continuous(NULL) +
  scale_colour_manual("Marché visé", 
                      values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  theme_minimal() +
  default_theme +
  theme(legend.position = "bottom",
        legend.title.position = "top")

ggsave_pdf_png(plot_6_1_1_market, "outputs/6/57_acheves_marchevise.pdf", 
               width = 6.5, height = 3)

# Variation with between-market difference emphasized
plot_6_1_1_market_facet <-
  completions_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "Total") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = market) |>
  ggplot(aes(year, value, group = market)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Logements achevés") +
  scale_x_continuous(NULL) +
  facet_wrap(~market) +
  # ggtitle(
  #   "Annual housing completions by intended market (Moyenne mobile sur cinq ans)"
  #   ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave_pdf_png(plot_6_1_1_market_facet, "outputs/6/58_acheves_marchevise_5yrrolling.pdf", 
               width = 6.5, height = 4)

# Map of housing completions by five-year chunk
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Mises en chantier annuelles", colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Mises en chantier pour 1000 logements", 
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

map_6_1_2_annual <- 
  patchwork::wrap_plots(map_6_1_2_annual_1, map_6_1_2_annual_2)


map_6_1_1_annual_1 <-
  completions_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "Total") |> 
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Logements achevés", 
                    limits = c(0, 1000),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(1, "cm"),
        legend.title.position = "top")
  
map_6_1_1_annual_2 <-
  completions_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "Total") |> 
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Logements achevés pour 1000 logements",
                    limits = c(0,20),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(1, "cm"),
        legend.title.position = "top")

map_6_1_1_annual <- 
  patchwork::wrap_plots(map_6_1_1_annual_1, map_6_1_1_annual_2)

ggsave_pdf_png(map_6_1_1_annual, "outputs/6/59_cartes_acheves.pdf", 
               width = 6.5, height = 6)

# 6.1.2 Mises en chantier par typologie -----------------------------------

starts_by_type <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Dwelling Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x,
      frequency = "Annual")}) |> 
  bind_rows() |> 
  set_names(c("zone", "type", "value", "date", "year", "survey", "series")) |> 
  mutate(type = case_when(type == "Single" ~ "Unifamilial",
                          type == "Semi-Detached" ~ "Jumelé",
                          type == "Row" ~ "En rangée",
                          type == "Apartment" ~ "Appartement",
                          type == "All" ~ "Total"
  ))

#' Overall starts
plot_6_1_2_overall <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type == "Total") |>
  mutate(value_trend = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  pivot_longer(c(value, value_trend)) |> 
  mutate(name = if_else(name == "value", "Réel", 
                        "Moyenne mobile quinquennale")) |> 
  ggplot(aes(year, value, group = name, linewidth = name, alpha = name)) +
  geom_line(size = 1) +
  scale_y_continuous("Mises en chantier") +
  scale_x_continuous(NULL) +
  scale_linewidth_manual(name = NULL, values = c(
    "Réel" = 0.4, "Moyenne mobile quinquennale" = 1)) +
  scale_alpha_manual(name = NULL, values = c(
    "Réel" = 0.2, "Moyenne mobile quinquennale" = 1)) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/60_chantier.pdf"),
                plot = plot_6_1_2_overall, width = 6.5, height = 3)


# Table with 5-year aggregations
table_6_1_2_five_year <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  mutate("Années" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |>
  summarize(avg = mean(value), .by = c(`Années`, type)) |>
  pivot_wider(names_from = type, values_from = avg) |>
  mutate(
    Total = Unifamilial + `Jumelé` + `En rangée` + Appartement,
    across(c(Unifamilial, `Jumelé`, `En rangée`, Appartement), 
           list(n = ~., pct = ~. / Total), .names = "{.col}_{.fn}")
  ) |>
  transmute(
    "Années" = `Années`,
    "Total (n)" = Total,
    "Unifamilial (n)" = Unifamilial_n,
    "Unifamilial (%)" = Unifamilial_pct,
    "Jumelé (n)" = `Jumelé_n`,
    "Jumelé (%)" = `Jumelé_pct`,
    "En rangée (n)" = `En rangée_n`,
    "En rangée (%)" = `En rangée_pct`,
    "Appartement (n)" = Appartement_n,
    "Appartement (%)" = Appartement_pct
  ) |>
  gt::gt() |>
  data_color(
    columns = c("Unifamilial (%)", "Jumelé (%)", "En rangée (%)", "Appartement (%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(
    columns = c("Total (n)", "Unifamilial (n)", "Jumelé (n)", 
                "En rangée (n)", "Appartement (n)"),
    fns = convert_number
  ) |>
  fmt(
    columns = c("Unifamilial (%)", "Jumelé (%)", "En rangée (%)", "Appartement (%)"),
    fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))
  ) |>
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()
  ) |>
  tab_options(table.font.size = 12)

gt_save_word(gt_table = table_6_1_2_five_year, 
             file_path = "outputs/6/18_chantier_moy_typologie.docx")

# Comparison of long-term residential start trends by building type
plot_6_1_2_type <-
  starts_by_type |> 
  filter(is.na(zone)) |> 
  filter(type != "Total") |>
  ggplot(aes(year, value, colour = type)) +
  geom_line(size = 1) +
  scale_y_continuous("Mises en chantier", label = convert_number) +
  scale_x_continuous(NULL) +
  scale_colour_manual("Type de logement", 
                      values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  graph_theme


# Variation with between-type difference emphasized
plot_6_1_2_type_facet <-
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "Total") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = type) |>
  ggplot(aes(year, value, group = type, colour = type)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Mises en chantier") +
  scale_x_continuous(NULL) +
  scale_colour_manual("Type de logement", 
                      values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  facet_wrap(~type) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/61_chantier_type_5yrrolling.pdf"),
               plot = plot_6_1_2_type_facet, width = 6.5, height = 4)


# Pct of annual starts which are apartment
plot_6_1_2_type_apart <- 
  starts_by_type |> 
  filter(is.na(zone)) |>
  filter(type != "Total") |>
  summarize(apart_pct = value[type == "Appartement"] / sum(value), .by = year) |>
  ggplot(aes(year, apart_pct)) +
  geom_line(size = 1) +
  scale_y_continuous("Mises en chantier", limits = c(0, 1), labels = convert_pct) +
  scale_x_continuous(NULL) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/62_chantier_propapp.pdf"),
                plot = plot_6_1_2_type_apart, width = 6.5, height = 3)

# Map of housing starts by five-year chunk
map_6_1_2_annual_1 <- 
  starts_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "Total") |> 
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Mises en chantier annuelles", colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(1, "cm"),
        legend.title.position = "top")

map_6_1_2_annual_2 <- 
  starts_by_type |> 
  filter(!is.na(zone)) |>
  filter(type == "Total") |> 
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
  # gg_cc_tiles +
  geom_sf(colour = "black") +
  facet_wrap(vars(date), nrow = 3) +
  scale_fill_stepsn("Mises en chantier pour 1000 logements", 
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme_nodistricts +
  theme(legend.key.width = unit(1, "cm"),
        legend.title.position = "top")

map_6_1_2_annual <- 
  patchwork::wrap_plots(map_6_1_2_annual_1, map_6_1_2_annual_2)

ggsave_pdf_png(map_6_1_2_annual, "outputs/6/63_cartes_chantier.pdf", 
               width = 6.5, height = 6)

# 6.1.3 Mises en chantier par mode d'occupation ---------------------------

starts_by_market <- 
  map(1990:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x,
      frequency = "Annual")}) |> 
  bind_rows() |> 
  set_names(c("zone", "market", "value", "date", "year", "survey", "series")) |> 
  mutate(market = case_when(market == "Homeowner" ~ "Prop.-occ.",
                            market == "Rental" ~ "Locatif",
                            market == "Condo" ~ "Copropriété",
                            market == "Co-Op" ~ "Coopératif",
                            market == "Unknown" ~ "Non disponible",
                            market == "All" ~ "Total"
  )) |> 
  filter(market != "Non disponible", !is.na(market))

starts_by_market$market <- factor(starts_by_market$market,
                                  levels = c("Total", "Prop.-occ.",
                                             "Locatif", "Copropriété",
                                             "Coopératif"))

# Table with 5-year aggregations
table_6_1_3_five_year <- 
  starts_by_market |> 
  filter(is.na(zone)) |>
  mutate("Période" = case_when(
    year >= 2019 ~ "2019-2023",
    year >= 2014 ~ "2014-2018",
    year >= 2009 ~ "2009-2013",
    year >= 2004 ~ "2004-2008",
    year >= 1999 ~ "1999-2003",
    year >= 1994 ~ "1994-1998",
    year >= 1990 ~ "1990-1993")) |> 
  summarize(Moyenne = mean(value), .by = c(`Période`, market)) |> 
  pivot_wider(names_from = market, values_from = Moyenne) |> 
  mutate(
    across(c(`Locatif`, `Copropriété`, `Prop.-occ.`, `Coopératif`), 
           list(n = ~., pct = ~. / Total), .names = "{.col}_{.fn}")
  ) |>  transmute(
    "Période" = `Période`,
    "Total (n)" = Total,
    "Locatif (n)" = Locatif_n,
    "Locatif (%)" = Locatif_pct,
    "Copropriété (n)" = Copropriété_n,
    "Copropriété (%)" = Copropriété_pct,
    "Prop.-occ. (n)" = `Prop.-occ._n`,
    "Prop.-occ. (%)" = `Prop.-occ._pct`,
    "Coopératif (n)" = `Coopératif_n`,
    "Coopératif (%)" = `Coopératif_pct`
  ) |>
  gt() |> 
  data_color(
    columns = c("Locatif (%)", "Copropriété (%)", "Prop.-occ. (%)", "Coopératif (%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(
    columns = c("Locatif (n)", "Copropriété (n)", "Prop.-occ. (n)", "Coopératif (n)"),
    fns = convert_number
  ) |>
  fmt(
    columns = c("Locatif (%)", "Copropriété (%)", "Prop.-occ. (%)", "Coopératif (%)"),
    fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))
  ) |>
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()
  ) |>
  tab_options(table.font.size = 12)

gt_save_word(gt_table = table_6_1_3_five_year, 
             file_path = "outputs/6/19_chantier_marchevise.docx")

# Comparison of long-term residential start trends by intended market
# plot_6_1_3_market <- 
  starts_by_market |>
  filter(is.na(zone)) |>
  filter(market != "Total", market != "Unknown") |>
  ggplot(aes(year, value, colour = market)) +
  geom_line(size = 1) +
  scale_y_continuous("Mises en chantier", labels = convert_number) +
  scale_x_continuous(NULL) +
  scale_colour_manual(
    "Marché visé",
    values = curbcut_colors$brandbook$color[c(2:4, 9)]
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")


# ggsave_pdf_png(filename = here::here("outputs/6/plot_6_1_3_market.pdf"), 
#                 plot = plot_6_1_3_market, width = 6.5, height = 5)

# Variation with between-type difference emphasized
plot_6_1_3_market_facet <-
  starts_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "Total") |>
  mutate(value = slider::slide_dbl(value, mean, .before = 2, .after = 2), 
         .by = market) |>
  ggplot(aes(year, value, group = market, colour = market)) +
  geom_line(size = 1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Mises en chantier", labels = convert_number) +
  scale_x_continuous(NULL) +
  scale_colour_manual("Marché visé", 
                      values = curbcut_colors$brandbook$color[c(2:4, 9)]) +
  facet_wrap(~market) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/64_chantier_marchevise_5yrrolling.pdf"),
                plot = plot_6_1_3_market_facet, width = 6.5, height = 4)

# Pct of annual starts which are rental
plot_6_1_3_market_rental <-
  starts_by_market |> 
  filter(is.na(zone)) |>
  filter(market != "Total") |>
  summarize(rental_pct = value[market == "Locatif"] / sum(value), .by = year) |>
  ggplot(aes(year, rental_pct)) +
  geom_line(size = 1) +
  scale_y_continuous("Mises en chantier", limits = c(0, 1), labels = convert_pct) +
  scale_x_continuous(NULL) +
  graph_theme

ggsave_pdf_png(filename = here::here("outputs/6/65_chantier_proplocatif.pdf"),
                plot = plot_6_1_3_market_rental, width = 6.5, height = 3)

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
    name == "p20" ~ "20e percentile",
    name == "p40" ~ "40e percentile",
    name == "median" ~ "Médiane",
    name == "p60" ~ "60e percentile",
    name == "p80" ~ "80e percentile")) |> 
  mutate(name = factor(name, levels = c("20e percentile", "40e percentile", 
                                        "Médiane", "60e percentile", 
                                        "80e percentile"))) |> 
  filter(year != 2019) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_line(size = 1) +
  scale_y_continuous("Prix", labels = convert_dollar) +
  scale_x_continuous("Année") +
  scale_colour_manual(name = NULL,values = curbcut_colors$brandbook$color[c(2:4, 8,9)]) +
  # ggtitle(
  #   "Prix annuel moyen pour les logements absorbés en propriété et en copropriété") +
  theme_minimal() +
  theme(legend.position = "bottom")

plot_6_1_5_units <- 
  new_prices |> 
  filter(year != 2019) |> 
  ggplot(aes(year, units)) +
  geom_line(size = 1) +
  scale_y_continuous("Unités", labels = convert_number) +
  scale_x_continuous("Année") +
  # ggtitle(
  #   "Annual absorbed homeowner and condominimum units") +
  theme_minimal()


# R Markdown --------------------------------------------------------------

qs::qsavem(cmhc_zones, completions_by_type, completions_by_market, 
           plot_6_1_1_overall, table_6_1_1_five_year, plot_6_1_1_type, 
           plot_6_1_1_type_facet, plot_6_1_1_market, plot_6_1_1_market_facet, 
           map_6_1_1_annual, starts_by_type, plot_6_1_2_overall, 
           table_6_1_2_five_year, plot_6_1_2_type, plot_6_1_2_type_facet, 
           plot_6_1_2_type_apart, map_6_1_2_annual, starts_by_market,
           table_6_1_3_five_year, plot_6_1_3_market, plot_6_1_3_market_facet,
           plot_6_1_3_market_rental, new_prices, plot_6_1_5_percentiles,
           plot_6_1_5_units,
           file = "data/section_6_1.qsm")
