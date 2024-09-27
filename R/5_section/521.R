source("R/utils/startup.R")

# 5.2.1.1 -----------------------------------------------------------------

library(rvest)

# Xpaths of tables
url <- "https://www.centris.ca/en/tools/real-estate-statistics/laval"

categories <- c("total", "single_family", "condo", "plex_2_to_5_units")

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
      font = "KMR Apparat Regular"
    ),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(
      font = "KMR Apparat Regular"
    ),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size = table_font_size,
    row_group.font.size = table_font_size,
    table.width = px(6 * 96)
  )

gtsave(prix_sur_marche_table, "outputs/5/5_2_1_prixsurmarchetable.png", zoom = 1)

  


# 5.2.1.2 -----------------------------------------------------------------

# 5.2.1.3 -----------------------------------------------------------------

# 5.2.1.4 -----------------------------------------------------------------

# 5.2.1.5 -----------------------------------------------------------------

# 5.2.1.6 -----------------------------------------------------------------

# 5.2.1.7 -----------------------------------------------------------------

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
  summarize(`Per property` = mean(value),
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
  rename(`Per property` = value) |> 
  pivot_longer(c(`Per property`, `Per unit`)) |> 
  ggplot(aes(type, value)) +
  geom_boxplot(outliers = FALSE) +
  facet_wrap(~name) +
  scale_y_continuous("Assessed value", labels = scales::dollar) +
  scale_x_discrete("Property type") +
  theme_minimal()

plot_6_1_12_year_property <-
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
  ggtitle("Average annual per-property assessed value by property type") +
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



# Save --------------------------------------------------------------------

qs::qsavem(prix_sur_marche_table, uef, map_6_1_12, plot_6_1_12_boxplot,
           plot_6_1_12_year_property, plot_6_1_12_year_unit, file = "data/5_2_1.qsm")
