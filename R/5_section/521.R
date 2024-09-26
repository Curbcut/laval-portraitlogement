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

# 5.2.1.8 -----------------------------------------------------------------



# Save --------------------------------------------------------------------

qs::qsavem(prix_sur_marche_table, file = "data/5_2_1.qsm")
