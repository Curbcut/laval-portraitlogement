source("R/utils/startup.R")

# 5.3.1 -------------------------------------------------------------------

# 5.3.2 -------------------------------------------------------------------
commute21v <- c("total" = "v_CA21_7617", 
                "same_csd" = "v_CA21_7620",
                "diffcsd_samecd" = "v_CA21_7623", 
                "diffcsd_diffcd" = "v_CA21_7626", 
                "diff_prov" = "v_CA21_7629")
commute16v <- c("total" = "v_CA16_5777", 
                "same_csd" = "v_CA16_5780",
                "diffcsd_samecd" = "v_CA16_5783", 
                "diffcsd_diffcd" = "v_CA16_5786", 
                "diff_prov" = "v_CA16_5789")


#Creating a function to grab data for commute destination and to calculate new vectors to be used
commute_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) |>
    mutate(Year = cyear, "Within CSD" = same_csd / total,
           "Outside CSD" = (diffcsd_samecd + diffcsd_diffcd + diff_prov) / total) |> 
    select(Year, "Within CSD", "Outside CSD")
}

#Grabbing the data for years 2016 and 2021
commute21 <- commute_grabber("CA21", commute21v, "2021")
commute16 <- commute_grabber("CA16", commute16v, "2016")

commute_grabber_qc <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(PR = 24), 
             level = "PR",
             vectors = cvector) |>
    mutate(Year = cyear, "Within CSD" = same_csd / total,
           "Outside CSD" = (diffcsd_samecd + diffcsd_diffcd + diff_prov) / total) |> 
    select(Year, "Within CSD", "Outside CSD")
}

#Grabbing the data for years 2016 and 2021
commute21_qc <- commute_grabber_qc("CA21", commute21v, "2021")
commute16_qc <- commute_grabber_qc("CA16", commute16v, "2016")


commute_dest <- bind_rows(commute21, commute16)
commute_dest_qc <- bind_rows(commute21_qc, commute16_qc)

emploi_within_CSD <- commute_dest$`Within CSD`[commute_dest$Year == 2021] |> convert_pct()
emploi_within_CSD_2016 <- commute_dest$`Within CSD`[commute_dest$Year == 2016] |> convert_pct()
emploi_outside_CSD <- commute_dest$`Outside CSD`[commute_dest$Year == 2021] |> convert_pct()
emploi_outside_CSD_2016 <- commute_dest$`Outside CSD`[commute_dest$Year == 2016] |> convert_pct()


emploi_within_CSD_qc <- commute_dest_qc$`Within CSD`[commute_dest_qc$Year == 2021] |> convert_pct()
emploi_within_CSD_2016_qc <- commute_dest_qc$`Within CSD`[commute_dest_qc$Year == 2016] |> convert_pct()


# Place of Work -----------------------------------------------------------
#Grabbing the vectors for place of work in 2016 and 2011
place21v <- c("total" = "v_CA21_7602", "wfh" = "v_CA21_7605", "outside_ca" = "v_CA21_7608",
              "no_fix" = "v_CA21_7611", "usual" = "v_CA21_7614")
place16v <- c("total" = "v_CA16_5762", "wfh" = "v_CA16_5765", "outside_ca" = "v_CA16_5768",
              "no_fix" = "v_CA16_5771", "usual" = "v_CA16_5774")
place11v <- c("total" = "v_CA11N_2176", "wfh" = "v_CA11N_2179", "outside_ca" = "v_CA11N_2182",
              "no_fix" = "v_CA11N_2185", "usual" = "v_CA11N_2188")
place06v <- c("total" = "v_CA06_1076", "wfh" = "v_CA06_1081", "outside_ca" = "v_CA06_1082",
              "no_fix" = "v_CA06_1083", "usual" = "v_CA06_1077")
place01v <- c("total" = "v_CA06_1076", "wfh" = "v_CA06_1081", "outside_ca" = "v_CA06_1082",
              "no_fix" = "v_CA06_1083", "usual" = "v_CA06_1077")

#Function to grab the data from the census and to calculate new vectors to be used
place_grabber <- function(dyear, cvector, cyear){
  get_census(dataset = dyear, 
             regions = list(CSD = 2465005), 
             level = "CSD",
             vectors = cvector) |>
    mutate(Year = cyear, "Work from Home" = wfh / total,
           "Outside Canada" = outside_ca / total,
           "No Fixed Address" = no_fix / total,
           "Usual Place of Work" = usual / total) |> 
    select(Year, "Usual Place of Work", "Work from Home",
           "No Fixed Address", "Outside Canada")
}

#grabbing the census data
place21 <- place_grabber("CA21", place21v, "2021")
place16 <- place_grabber("CA16", place16v, "2016")
place11 <- place_grabber("CA11", place11v, "2011")
place06 <- place_grabber("CA06", place11v, "2006")
place01 <- get_census(dataset = "CA01", 
                      regions = list(CSD = 2465005), 
                      level = "CSD",
                      vectors = c("total" = "v_CA01_1236", "wfh_m" = "v_CA01_1242",
                                  "wfh_f" = "v_CA01_1250", "outside_ca_m" = "v_CA01_1243",
                                  "outside_ca_f" = "v_CA01_1251", "no_fix_m" = "v_CA01_1244",
                                  "no_fix_f" = "v_CA01_1252", "usual_m" = "v_CA01_1238",
                                  "usual_f" = "v_CA01_1246")) |> 
  mutate(Year = "2001", "Usual Place of Work" = (usual_m + usual_f) / total,
         "Work from Home" = (wfh_m + wfh_f) / total,
         "No Fixed Address" = (no_fix_m + no_fix_f) / total,
         "Outside Canada" = (outside_ca_m + outside_ca_f) / total) |> 
  select(Year, "Usual Place of Work", "Work from Home", "No Fixed Address", "Outside Canada")

#Prepping the data to be made into a graph
place <- bind_rows(place21, place16, place11, place06, place01) |> 
  pivot_longer(cols = -Year, names_to = "destination", values_to = "percentage") |> 
  mutate(destination = factor(destination, levels = c("Usual Place of Work", "Work from Home",
                                                      "No Fixed Address", "Outside Canada")))

place_fun <- function(dest, year) {
  place$percentage[place$Year == year & place$destination == dest] |> 
    convert_pct()
}

emploi_place_atwork_2001 <- place_fun("Usual Place of Work", 2001)
emploi_place_atwork_2016 <- place_fun("Usual Place of Work", 2016)
emploi_place_atwork <- place_fun("Usual Place of Work", 2021)

emploi_place_home_2016 <- place_fun("Work from Home", 2016)
emploi_place_home <- place_fun("Work from Home", 2021)


# 5.3.3 -------------------------------------------------------------------

library(httr)
library(sf)

fetch_all_pages <- function(base_url) {
  # Function to fetch data with pagination and error handling
  # Set initial query parameters
  query_params <- list(
    where = "1=1", # to get all the data; no filter
    outFields = "*", # to get all fields
    outSR = "4326", # output spatial reference; EPSG:4326 is WGS84 lat/long
    f = "geojson", # output format
    returnGeometry = "true" # to ensure geometry is included
  )
  
  all_data <- list()
  offset <- 0
  keep_fetching <- TRUE
  
  while (keep_fetching) {
    # Update the query parameters with the current offset and limit
    query_params$resultOffset <- offset
    query_params$resultRecordCount <- 1000
    
    # Make the GET request
    response <- httr::GET(url = base_url, query = query_params)
    
    # Check if the request was successful
    if (httr::status_code(response) != 200) {
      stop("Failed to retrieve data: ", httr::status_code(response))
    }
    
    # Inspect the content type to ensure it's GeoJSON
    content_type <- httr::headers(response)$`content-type`
    if (!grepl("application/geo", content_type, ignore.case = TRUE)) {
      break
    }
    
    # Read the content as geojson and convert to an sf object
    page_data <- tryCatch(
      sf::st_read(httr::content(response, "text"), quiet = TRUE),
      error = function(e) {
        message("Error reading data: ", e$message)
        return(NULL)
      }
    )
    
    # Check if data was successfully read
    if (is.null(page_data) || nrow(page_data) == 0) {
      keep_fetching <- FALSE
    } else {
      # Store the fetched data
      all_data[[length(all_data) + 1]] <- page_data
      # Increment the offset for the next page
      offset <- offset + 1000
    }
  }
  
  all_data <- Reduce(rbind, all_data)
  all_data <- sf::st_make_valid(all_data)

  all_data
}

# res_aines <- fetch_all_pages(
#   "https://public.arcgis.msss.rtss.qc.ca/arcgis/rest/services/Ressources/K10/MapServer/0/query"
# )
# 
# # Filter only Laval
# res_aines <- res_aines["nbPlaceRPA"]
# res_aines <- sf::st_transform(res_aines, crs = sf::st_crs(lvl))
# res_aines <- sf::st_filter(res_aines, lvl)
# 
# # Separate by electoral districts
# res_aines <- sf::st_intersection(res_aines, electoral_districts) |> 
#   group_by(NOM) |> 
#   summarize(places = sum(nbPlaceRPA, na.rm = TRUE)) |> 
#   sf::st_drop_geometry() |> 
#   cc.buildr::merge(electoral_districts["NOM"], by = "NOM", all = TRUE) |> 
#   mutate(places = ifelse(is.na(places), 0, places))
# 
# qs::qsave(res_aines, "data/placesRPA.qs")
res_aines <- qs::qread("data/placesRPA.qs")
res_aines <- sf::st_intersection(res_aines, electoral_districts["geometry"]) |> 
  st_collection_extract("POLYGON") |> 
  group_by(NOM, places) %>%
  summarise(geometry = st_union(geometry)) %>%
  ungroup() |> 
  sf::st_cast("MULTIPOLYGON")

breaks <- c(-Inf, 250, 500, 750, 1000, Inf)
res_aines$breaks <- cut(res_aines$places, 
                        breaks = breaks, 
                        labels = c("< 250", "250 - 500", "500 - 750", "750 - 1 000", "> 1 000"), 
                        include.lowest = TRUE)

map_5_3_3 <- ggplot(data = res_aines) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = breaks), color = "transparent") +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = NULL) +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

ggsave(plot = map_5_3_3, "outputs/5/map_5_3_3_res_aines.pdf", width = 7.5, height = 6)

# Table
persones_ages_census <- get_census(dataset = "CA21",
                            regions = list(CSD = 2465005),
                            level = "CSD",
                            vectors = c("v_CA21_290", "v_CA21_308", "v_CA21_326"))

persones_ages <- rowSums(persones_ages_census[c(11:13)], )
deficit <- convert_number_noround(persones_ages - sum(res_aines$places))
persones_ages_pct <- convert_pct(persones_ages / persones_ages_census$Population)
persones_ages <- convert_number_noround(persones_ages)


persones_ages_DA <- get_census(dataset = "CA21",
                               regions = list(CSD = 2465005),
                               level = "DA",
                               vectors = c(sf = "v_CA21_290", e = "v_CA21_308", ef = "v_CA21_326"),
                               geo_format = "sf")

persones_ages_DA$ages <- rowSums(persones_ages_DA[c("sf", "e", "ef")] |> sf::st_drop_geometry(),
                                 na.rm = TRUE)

res_aines <- 
  merge(res_aines, 
        interpolate(persones_ages_DA, additive_vars = "ages", 
                    round_additive = TRUE)[c("NOM", "ages")] |> 
          sf::st_drop_geometry(),
        by = "NOM")

res_aines_table <-
  res_aines |> 
  sf::st_drop_geometry() |> 
  select(NOM, places, ages) |> 
  arrange(-places) |> 
  gt() %>%
  cols_label(
    NOM = "District électoral",
    places = "Nombre de places en RPA",
    ages = "Individus âgés de 75 ans et +"
  ) |>
  fmt(columns = "places", fns = convert_number) |>
  # data_color(
  #   columns = "places",
  #   colors = scales::col_numeric(
  #     palette = c("white", color_theme("purpletransport")),
  #     domain = NULL
  #   )
  # ) |>
  # tab_style(
  #   style = cell_text(
  #     font = font_local_name
  #   ),
  #   locations = cells_body()
  # ) |> 
  tab_style(
    style = cell_text(
      font = font_local_name
    ),
    locations = cells_column_labels()
  ) |>
  tab_options(
    table.font.size = table_font_size,
    row_group.font.size = table_font_size,
    table.width = px(3 * 96)
  ) |>
  gt_split(row_every_n = ceiling(nrow(res_aines) / 2))

gtsave(grp_pull(res_aines_table, 1), "outputs/5/5_3_3_res_aines_1.png", zoom = 1)
gtsave(grp_pull(res_aines_table, 2), "outputs/5/5_3_3_res_aines_2.png", zoom = 1)
  

cor_aines_rpa <- round(cor(res_aines$places, res_aines$ages) * 1000) / 1000
cor_aines_rpa <- gsub("\\.", ",", cor_aines_rpa)

places_rpa <- convert_number_noround(sum(res_aines$places))


household_maintainer <-   get_census(dataset = "CA21", 
                                     regions = list(CSD = 2465005), 
                                     level = "CSD",
                                     vectors = c("75-85" = "v_CA21_4286",
                                                 "85+" = "v_CA21_4287",
                                                 "maintainers" = "v_CA21_4279"))
household_maintainer$`75+` <- household_maintainer$`75-85` + household_maintainer$`85+`
seventyfiveplus_households <- convert_number_noround(household_maintainer$`75+`)
household_maintainer$seventyfive_pct <- household_maintainer$`75+` / household_maintainer$maintainers
seventyfiveplus_households_pct <- convert_pct(household_maintainer$seventyfive_pct)


# 5.3.4 -------------------------------------------------------------------

# 5.3.5 -------------------------------------------------------------------

# 5.3.6 -------------------------------------------------------------------

# 5.3.7 -------------------------------------------------------------------

# 5.3.8 -------------------------------------------------------------------


# Save --------------------------------------------------------------------

qs::qsavem(emploi_outside_CSD_2016, emploi_outside_CSD, emploi_within_CSD_2016,
           emploi_within_CSD,
           emploi_place_atwork_2001, emploi_place_atwork_2016, emploi_place_atwork,
           emploi_place_home_2016, emploi_place_home, res_aines_table, map_5_3_3,
           persones_ages, cor_aines_rpa, places_rpa, seventyfiveplus_households,
           seventyfiveplus_households_pct, persones_ages_pct, deficit,
           file = "data/section_5_3.qsm")
