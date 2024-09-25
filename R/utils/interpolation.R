## Aerial interpolation method

# CT <- cancensus::get_census(dataset = "CA21",
#                             regions = list(CSD = 2465005),
#                             level = "CT",
#                             geo_format = "sf")

interpolate <- function(from, to = sf::st_read("data/limite-district-electoral.geojson"), 
                        average_vars = c(), additive_vars = c(), round_additive = FALSE,
                        crs = 32618) {
  
  # Cut using Laval
  to <- sf::st_transform(to, crs = 32618)
  to <- sf::st_intersection(to, lvl["geometry"]) |> 
    st_collection_extract("POLYGON") |> 
    group_by(NOM) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  # Add an identifier used for interpolation process
  to$ID <- paste0("to-", seq_along(to$geometry))
  
  # Interpolate using area
  cc.buildr::interpolate_from_area(from = from, to = to, average_vars = average_vars, 
                                   additive_vars = additive_vars, round_additive = round_additive,
                                   crs = crs)
  
}