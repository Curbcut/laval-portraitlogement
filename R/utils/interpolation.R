## Aerial interpolation method

# CT <- cancensus::get_census(dataset = "CA21",
#                             regions = list(CSD = 2465005),
#                             level = "CT",
#                             geo_format = "sf")

interpolate <- function(from, to = sf::st_read("data/limite-district-electoral.geojson"), 
                        group = NULL, average_vars = c(), additive_vars = c(), 
                        round_additive = FALSE, crs = 32618) {
  
  # Cut using Laval
  to <- sf::st_transform(to, crs = 32618)
  to <- sf::st_intersection(to, lvl["geometry"]) |> 
    sf::st_collection_extract("POLYGON") |> 
    group_by(NOM) |> 
    summarise(geometry = st_union(geometry)) |> 
    ungroup() |> 
    suppressWarnings()
  
  # Add an identifier used for interpolation process
  to$ID <- paste0("to-", seq_along(to$geometry))
  
  if (!missing(group)) {
    
    group_class <- from[group] |> 
      st_drop_geometry() |> 
      map(class) |> 
      unlist()
    
    # Split variable
    split_var <- as.vector(st_drop_geometry(from[group]))
    
    # Interpolate using area
    from_list <- split(from, split_var)
    
    to_list <- 
      map(from_list, \(x) cc.buildr::interpolate_from_area(
        from = x, to = to, average_vars = average_vars, 
        additive_vars = additive_vars, round_additive = round_additive, 
        crs = crs))
    
    to <- to_list |> 
      map2(names(to_list), \(x, y) x |> 
             mutate(group_col = y, .after = NOM) |>
             mutate(group_col = if (group_class == "numeric") 
               as.numeric(group_col) else group_col) |> 
             rename_with(.fn = \(x) group, .cols = group_col)) |> 
      bind_rows()
    
  } else {
    to <- cc.buildr::interpolate_from_area(
      from = from, to = to, average_vars = average_vars, 
      additive_vars = additive_vars, round_additive = round_additive, 
      crs = crs)
  }
  
  return(to)
  
}
