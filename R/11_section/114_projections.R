#https://statistique.quebec.ca/fr/document/projections-de-menages-regions-administratives-et-regions-metropolitaines-rmr

proj <- read_excel("data/4/Ménages_Total_RA_base_2024.xlsx", 
                                 skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |>
  select(4:34) |> 
  mutate(`2021` = as.numeric(`2021`)) |> 
  pivot_longer(cols = `2021`:`2051`,
               names_to = "year") |> 
  mutate(yoy_change = value / lag(value) - 1)

v <- \(year) {
  proj$value[proj$year == year]
}

growth <- (v(2051) - v(2024)) / v(2024)
v(2051)

proj$yoy_change[proj$year %in% c(2024:2051)] |> mean()

(v(2051) / v(2024))^(1 / (2051-2024)) - 1


{v(2024) * 1.003644267 ^ (2051-2024)} - v(2051)
