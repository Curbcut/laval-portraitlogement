source("R/utils/startup.R")

# 4.2.1 -------------------------------------------------------------------

# 4.2.2 -------------------------------------------------------------------
#Setting up a pre-pivoted table to make proportion calculations easier later
data_4_2_2_pivot <- read.csv("data/4/4_2_2.csv") |>
  select(total.total, total.owner, total.coowner, total.tenant, total.cotenant,
         total.sub, total.unsub) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  rename_with(~ gsub("total.", "low.", .)) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "values")

#Grabbing data for the plot
data_4_2_2 <- read.csv("data/4/4_2_2.csv") |> #Edited version of the spreadsheet
  select(-GeoUID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  mutate(diff.total = total.total - low.total,
         diff.owner = total.owner - low.owner,
         diff.coowner = total.coowner - low.coowner,
         diff.tenant = total.tenant - low.cotenant,
         diff.cotenant = total.cotenant - low.cotenant,
         diff.sub = total.sub - low.sub,
         diff.unsub = total.unsub - low.unsub) |> 
  select(!starts_with("total")) |> 
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "value") |> 
  left_join(data_4_2_2_pivot, by = "variable") |> 
  mutate(prop = if_else(!is.na(values),
                        convert_pct(value / values),
                        NA_character_),
         hh = case_when(
           str_detect(variable, "total") ~ "Tous les ménages",
           str_detect(variable, "owner") & !str_detect(variable, "coowner") ~ "Propriétaire",
           str_detect(variable, "coowner") ~ "Propriétaire d’une copropriété",
           str_detect(variable, "tenant") & !str_detect(variable, "cotenant") ~ "Locataire",
           str_detect(variable, "cotenant") ~ "Locataire en copropriété",
           str_detect(variable, "sub") & !str_detect(variable, "unsub") ~ "Logement subventionné",
           str_detect(variable, "unsub") ~ "Logement non subventionné",
           TRUE ~ "Total"
         ),
         income = case_when(
           str_detect(variable, "low") ~ "Ménages à faible revenu",
           str_detect(variable, "diff") ~ "Tous les ménages",
           TRUE ~ "Other"
         )) |> 
  select(-values, -variable) |> 
  mutate(hh = factor(hh, levels = c(
    "Tous les ménages", 
    "Propriétaire", 
    "Propriétaire d’une copropriété", 
    "Locataire", 
    "Locataire en copropriété", 
    "Logement subventionné", 
    "Logement non subventionné")),
    income = factor(income, levels = c(
      "Tous les ménages", "Ménages à faible revenu"
    )))

#Plotting the bar graph
plot_4_2_2 <- ggplot(data_4_2_2, aes(x = hh, y = value, fill = income)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = prop), vjust = -0.5,
                                         size = 4, color = "black") +
  scale_fill_manual(values = c("Ménages à faible revenu" = "#A3B0D1", "Tous les ménages" = "#CD718C")) +
  labs(x = "", y = "Nombre de ménages (n)", fill = "Income Type") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

#Data for the map
data_4_2_2_map <- read.csv("data/4/4_2_2.csv") |> 
  as.data.frame() |> 
  mutate(GeoUID = sprintf("%.2f", as.numeric(GeoUID))) |>  # Ensure two decimal places
  mutate(GeoUID = as.character(GeoUID))

data_4_2_2_sf <- full_join(laval_ct, data_4_2_2_map, by = "GeoUID") |> 
  mutate(prop = low.total / total.total) |> 
  select(prop)

#Finding the appropriate breaks
#list(classInt::classIntervals(data_4_2_2_sf$prop, n = 5, style = "pretty")$brks)
`4_2_2_breaks` <- c(-Inf, 0.08, 0.12, 0.17, 0.25, Inf)
`4_2_2_breaks_labels` <- c("< 8,0 %", "8,0 - 12,0 %", "12,0 - 17,0 %", "17,0 - 25,0 %", "> 25,0 %")

#Pasting the breaks back into the data frame
data_4_2_2_sf <- data_4_2_2_sf |> 
  mutate("quantile" = cut(`prop`, breaks = `4_2_2_breaks`, include.lowest = TRUE,
                                labels = `4_2_2_breaks_labels`))

map_4_2_2 <- ggplot(data = data_4_2_2_sf) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = `quantile`), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "Proportion de ménages à faible revenu (%)") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Saving the visuals as images
ggsave("outputs/4/plot_4_2_2.png", plot = plot_4_2_2, width = 600/72, height = 800/72, dpi = 72)
ggsave("outputs/4/map_4_2_2.png", plot = map_4_2_2, width = 600/72, height = 800/72, dpi = 72)
# 4.2.3 -------------------------------------------------------------------


# 4.2.4 -------------------------------------------------------------------


# 4.2.5 -------------------------------------------------------------------


# 4.2.6 -------------------------------------------------------------------


# 4.2.7 -------------------------------------------------------------------


# 4.2.8 -------------------------------------------------------------------


# 4.2.9 -------------------------------------------------------------------


# 4.2.10 ------------------------------------------------------------------


# 4.2.11 ------------------------------------------------------------------

# R Markdown --------------------------------------------------------------
qs::qsavem(plot_4_2_2, map_4_2_2,
           file = "data/section_4_2.qsm")