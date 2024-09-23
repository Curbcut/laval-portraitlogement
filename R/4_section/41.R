source("R/utils/startup.R")

# 4.1.1.1 Répartition ménages selon mode d'occupation ---------------------

#Vectors for tenure type
pto_21v <- c("total" = "v_CA21_4288", "owner" = "v_CA21_4305", "tenant" = "v_CA21_4313")
pto_16v <- c("total" = "v_CA16_4886", "owner" = "v_CA16_4890", "tenant" = "v_CA16_4897")
pto_11v <- c("total" = "v_CA11N_2277", "owner" = "v_CA11N_2281", "tenant" = "v_CA11N_2288")
pto_06v <- c("total" = "v_CA06_2048", "owner" = "v_CA06_2053", "tenant" = "v_CA06_2049")
pto_01v <- c("owner" = "v_CA01_1670", "tenant" = "v_CA01_1666")

#Census Grabbing Function for Laval
pto_census <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = pto_year
  ) |> 
    mutate(Year = cyear) |> 
    select(-Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households, -CD_UID, -CMA_UID, -PR_UID, -GeoUID)
}

#Grabbing data for 2001-2021 for Laval
pto_21 <- pto_census("CA21", pto_21v, "2021")
pto_16 <- pto_census("CA16", pto_16v, "2016")
pto_11 <- pto_census("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06 <- pto_census("CA06", pto_06v, "2006")
pto_01 <- pto_census("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)

#Binding the data for the graphs
pto <- bind_rows(pto_21, pto_16, pto_11, pto_06, pto_01) |>
  pivot_longer(cols = c("owner", "tenant"), 
               names_to = "type", 
               values_to = "count") |>
  group_by(type) |>
  arrange(Year) |>
  mutate(
    count_cc = convert_number(count),
    prop = count / total,
    prop_cc = paste0(
      convert_pct(prop),
      ifelse(is.na(lag(prop)), "", 
             ifelse(prop > lag(prop), "▲", "▼"))
    )
  ) |>
  ungroup()

#Creating a grouped bar chart
plot_4_1_1_1 <- ggplot(pto, aes(x = Year, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = prop_cc), position = position_dodge(width = 0.9),
            vjust = 2, size = 4, color = "white") +
  scale_fill_manual(values = c("owner" = "#A3B0D1", "tenant" = "#CD718C"),
                    labels = c("owner" = "Propriétaire", "tenant" = "Locataire")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages") +
  graph_theme

#Grabbing the data to create a map
pto_map <- get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "DA",
             vectors = c("total" = "v_CA21_4288", "owner" = "v_CA21_4305",
                         "tenant" = "v_CA21_4313"),
             geo_format = "sf") |> 
    select(total, owner, tenant) |> 
  mutate(owner = if_else(is.na(owner), 0, owner),
         tenant = if_else(is.na(tenant), 0, tenant)) |> 
  mutate(total = if_else(is.na(total), owner + tenant, total))

#Finding and creating the breaks
#list(classInt::classIntervals(pto_map$total, n = 5, style = "jenks")$brks)
pto_total_breaks <- c(-Inf, 230, 415, 715, 1315, Inf)
pto_total_breaks_labels <- c("< 230", "230 - 415", "415 - 715", "715 - 1 315", "> 1 315")
#list(classInt::classIntervals(pto_map$owner, n = 5, style = "jenks")$brks)
pto_owner_breaks <- c(-Inf, 115, 210, 365, 660, Inf)
pto_owner_breaks_labels <- c("< 115", "115 - 210", "210 - 365", "365 - 660", "> 660")
#list(classInt::classIntervals(pto_map$tenant, n = 5, style = "jenks")$brks)
pto_tenant_breaks <- c(-Inf, 55, 165, 365, 715, Inf)
pto_tenant_breaks_labels <- c("< 55", "55 - 165", "165 - 365", "365 - 715", "> 715")

#Applying the breaks as new columns to the data frame
pto_map <- pto_map |> 
  mutate(
    total_quantile = cut(total, breaks = pto_total_breaks, include.lowest = TRUE,
                         labels = pto_total_breaks_labels),
    owner_quantile = cut(total, breaks = pto_owner_breaks, include.lowest = TRUE,
                         labels = pto_owner_breaks_labels),
    tenant_quantile = cut(total, breaks = pto_tenant_breaks, include.lowest = TRUE,
                         labels = pto_tenant_breaks_labels)
  )

#Plotting total number of households
map_4_1_1_1_total <- ggplot(data = pto_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = total_quantile), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "Nombre de ménages (n)") +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Plotting total number of owner households
map_4_1_1_1_owner <- ggplot(data = pto_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = owner_quantile), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "Nombre de ménages propriétaires (n)") +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Plotting total number of tenant households
map_4_1_1_1_tenant <- ggplot(data = pto_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = tenant_quantile), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "Nombre de ménages locataires (n)") +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Saving the files as photos
ggsave("outputs/4/plot_4_1_1_1.png", plot = plot_4_1_1_1, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/map_4_1_1_1_total.png", plot = map_4_1_1_1_total, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/map_4_1_1_1_owner.png", plot = map_4_1_1_1_owner, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/map_4_1_1_1_tenant.png", plot = map_4_1_1_1_tenant, width = 800/72, height = 600/72, dpi = 72)

# 4.1.1.2 Catégorie de revenu et mode d'occupation ------------------------
revenu <- c("none"= "Sans revenu total", "10" = "  Supérieur à zéro, moins de 10 000 $",
            "2" = "10 000 $ à 19 999 $", "40" = "20 000 $ à 39 999 $",
            "60" = "40 000 $ à 59 999 $", "80" = "60 000 $ à 79 999 $",
            "100" = "80 000 $ à 99 999 $", "124" = "100 000 $ à 124 999 $",
            "125" = "125 000 $ et plus")
mode_occupation <- c(owner = "Propriétaire", tenant = "Locataire")

data_4_1_1_2 <- crosstab_get(mode_occupation = mode_occupation, revenu = revenu) |> 
  mutate(owner_20 = owner_none + owner_10 + owner_2,
         tenant_20 = tenant_none + tenant_10 + tenant_2) |> 
  select(-owner_none, -tenant_none, -owner_10, -tenant_10, -owner_2, -tenant_2, -DA_ID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "count") |> 
  mutate(income = case_when(
        str_detect(type, "20") ~ "< 19 999 $",
        str_detect(type, "40") ~ "20 - 39 999 $",
        str_detect(type, "60") ~ "40 - 59 999 $",
        str_detect(type, "80") ~ "60 - 79 999 $",
        str_detect(type, "100") ~ "80 - 99 999 $",
        str_detect(type, "124") ~ "100 - 124 999 $",
        str_detect(type, "125") ~ "> 125 000 $",
        TRUE ~ NA_character_),
        type = case_when(
              str_detect(type, "owner") ~ "Propriétaire",
              str_detect(type, "tenant") ~ "Locataire",
              TRUE ~ type
              )) |> 
  mutate(type = factor(type, levels = c(
        "Propriétaire",
        "Locataire")),
        income = factor(income, levels = c(
          "< 19 999 $",
          "20 - 39 999 $",
          "40 - 59 999 $",
          "60 - 79 999 $",
          "80 - 99 999 $",
          "100 - 124 999 $",
          "> 125 000 $")))

plot_4_1_1_2 <- ggplot(data_4_1_1_2, aes(x = income, y = count, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(x = "Revenu médian des ménages", y = "Nombre de ménages (n)", title = "") +
    scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
    scale_y_continuous(labels = function(x) convert_number(x)) +
    graph_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

map_data_4_1_1_2_pre <- crosstab_get(mode_occupation = mode_occupation, revenu = c(medrev = "Revenu total médian ($)")) |> 
  rename("GeoUID" = "DA_ID")

map_data_4_1_1_2 <- full_join(laval_da, map_data_4_1_1_2_pre, by = "GeoUID") |> 
  drop_na()

#list(classInt::classIntervals(map_data_4_1_1_2$owner_medrev, n = 5, style = "fisher")$brks)
`4_1_1_2_owner_breaks` <- c(-Inf, 21000, 83000, 107500, 137500, Inf)
`4_1_1_2_owner_breaks_labels` <- c("< 21000", "21000 - 83000", "83000 - 107500", "107500 - 137500", "> 137500")
#list(classInt::classIntervals(map_data_4_1_1_2$tenant_medrev, n = 5, style = "fisher")$brks)
`4_1_1_2_tenant_breaks` <- c(-Inf, 11000, 43000, 56000, 70000, Inf)
`4_1_1_2_tenant_breaks_labels` <- c("< 11000", "11000 - 43000", "43000 - 56000", "56000 - 70000", "> 70000")

map_data_4_1_1_2 <- map_data_4_1_1_2 |> 
  mutate("owner_quant" = cut(owner_medrev, breaks = `4_1_1_2_owner_breaks`, include.lowest = TRUE, labels = `4_1_1_2_owner_breaks_labels`),
         "tenant_quant" = cut(tenant_medrev, breaks = `4_1_1_2_tenant_breaks`, include.lowest = TRUE, labels = `4_1_1_2_tenant_breaks_labels`))

map_4_1_1_2_owner <- ggplot(data = map_data_4_1_1_2) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = `owner_quant`), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                      name = "Revenu médian des ménages propriétaires ($)") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

map_4_1_1_2_tenant <- ggplot(data = map_data_4_1_1_2) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = `tenant_quant`), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "Revenu médian des ménages locataires ($)") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

#Saving the visuals as images
ggsave("outputs/4/plot_4_1_1_2.png", plot = plot_4_1_1_2, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/map_4_1_1_2_owner.png", plot = map_4_1_1_2_owner, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/map_4_1_1_2_tenant.png", plot = map_4_1_1_2_tenant, width = 800/72, height = 600/72, dpi = 72)

#Data frame for the plot
# data_4_1_2 <- read_excel("data/4/mode_occupation_revenu_SR.xlsx") |> #Edited version of the spreadsheet
#   select(-GeoUID) |> 
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
#   pivot_longer(cols = everything(), names_to = "type", values_to = "households") |> 
#   mutate(income = case_when(
#     str_detect(type, "19999") ~ "< 19 999 $",
#     str_detect(type, "39999") ~ "20 - 39 999 $",
#     str_detect(type, "59999") ~ "40 - 59 999 $",
#     str_detect(type, "79999") ~ "60 - 79 999 $",
#     str_detect(type, "99999") ~ "80 - 99 999 $",
#     str_detect(type, "124999") ~ "100 - 124 999 $",
#     str_detect(type, "125000") ~ "> 125 000 $",
#     TRUE ~ NA_character_)) |> 
#   mutate(type = case_when(
#     str_detect(type, "Owner") & !str_detect(type, "Condo Owner") ~ "Propriétaire",
#     str_detect(type, "Tenant") & !str_detect(type, "Condo Tenant") ~ "Locataire",
#     TRUE ~ type
#     )) |>
#   filter(type %in% c("Propriétaire", "Locataire")) |> 
#   mutate(type = factor(type, levels = c(
#     "Propriétaire", 
#     "Locataire")),
#     income = factor(income, levels = c(
#       "< 19 999 $", 
#       "20 - 39 999 $", 
#       "40 - 59 999 $", 
#       "60 - 79 999 $", 
#       "80 - 99 999 $", 
#       "100 - 124 999 $", 
#       "> 125 000 $")))
# 
# #Plotting the data
# plot_4_1_2 <- ggplot(data_4_1_2, aes(x = income, y = households, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "", y = "Nombre de ménages (n)", title = "") +
#   scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
#   scale_y_continuous(labels = function(x) convert_number(x)) +
#   graph_theme +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 
# #Data for the maps
# data_4_1_2_map <- read_excel("data/4/mode_occupation_revenu_SR.xlsx") |> 
#   as.data.frame() |> 
#   mutate(GeoUID = sprintf("%.2f", as.numeric(GeoUID))) |>  # Ensure two decimal places
#   mutate(GeoUID = as.character(GeoUID))
# 
# data_4_1_2_sf <- full_join(laval_ct, data_4_1_2_map, by = "GeoUID")
# 
# #Finding and creating the breaks
# data_4_1_2_breaks <- data_4_1_2_sf |> 
#   select(contains("total")) |> 
#   st_drop_geometry() |> 
#   pivot_longer(cols = everything(),
#                names_to = "variable", 
#                values_to = "value")
# 
# #list(classInt::classIntervals(data_4_1_2_breaks$value, n = 5, style = "jenks")$brks)
# `4_1_2_total_breaks` <- c(-Inf, 115, 250, 425, 725, Inf)
# `4_1_2_total_breaks_labels` <- c("< 115", "115 - 250", "250 - 425", "425 - 725", "> 725")
# 
# #Adding the breaks back into the joined file
# data_4_1_2_sf <- data_4_1_2_sf |> 
#   mutate("quantile_19999" = cut(`Total 19999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_39999" = cut(`Total 39999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_59999" = cut(`Total 59999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_79999" = cut(`Total 79999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_99999" = cut(`Total 99999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_124999" = cut(`Total 124999`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "quantile_125000" = cut(`Total 125000`, breaks = `4_1_2_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`))
# 
# #Mapping the data
# map_4_1_2_19999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_19999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu inférieur à 19 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_39999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_39999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu entre 20 000 $ et 39 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_59999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_59999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu entre 40 000 $ et 59 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_79999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_79999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu entre 60 000 $ et 79 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_99999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_99999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu entre 80 000 $ et 99 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_124999 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_124999`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu entre 100 000 $ et 124 999 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_2_125000 <- ggplot(data = data_4_1_2_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `quantile_125000`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (revenu supérieur à 125 000 $) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
#   
# #Saving the visuals as images
# ggsave("outputs/4/plot_4_1_2.png", plot = plot_4_1_2, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_19999.png", plot = map_4_1_2_19999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_39999.png", plot = map_4_1_2_39999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_59999.png", plot = map_4_1_2_59999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_79999.png", plot = map_4_1_2_79999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_99999.png", plot = map_4_1_2_99999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_124999.png", plot = map_4_1_2_124999, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_2_125000.png", plot = map_4_1_2_125000, width = 800/72, height = 600/72, dpi = 72)

# 4.1.1.3 Type de ménage (taille et composition) selon mode d'occupation --------
composition <- c("wo_kids"= "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple sans enfants",
                 "w_kids" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple avec enfants",
                 "mono" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : famille monoparentale",
                 "multi" = "Ménage multigénérationnel",
                 "solo" = "Ménage composé d'une seule personne",
                 "other" = "  Ménage sans famille de recensement, composé de deux personnes ou plus")

data_4_1_1_3 <- crosstab_get(mode_occupation = mode_occupation, composition = composition) |> 
  select(-DA_ID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "count") |> 
  mutate(comp = case_when(
    str_detect(type, "wo_kids") ~ "Couple sans enfants*",
    str_detect(type, "w_kids") ~ "Couple avec enfants*",
    str_detect(type, "mono") ~ "Famille monoparentale*",
    str_detect(type, "multi") ~ "Ménage multigénérationnel",
    str_detect(type, "solo") ~ "Seule personne",
    str_detect(type, "other") ~ "Deux personnes ou plus",
    TRUE ~ NA_character_),
    type = case_when(
      str_detect(type, "owner") ~ "Propriétaire",
      str_detect(type, "tenant") ~ "Locataire",
      TRUE ~ type
    )) |> 
  mutate(type = factor(type, levels = c(
    "Propriétaire",
    "Locataire")),
    comp = factor(comp, levels = c(
      "Couple sans enfants*",
      "Couple avec enfants*",
      "Famille monoparentale*",
      "Ménage multigénérationnel",
      "Seule personne",
      "Deux personnes ou plus")))

plot_4_1_1_3 <- ggplot(data_4_1_1_3, aes(x = comp, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Nombre de ménages (n)", title = "", caption = "*Ménage comptant une seule famille de recensement, sans personnes additionnelles") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

table_data_4_1_1_3 <- crosstab_get(mode_occupation = mode_occupation, composition = composition) |> 
  select(-DA_ID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  rename_with(~ gsub("^owner_", "Propriétaire_", .x), starts_with("owner_")) |>  # Rename owner_ to Propriétaire_
  rename_with(~ gsub("^tenant_", "Locataire_", .x), starts_with("tenant_")) |> 
  pivot_longer(
    cols = starts_with("Propriétaire_") | starts_with("Locataire_"),
    names_to = c("Type de ménage", ".value"),
    names_pattern = "(Propriétaire|Locataire)_(.*)") |> 
  mutate(total = wo_kids + w_kids + mono + multi + solo + other) |> 
  mutate("Couple sans enfants (n)" = wo_kids,
         "Couple sans enfants (%)" = wo_kids / total,
         "Couple avec enfants (n)" = w_kids,
         "Couple avec enfants (%)" = w_kids / total,
         "Famille monoparentale (n)" = mono,
         "Famille monoparentale (%)" = mono / total,
         "Ménage multigénérationnel (n)" = multi,
         "Ménage multigénérationnel (%)" = multi / total,
         "Seule personne (n)" = solo,
         "Seule personne (%)" = solo / total,
         "Deux personnes ou plus (n)" = other,
         "Deux personnes ou plus (%)" = other / total) |> 
  select(-total, -wo_kids, -w_kids, -mono, -multi, -solo, -other)

table_4_1_1_3 <- table_data_4_1_1_3 |> 
  gt() |>
  cols_label(
    `Couple sans enfants (n)` = html("<div style='width:95px;'>Couple sans enfants (n)</div>"),
    `Couple sans enfants (%)` = html("<div style='width:95px;'>Couple sans enfants (%)</div>"),
    `Couple avec enfants (n)` = html("<div style='width:95px;'>Couple avec enfants (n)</div>"),
    `Couple avec enfants (%)` = html("<div style='width:95px;'>Couple avec enfants (%)</div>"),
    `Deux personnes ou plus (n)` = html("<div style='width:100px;'>Deux personnes ou plus (n)</div>"),
    `Deux personnes ou plus (%)` = html("<div style='width:100px;'>Deux personnes ou plus (%)</div>")
  ) |> 
  data_color(
    columns = c(3,5,7,9,11,13),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = c(2,4,6,8,10,12), fns = convert_number) |> 
  fmt(columns = c(3,5,7,9,11,13), fns = convert_pct) |> 
  tab_style(
    style = cell_text(font = "KMR Apparat Regular", size = px(15)),
    locations = cells_body()) |> 
  tab_style(
    style = cell_text(font = "KMR Apparat Regular"),
    locations = cells_column_labels())
    

# #Grabbing the data for the plot
# data_4_1_3 <- read_excel("data/4/mode_occupation_composition_SR.xlsx") |> #Edited version of the spreadsheet
#   select(-GeoUID) |> 
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
#   pivot_longer(cols = everything(), names_to = "type", values_to = "households") |> 
#   mutate(composition = case_when(
#     str_detect(type, "wo child") ~ "Couple sans enfants*",
#     str_detect(type, "child") ~ "Couple avec enfants*",
#     str_detect(type, "single parent") ~ "Famille monoparentale*",
#     str_detect(type, "multigen") ~ "Ménage multigénérationnel",
#     str_detect(type, "other") ~ "Autres ménages comptant une famille de recensement",
#     str_detect(type, "sole") ~ "Ménage composé d'une seule personne",
#     str_detect(type, "non cf") ~ "Ménage composé de deux personnes ou plus",
#     TRUE ~ NA_character_)) |> 
#   mutate(type = case_when(
#     str_detect(type, "Owner") & !str_detect(type, "Condo Owner") ~ "Propriétaire",
#     str_detect(type, "Tenant") & !str_detect(type, "Condo Tenant") ~ "Locataire",
#     TRUE ~ type
#   )) |>
#   filter(type %in% c("Propriétaire", "Locataire")) |>  
#   mutate(type = factor(type, levels = c(
#     "Propriétaire", 
#     "Locataire")),
#     composition = factor(composition, levels = c(
#       "Couple avec enfants*", 
#       "Couple sans enfants*", 
#       "Famille monoparentale*", 
#       "Ménage multigénérationnel", 
#       "Autres ménages comptant une famille de recensement", 
#       "Ménage composé d'une seule personne", 
#       "Ménage composé de deux personnes ou plus")))
# 
# #Plotting the plot
# plot_4_1_3 <- ggplot(data_4_1_3, aes(x = composition, y = households, fill = type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(x = "", y = "Nombre de ménages (n)", title = "", caption = "*Ménage comptant une seule famille de recensement, sans personnes additionnelles") +
#   scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
#   scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
#   scale_y_continuous(labels = function(x) convert_number(x)) +
#   graph_theme +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         plot.caption = element_text(hjust = 0))
# 
# #Data for the map
# data_4_1_3_map <- read_excel("data/4/mode_occupation_composition_SR.xlsx") |> 
#   as.data.frame() |> 
#   mutate(GeoUID = sprintf("%.2f", as.numeric(GeoUID))) |>  # Ensure two decimal places
#   mutate(GeoUID = as.character(GeoUID))
# 
# data_4_1_3_sf <- full_join(laval_ct, data_4_1_3_map, by = "GeoUID")
# 
# #Finding and creating the breaks
# data_4_1_3_breaks <- data_4_1_3_sf |> 
#   select(contains("total")) |> 
#   st_drop_geometry() |> 
#   pivot_longer(cols = everything(),
#                names_to = "variable", 
#                values_to = "value")
# 
# #list(classInt::classIntervals(data_4_1_3_breaks$value, n = 5, style = "quantile")$brks)
# `4_1_3_total_breaks` <- c(-Inf, 50, 100, 300, 600, Inf)
# `4_1_3_total_breaks_labels` <- c("< 50", "50 - 100", "100 - 300", "300 - 600", "> 600")
# 
# #Adding the breaks back into the joined file
# data_4_1_3_sf <- data_4_1_3_sf |> 
#   mutate("wo_child" = cut(`Total - wo child`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_2_total_breaks_labels`),
#          "child" = cut(`Total - child`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_3_total_breaks_labels`),
#          "single" = cut(`Total - single parent`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_3_total_breaks_labels`),
#          "multigen" = cut(`Total - multigen`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_3_total_breaks_labels`),
#          "other" = cut(`Total - other`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                 labels = `4_1_3_total_breaks_labels`),
#          "sole" = cut(`Total - sole`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                  labels = `4_1_3_total_breaks_labels`),
#          "non_cf" = cut(`Total - non cf`, breaks = `4_1_3_total_breaks`, include.lowest = TRUE,
#                                  labels = `4_1_3_total_breaks_labels`))
# 
# #Graphing the maps
# map_4_1_3_wochild <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `wo_child`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (couple sans enfants) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_child <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `child`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (couple avec enfants) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_single <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `single`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (famille monoparentale) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_multigen <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `multigen`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (multigénérationnel) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_other <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `other`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (autres familles de recensement) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_sole <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `sole`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (une seule personne) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_1_3_noncf <- ggplot(data = data_4_1_3_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `non_cf`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Nombre de ménages (sans famille de recensement, composé de deux personnes ou plus) (n)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# #Saving visuals as images
# ggsave("outputs/4/plot_4_1_3.png", plot = plot_4_1_3, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_wochild.png", plot = map_4_1_3_wochild, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_child.png", plot = map_4_1_3_child, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_single.png", plot = map_4_1_3_single, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_multigen.png", plot = map_4_1_3_multigen, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_other.png", plot = map_4_1_3_other, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_sole.png", plot = map_4_1_3_sole, width = 800/72, height = 600/72, dpi = 72)
# ggsave("outputs/4/map_4_1_3_noncf.png", plot = map_4_1_3_noncf, width = 800/72, height = 600/72, dpi = 72)

# 4.1.1.4 Statut d'immigrant selon mode d'occupation ----------------------
#Percentages based on provided PDF and 2016 census data
imm_2016 <- get_census(dataset = datayear,
                       regions = list(CSD = 2465005),
                       level = "CSD",
                       vectors = pto_year)

# 4.1.5 -------------------------------------------------------------------

# 4.1.6 -------------------------------------------------------------------

# 4.1.7 -------------------------------------------------------------------
data_4_1_7 <- read_excel("data/4/4_1_7.xlsx") |> 
  filter(RA == 13) |> 
  mutate(period = factor(ifelse(Year < 2024, "Avant 2024", "Après 2024"),
                         levels = c("Avant 2024", "Après 2024")))

plot_4_1_7 <- ggplot(data_4_1_7, aes(x = Year, y = Total, linetype = period)) +
  geom_line(color = "black", linewidth = 1.75) +
  scale_linetype_manual(values = c("Avant 2024" = "solid", "Après 2024" = "dotted"),
                        labels = c("Avant 2024", "Après 2024")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages (n)", linetype = "Période") +
  graph_theme

ggsave("outputs/4/plot_4_1_7.png", plot = plot_4_1_7, width = 800/72, height = 600/72, dpi = 72)
# 4.1.8 -------------------------------------------------------------------

# 4.1.9 -------------------------------------------------------------------
data_4_1_9 <- read_excel("data/4/4_1_9.xlsx") |> 
  filter(RA == 13 & Sexe == 3) |> 
  mutate("0-24" = `0-19` + `20-24`,
         "25-64" = `20-64` - `20-24`)

data_4_1_9_2 <- data_4_1_7 |> 
  mutate(`15-24` = `15-19` + `20-24`,
         `25-64` = `25-29` + `30-34` + `35-39` + `40-44` + `45-49` +
                   `50-54`+ `55-59` + `60-64`,
         `65+` = `65-69` + `70-74` + `75-79` + `80-84` + `85+`)

plot_4_1_9_pop <- ggplot(data_4_1_9, aes(x = Year)) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `0-24`, color = "0-24", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `0-24`, color = "0-24", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `25-64`, color = "25-64", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `25-64`, color = "25-64", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `65+`, color = "65+", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `65+`, color = "65+", linetype = "After 2024"), linewidth = 1.75) +
  scale_color_manual(values = c("0-24" = "#A3B0D1", "25-64" = "#CD718C", "65+" = "#73AD80"),
                     labels = c("0-24", "25-64", "65+")) +
  scale_linetype_manual(values = c("Before 2024" = "solid", "After 2024" = "dotted"),
                        labels = c("Avant 2024", "Après 2024")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Population (n)", color = "Age Group", linetype = "Period") +
  graph_theme

plot_4_1_9_hh <- ggplot(data_4_1_9_2, aes(x = Year)) +
  geom_line(data = subset(data_4_1_9_2, Year < 2024), aes(y = `15-24`, color = "15-24", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9_2, Year >= 2024), aes(y = `15-24`, color = "15-24", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9_2, Year < 2024), aes(y = `25-64`, color = "25-64", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9_2, Year >= 2024), aes(y = `25-64`, color = "25-64", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9_2, Year < 2024), aes(y = `65+`, color = "65+", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9_2, Year >= 2024), aes(y = `65+`, color = "65+", linetype = "After 2024"), linewidth = 1.75) +
  scale_color_manual(values = c("15-24" = "#A3B0D1", "25-64" = "#CD718C", "65+" = "#73AD80"),
                     labels = c("15-24", "25-64", "65+")) +
  scale_linetype_manual(values = c("Before 2024" = "solid", "After 2024" = "dotted"),
                        labels = c("Avant 2024", "Après 2024")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages (n)", color = "Age Group", linetype = "Period") +
  graph_theme

ggsave("outputs/4/plot_4_1_9_pop.png", plot = plot_4_1_7, width = 800/72, height = 600/72, dpi = 72)
ggsave("outputs/4/plot_4_1_9_hh.png", plot = plot_4_1_7, width = 800/72, height = 600/72, dpi = 72)

# R Markdown --------------------------------------------------------------
qs::qsavem(plot_4_1_1_1, map_4_1_1_1_total, map_4_1_1_1_owner, map_4_1_1_1_tenant,
           plot_4_1_1_2, map_4_1_1_2_owner, map_4_1_1_2_tenant,
           plot_4_1_1_3, table_4_1_1_3,
           map_4_1_3_other, map_4_1_3_sole, map_4_1_3_noncf,
           plot_4_1_9_hh, plot_4_1_9_pop,
           file = "data/section_4_1.qsm")
