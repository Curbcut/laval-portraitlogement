source("R/utils/startup.R")

# 4.2.1 -------------------------------------------------------------------

composition <- c("wo_kids"= "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple sans enfants",
                 "w_kids" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple avec enfants",
                 "mono" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : famille monoparentale",
                 "multi" = "Ménage multigénérationnel",
                 "solo" = "Ménage composé d'une seule personne",
                 "other" = "  Ménage sans famille de recensement, composé de deux personnes ou plus")

occ_rev_comp <- crosstab_get(mode_occupation = c("Propriétaire" = "Propriétaire",
                                 "Locataire" = "Locataire"),
             revenu = c("rev_med" = "Revenu total médian ($)"),
             composition = composition,
             scale = "CSD") |> 
  pivot_longer(cols = starts_with("Propriétaire") | starts_with("Locataire"),
               names_to = c("mode_occupation", "composition"),
               names_pattern = "(Propriétaire|Locataire)_(.*)_rev_med",
               values_to = "median_revenue") |> 
  mutate(comp_pretty = case_when(
    str_detect(composition, "wo_kids") ~ "Couple sans enfants",
    str_detect(composition, "w_kids") ~ "Couple avec enfants",
    str_detect(composition, "mono") ~ "Famille monoparentale",
    str_detect(composition, "multi") ~ "Ménage multigénérationnel",
    str_detect(composition, "solo") ~ "Personne seule",
    str_detect(composition, "other") ~ "Deux personnes ou plus",
    TRUE ~ NA_character_))


occ_rev_comp <- merge(
  occ_rev_comp,
  crosstab_get(mode_occupation = c("Propriétaire" = "Propriétaire",
                                   "Locataire" = "Locataire"),
               composition = composition,
               scale = "CSD") |> 
    pivot_longer(cols = starts_with("Propriétaire") | starts_with("Locataire"),
                 names_to = c("mode_occupation", "composition"),
                 names_pattern = "(Propriétaire|Locataire)_(.*)",
                 values_to = "nb") |> 
    mutate(comp_pretty = case_when(
      str_detect(composition, "wo_kids") ~ "Couple sans enfants",
      str_detect(composition, "w_kids") ~ "Couple avec enfants",
      str_detect(composition, "mono") ~ "Famille monoparentale",
      str_detect(composition, "multi") ~ "Ménage multigénérationnel",
      str_detect(composition, "solo") ~ "Personne seule",
      str_detect(composition, "other") ~ "Deux personnes ou plus",
      TRUE ~ NA_character_)),
  by = c("CSD_ID", "mode_occupation", "composition", "comp_pretty")) %>%
  group_by(mode_occupation) %>%
  mutate(alpha = nb / sum(nb)) %>%
  mutate(alpha_custom = scales::rescale(alpha, to = c(0.4, 1))) %>%
  mutate(alpha_cat = cut(alpha, 
                         breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
                         labels = c("< 10 %", "10 % - 20 %", "20 % - 30 %", "> 30%")))


multi_owner_pct <-
  (occ_rev_comp$nb[occ_rev_comp$composition == "multi" &
                     occ_rev_comp$mode_occupation == "Propriétaire"] /
     sum(occ_rev_comp$nb[occ_rev_comp$mode_occupation == "Propriétaire"])) |>
  convert_pct()

wkids_owner_pct <-
  (occ_rev_comp$nb[occ_rev_comp$composition == "w_kids" &
                     occ_rev_comp$mode_occupation == "Propriétaire"] /
     sum(occ_rev_comp$nb[occ_rev_comp$mode_occupation == "Propriétaire"])) |>
  convert_pct()

solo_tenant_pct <-
  (occ_rev_comp$nb[occ_rev_comp$composition == "solo" &
                     occ_rev_comp$mode_occupation == "Locataire"] /
     sum(occ_rev_comp$nb[occ_rev_comp$mode_occupation == "Locataire"])) |>
  convert_pct()

plot_4_2_1 <- 
  ggplot(occ_rev_comp, aes(x = comp_pretty, y = median_revenue, fill = mode_occupation, alpha = alpha_cat)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = \(x) paste(convert_number(x), "$")) +
  scale_alpha_manual(values = c("< 10 %" = 0.2, "10 % - 20 %" = 0.5, 
                                "20 % - 30 %" = 0.8, "> 30%" = 1)) +
  guides(
    alpha = guide_legend(
      title = "Proportion des ménages\nselon le statut d'occupation",
      nrow = 2
    ),
    fill = guide_legend(nrow = 1)
  ) +
  labs(x = NULL, y = "Revenu annuel médian", fill = "Statut d'occupation") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.5, "in"),
        axis.title.y = element_text(size = 11))

ggsave(plot = plot_4_2_1, "outputs/4/plot_4_2_1.pdf", width = 7.5, height = 4)

rev_fun_421 <- function(mode_occupation, composition) {
  z <- occ_rev_comp$median_revenue[occ_rev_comp$mode_occupation == mode_occupation &
                                occ_rev_comp$composition == composition]
  paste(convert_number(z), "$")
}


hou_med_inc <- cancensus::get_census("CA21", 
                                     vectors = c(med_inc = "v_CA21_906"), 
                                     regions = c("CSD" = 2465005))$med_inc

loc_mono_vs_med <- 
  round((occ_rev_comp$median_revenue[occ_rev_comp$mode_occupation == "Locataire" &
                                       occ_rev_comp$composition == "mono"] / 
           hou_med_inc)*100) / 100

loc_solo_vs_med <- 
  round((occ_rev_comp$median_revenue[occ_rev_comp$mode_occupation == "Locataire" &
                                       occ_rev_comp$composition == "solo"] / 
           hou_med_inc)*100) / 100

hou_med_inc <- hou_med_inc |> 
  convert_number() |>
  paste("$")




# # 4.2.2 -------------------------------------------------------------------
# 
# mode_occupation <- c(owner = "Propriétaire", tenant = "Locataire")
# 
# data_4_2_2 <- crosstab_get(mode_occupation = mode_occupation, revenu = c("low" = "Avec un faible revenu fondé sur la Mesure de faible revenu après impôt (MFR-ApI)",
#                                                                          "total" = "Total - Revenu total du ménage")) |> 
#   mutate("total_low" = owner_low + tenant_low) |> 
#   full_join(laval_ct_hou, join_by("CT_ID" == "GeoUID")) |> 
#   st_as_sf() |> 
#   interpolate(additive_vars = c("owner_low", "tenant_low", "owner_total", "tenant_total", "total_low", "Households"))
# 
# data_4_2_2_map <- data_4_2_2 |> 
#   mutate(low_prop = total_low / Households) |> 
#   mutate(low_quantile = cut(low_prop, breaks = classInt::classIntervals(low_prop, n = 5, style = "jenks")$brks, include.lowest = TRUE,
#                             labels = c("< 9,7 %", "9,7 - 12,1 %", "12,1 - 16,6 %", "16,6 - 20,4 %", "> 20,4 %")))
# 
# map_4_2_2 <- ggplot(data = data_4_2_2_map) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = low_quantile), alpha = 0.9, color = "transparent", show.legend = TRUE) +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Proportion de ménages à faible revenu") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# data_4_2_2_table <- data_4_2_2 |> 
#   st_drop_geometry() |> 
#   select(-ID) |> 
#   bind_rows(
#     summarise(
#       data_4_2_2,
#       NOM = "Laval (V)",
#       owner_total = sum(owner_total, na.rm = TRUE),
#       owner_low = sum(owner_low, na.rm = TRUE),
#       tenant_total = sum(tenant_total, na.rm = TRUE),
#       tenant_low = sum(tenant_low, na.rm = TRUE),
#       Households = sum(Households, na.rm = TRUE),
#       total_low = sum(total_low, na.rm = TRUE)
#     )
#   ) |> 
#   mutate(across(where(is.numeric) & !NOM, round, digits = 0)) |> 
#   mutate(tenant_low_prop = tenant_low / tenant_total,
#          owner_low_prop = owner_low / owner_total,
#          low_prop = total_low / Households) |> 
#   select(NOM, owner_total, owner_low, owner_low_prop, tenant_total, tenant_low,
#          tenant_low_prop, Households, total_low, low_prop) |> 
#   mutate(Laval_row = if_else(NOM == "Laval (V)", 1, 0)) |> 
#   arrange(desc(Laval_row)) |> 
#   select(-Laval_row) 
# 
# table_4_2_2 <- data_4_2_2_table |> 
#   gt() |>
#   cols_label(
#     NOM = "District électorale",
#     owner_total = "Nombre de ménages (n)",
#     owner_low = "Ménages à faible revenu (n)",
#     owner_low_prop = "Ménages à faible revenu (%)",
#     tenant_total = "Nombre de ménages (n)",
#     tenant_low = "Ménages à faible revenu (n)",
#     tenant_low_prop = "Ménages à faible revenu (%)",
#     Households = "Nombre de ménages (n)",
#     total_low = "Ménages à faible revenu (n)",
#     low_prop = "Ménages à faible revenu (%)"
#   ) |>
#   tab_style(
#     style = cell_borders(sides = "right", color = "lightgrey", weight = px(2)),
#     locations = cells_body(columns = c(1, 4, 7))
#   ) |> 
#   tab_style(
#     style = cell_borders(sides = "bottom", color = "lightgrey", weight = px(2)),
#     locations = cells_body(rows = 1)
#   ) |> 
#   tab_spanner(
#     label = "Propriétaires",
#     columns = c(owner_total, owner_low, owner_low_prop)
#   ) |>
#   tab_spanner(
#     label = "Locataires",
#     columns = c(tenant_total, tenant_low, tenant_low_prop)
#   ) |>
#   tab_spanner(
#     label = "Total",
#     columns = c(Households, total_low, low_prop)
#   ) |> 
#   tab_style(
#     style = cell_fill(color = "#f0f0f0"),
#     locations = cells_body(rows = seq(2, nrow(data_4_2_2_table), by = 2))
#   ) |>
#   data_color(
#     columns = c(4, 7, 10),
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |> 
#   fmt(columns = c(2, 3, 5, 6, 8, 9), fns = convert_number) |>
#   fmt(columns = c(4, 7, 10), fns = convert_pct) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |>
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(14)),
#     locations = cells_column_labels()) |>
#   tab_style(
#     style = cell_text(size = px(15)),
#     locations = cells_column_spanners()
#   ) |>
#   cols_width(
#     c(1) ~ px(75),
#     c(2, 5, 8) ~ px(90),
#     c(3, 6, 9) ~ px(110),
#     c(4, 7, 10) ~ px(115)
#   )
# 
# low_laval <- data_4_2_2_table |> filter(NOM == "Laval (V)") |> pull(low_prop) |> convert_pct()
# low_laval_tenant <- data_4_2_2_table |> filter(NOM == "Laval (V)") |> pull(tenant_low_prop) |> convert_pct()
# low_laval_owner <- data_4_2_2_table |> filter(NOM == "Laval (V)") |> pull(owner_low_prop) |> convert_pct()
# 
# ggsave(plot = map_4_2_2, "outputs/4/map_4_2_2.pdf", width = 7.5, height = 6)
# gtsave(table_4_2_2, "outputs/4/table_4_2_2.png", vwidth = 3200)
# 
# # 4.2.3 -------------------------------------------------------------------
# 
# data_4_2_3_percent <- get_cmhc(survey = "Core Housing Need", series = "Housing Standards",
#                                dimension = "% of Households in Core Housing Need", breakdown = "Historical Time Periods",
#                                geo_uid = 2465005)
# 
# 
# 
# #Proportion of core housing by electoral district in 2021
# data_4_2_3_map <- get_census(
#   dataset = "CA21",
#   regions = list(CSD = 2465005),
#   level = "CT",
#   vectors = c("core" = "v_CA21_4303", "hh" = "v_CA21_4302"),
#   geo_format = "sf") |> 
#   select(GeoUID, core, hh) |> 
#   interpolate(additive_vars = c("core", "hh")) |> 
#   mutate(prop = core / hh) |> 
#   mutate(low_quantile = cut(prop, breaks = classInt::classIntervals(prop, n = 5, style = "jenks")$brks, include.lowest = TRUE,
#                             labels = c("< 4,0 %", "4,0 - 5,5 %", "5,5 - 7,8 %", "7,8 - 9,3 %", "> 9,3 %")))
# 
# map_4_2_3 <- ggplot(data = data_4_2_3_map) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = low_quantile), alpha = 0.9, color = "transparent", show.legend = TRUE) +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Proportion de ménages ayant des besoins impérieux") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# # Combine all data frames into one
# data_4_2_3_2021 <- do.call(rbind, data_list) |> 
#   filter(DateString == 2021 & `Households in Core Housing Need` == "Total") |> 
#   select(GeoUID, Value) |> 
#   full_join(laval_hh_2021, by = "GeoUID") |> 
#   st_as_sf() |> 
#   interpolate(additive_vars = c("Value", "households"))
# 
# #Evolution of core housing
# laval_hh <- get_cmhc(survey = "Census", series = "All Households",
#                      dimension = "Age of Primary Household Maintainer",
#                      breakdown = "Historical Time Periods",
#                      geo_uid = 2465005) |> 
#   filter(`Age of Primary Household Maintainer` == "Total") |> 
#   select(DateString, Value)
# 
# data_4_2_3_evol <- get_cmhc(survey = "Core Housing Need", series = "Housing Standards",
#                              dimension = "Households in Core Housing Need", breakdown = "Historical Time Periods",
#                              geo_uid = 2465005) |> 
#   rename("Count" = "Value") |> 
#   select(DateString, `Households in Core Housing Need`, Count) |> 
#   left_join(laval_hh, by = "DateString") |> 
#   filter(`Households in Core Housing Need` != "Above Standards",
#          `Households in Core Housing Need` != "Below One or More Housing Standards") |> 
#   mutate(`Households in Core Housing Need` = case_when(
#     `Households in Core Housing Need` == "Below Affordability Standard" ~ "Inférieur au seuil d'abordabilité",
#     `Households in Core Housing Need` == "Below Adequacy Standard" ~ "Inférieur au seuil de taille convenable",
#     `Households in Core Housing Need` == "Below Suitability Standard" ~ "Inférieur au seuil de qualité convenable",
#     `Households in Core Housing Need` == "Total" ~ "Nombre total de ménages dans le besoin impérieux",
#     TRUE ~ `Households in Core Housing Need`
#   )) |>
#   mutate(prop = Count / Value)
# 
# plot_4_2_3_evol <- ggplot(data_4_2_3_evol, aes(x = DateString, y = prop, color = `Households in Core Housing Need`, group = `Households in Core Housing Need`)) +
#   geom_line(linewidth = 1.5) +
#   geom_point(size = 2) + 
#   labs(x = "",
#        y = "Proportion de ménages ayant\ndes besoins impérieux (%)",
#        color = "Housing Standard") +
#   scale_y_continuous(labels = function(x) convert_pct(x)) +
#   scale_color_manual(values = c(
#     "Inférieur au seuil d'abordabilité" = color_theme("yellowclimate"),
#     "Inférieur au seuil de taille convenable" = color_theme("purpletransport"),
#     "Inférieur au seuil de qualité convenable" = color_theme("pinkhealth"),
#     "Nombre total de ménages dans le besoin impérieux" = color_theme("blueexplorer")
#   )) +
#   graph_theme +
#   guides(color = guide_legend(nrow = 2))
# 
# data_4_2_3_table <- get_cmhc(survey = "Core Housing Need", series = "Housing Standards",
#                              dimension = "% of Households in Core Housing Need", breakdown = "Historical Time Periods",
#                              geo_uid = 2465005) |> 
#   group_by(DateString, `% of Households in Core Housing Need`) |> 
#   summarise(
#     Value = sum(Value, na.rm = TRUE),
#     .groups = 'drop'
#   ) |> 
#   pivot_wider(
#     names_from = `% of Households in Core Housing Need`,
#     values_from = c(Value),
#     values_fill = list(Value = 0)
#   ) |> 
#   rename("Year" = DateString) |> 
#   select(-`Above Standards`) |> 
#   mutate(across(-Year, ~ .x / 100))
# 
# table_4_2_3 <- data_4_2_3_table |> 
#   gt() |>
#   tab_style(
#     style = cell_fill(color = "#f0f0f0"),
#     locations = cells_body(rows = seq(2, nrow(data_4_2_3_table), by = 2))
#   ) |>
#   fmt(columns = c(2:6), fns = convert_pct) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |>
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(14)),
#     locations = cells_column_labels())
# 
# core_2006 <- data_4_2_3_table |> filter(Year == 2006) |> pull(Total) |> convert_pct()
# core_2021 <- data_4_2_3_table |> filter(Year == 2021) |> pull(Total) |> convert_pct()
# core_diff <- data_4_2_3_table |> filter(Year == 2006 | Year == 2021) |> summarise(across(-Year, ~ .[Year == 2006] - .[Year == 2021])) |> pull(Total) |> convert_pct()
# 
# ggsave(plot = plot_4_2_3_evol, "outputs/4/plot_4_2_3_evol.pdf", width = 7.5, height = 6)
# gtsave(table_4_2_3, "outputs/4/table_4_2_3.png", vwidth = 3200)

# 4.2.4 -------------------------------------------------------------------

rev <- c("none"= "Sans revenu total", "10" = "  Supérieur à zéro, moins de 10 000 $",
         "2" = "10 000 $ à 19 999 $", "40" = "20 000 $ à 39 999 $",
         "60" = "40 000 $ à 59 999 $", "80" = "60 000 $ à 79 999 $",
         "100" = "80 000 $ à 99 999 $", "124" = "100 000 $ à 124 999 $",
         "125" = "125 000 $ et plus")


owner_income_housingcost <- crosstab_get(mode_occupation = c("owner" = "Propriétaire"),
                                         revenu = rev,
                                         characteristic = c("housingcost" = "Frais de logement mensuels médians propriétaire ($)"),
                                         scale = "CSD") |> 
  mutate(owner_housingcost_20 = owner_housingcost_none + owner_housingcost_10 + owner_housingcost_2) |> 
  select(-owner_housingcost_none, -owner_housingcost_10, -owner_housingcost_2, -CSD_ID) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "housingcost") |> 
  mutate(income = case_when(
    str_detect(type, "20") ~ "< 19 999 $",
    str_detect(type, "40") ~ "20 - 39 999 $",
    str_detect(type, "60") ~ "40 - 59 999 $",
    str_detect(type, "80") ~ "60 - 79 999 $",
    str_detect(type, "100") ~ "80 - 99 999 $",
    str_detect(type, "124") ~ "100 - 124 999 $",
    str_detect(type, "125") ~ "> 125 000 $",
    TRUE ~ NA_character_)) |> 
  transmute(occupation = "Propriétaire", income, housingcost)
owner_income_housingcost <- merge(
  owner_income_housingcost, 
  crosstab_get(mode_occupation = c("owner" = "Propriétaire"),
               revenu = rev,
               scale = "CSD") |> 
    mutate(owner_20 = owner_none + owner_10 + owner_2) |> 
    select(-owner_none, -owner_10, -owner_2, -CSD_ID) |> 
    pivot_longer(cols = everything(), names_to = "type", values_to = "nb") |> 
    mutate(income = case_when(
      str_detect(type, "20") ~ "< 19 999 $",
      str_detect(type, "40") ~ "20 - 39 999 $",
      str_detect(type, "60") ~ "40 - 59 999 $",
      str_detect(type, "80") ~ "60 - 79 999 $",
      str_detect(type, "100") ~ "80 - 99 999 $",
      str_detect(type, "124") ~ "100 - 124 999 $",
      str_detect(type, "125") ~ "> 125 000 $",
      TRUE ~ NA_character_)) |> 
    transmute(occupation = "Propriétaire", income, nb),
  by = c("occupation", "income")) %>%
  group_by(occupation) %>%
  mutate(alpha = nb / sum(nb)) %>%
  mutate(alpha_custom = scales::rescale(alpha, to = c(0.4, 1))) %>%
  mutate(alpha_cat = cut(alpha, 
                         breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
                         labels = c("< 10 %", "10 % - 20 %", "20 % - 30 %", "> 30%")))


tenant_income_housingcost <- crosstab_get(mode_occupation = c("tenant" = "Locataire"),
                                          revenu = rev,
                                          characteristic = c("housingcost" = "Frais de logement mensuels médians locataire ($)" ),
                                          scale = "CSD") |> 
  mutate(tenant_housingcost_20 = tenant_housingcost_none + tenant_housingcost_10 + tenant_housingcost_2) |> 
  select(-tenant_housingcost_none, -tenant_housingcost_10, -tenant_housingcost_2, -CSD_ID) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "housingcost") |> 
  mutate(income = case_when(
    str_detect(type, "20") ~ "< 19 999 $",
    str_detect(type, "40") ~ "20 - 39 999 $",
    str_detect(type, "60") ~ "40 - 59 999 $",
    str_detect(type, "80") ~ "60 - 79 999 $",
    str_detect(type, "100") ~ "80 - 99 999 $",
    str_detect(type, "124") ~ "100 - 124 999 $",
    str_detect(type, "125") ~ "> 125 000 $",
    TRUE ~ NA_character_)) |> 
  transmute(occupation = "Locataire", income, housingcost)
tenant_income_housingcost <- merge(
  tenant_income_housingcost, 
  crosstab_get(mode_occupation = c("tenant" = "Locataire"),
               revenu = rev,
               scale = "CSD") |> 
    mutate(tenant_20 = tenant_none + tenant_10 + tenant_2) |> 
    select(-tenant_none, -tenant_10, -tenant_2, -CSD_ID) |> 
    pivot_longer(cols = everything(), names_to = "type", values_to = "nb") |> 
    mutate(income = case_when(
      str_detect(type, "20") ~ "< 19 999 $",
      str_detect(type, "40") ~ "20 - 39 999 $",
      str_detect(type, "60") ~ "40 - 59 999 $",
      str_detect(type, "80") ~ "60 - 79 999 $",
      str_detect(type, "100") ~ "80 - 99 999 $",
      str_detect(type, "124") ~ "100 - 124 999 $",
      str_detect(type, "125") ~ "> 125 000 $",
      TRUE ~ NA_character_)) |> 
    transmute(occupation = "Locataire", income, nb),
  by = c("occupation", "income")) %>%
  group_by(occupation) %>%
  mutate(alpha = nb / sum(nb)) %>%
  mutate(alpha_custom = scales::rescale(alpha, to = c(0.4, 1))) %>%
  mutate(alpha_cat = cut(alpha, 
                         breaks = c(-Inf, 0.1, 0.2, 0.3, Inf),
                         labels = c("< 10 %", "10 % - 20 %", "20 % - 30 %", "> 30%")))

income_housingcost <- rbind(owner_income_housingcost, tenant_income_housingcost) |> 
  mutate(income = factor(income, levels = c(
    "< 19 999 $",
    "20 - 39 999 $",
    "40 - 59 999 $",
    "60 - 79 999 $",
    "80 - 99 999 $",
    "100 - 124 999 $",
    "> 125 000 $"))) %>%
  # Make the alpha be dependent on the group
  group_by(occupation) %>%
  mutate(alpha_custom = scales::rescale(nb, to = c(0.4, 1)))

plot_4_2_4 <- 
  ggplot(income_housingcost, aes(x = income, y = housingcost, fill = occupation, alpha = alpha_cat)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Frais de logement", fill = NULL) +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = \(x) paste(convert_number(x), "$")) +
  scale_alpha_manual(values = c("< 10 %" = 0.2, "10 % - 20 %" = 0.5, 
                                "20 % - 30 %" = 0.8, "> 30%" = 1)) +
  guides(
    alpha = guide_legend(
      title = "Proportion des ménages\nselon le statut d'occupation",
      nrow = 2
    ),
    fill = guide_legend(nrow = 1)
  ) +
  labs(x = NULL, y = "Revenu annuel médian", fill = "Statut d'occupation") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        legend.spacing.x = unit(0.5, "in"),
        axis.title.y = element_text(size = 11))

ggsave(plot = plot_4_2_4, "outputs/4/plot_4_2_4.pdf", width = 7.5, height = 4)

loyer_fun_421 <- function(mode_occupation, income) {
  z <- income_housingcost$housingcost[income_housingcost$occupation == mode_occupation &
                                     income_housingcost$income == income]
  paste(convert_number(z), "$")
}

# Filtrer les propriétaires et locataires avec un revenu > 100 000 $
proprietaires <- income_housingcost %>%
  filter(occupation == "Propriétaire", income %in% c("100 - 124 999 $", "> 125 000 $"))

locataires <- income_housingcost %>%
  filter(occupation == "Locataire", income %in% c("100 - 124 999 $", "> 125 000 $"))

# Calculer le frais de logement moyen pondéré pour les propriétaires
frais_proprio_pondere <- sum(proprietaires$housingcost * proprietaires$nb) / sum(proprietaires$nb)

# Calculer le frais de logement moyen pondéré pour les locataires
frais_locataire_pondere <- sum(locataires$housingcost * locataires$nb) / sum(locataires$nb)

# Calculer la différence en pourcentage
prop_pay_more_100k <- convert_pct((frais_proprio_pondere - frais_locataire_pondere) / frais_locataire_pondere)


# 4.2.5 -------------------------------------------------------------------

occ_stress_lowinc <- crosstab_get(mode_occupation = c("owner" = "Propriétaire",
                                 "tenant" = "Locataire"),
             revenu = c("lowinc" = "Avec un faible revenu fondé sur la Mesure de faible revenu après impôt (MFR-ApI)",
                        "total" = "Total - Revenu total du ménage"),
             characteristic = setNames(c("Moins de 30 %",
                                         "30 % ou plus", "50 % ou plus"), 
                                       c("Moins de 30 %",
                                         "30 % ou plus", "50 % ou plus")),
             scale = "CSD") %>%
  pivot_longer(cols = -CSD_ID, names_to = "category", values_to = "count") %>%
  separate(category, into = c("occupation", "percent", "lowinc"), sep = "_") %>%
  mutate(
    occupation = recode(occupation, "owner" = "Propriétaire", "tenant" = "Locataire"),
    percent = factor(percent, levels = c("Moins de 30 %", "30 % ou plus", "50 % ou plus"))
  )

plot_4_2_5 <- 
  ggplot(occ_stress_lowinc, aes(x = occupation, y = count, fill = lowinc)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ percent) +
  labs(
    x = NULL,
    y = "Nombre de ménages",
    fill = NULL,
    title = NULL
  ) +
  scale_fill_manual(
    values = c("total" = "#9E9090", "lowinc" = "#ADB033"),
    labels = c("total" = "Tous les ménages", "lowinc" = "Faible revenu")
  ) +
  graph_theme +
  scale_y_continuous(labels = convert_number)

ggsave(plot = plot_4_2_5, "outputs/4/plot_4_2_5.pdf", width = 5, height = 7.5)


# denom <- sum(occ_stress_lowinc$count[occ_stress_lowinc$lowinc == "total" &
#                                        occ_stress_lowinc$occupation == "Locataire" &
#                                        occ_stress_lowinc$percent %in% c("Moins de 30 %", "30 % ou plus")])
# z <- sum(occ_stress_lowinc$count[occ_stress_lowinc$occupation == "Locataire" &
#                                    occ_stress_lowinc$lowinc == "total" &
#                                    occ_stress_lowinc$percent == "30 % ou plus"])
# tenant_30plus_pct <- convert_pct(z / denom)
# 
# denom <- sum(occ_stress_lowinc$count[occ_stress_lowinc$lowinc == "total" &
#                                        occ_stress_lowinc$occupation == "Propriétaire" &
#                                        occ_stress_lowinc$percent %in% c("Moins de 30 %", "30 % ou plus")])
# z <- sum(occ_stress_lowinc$count[occ_stress_lowinc$occupation == "Propriétaire" &
#                                    occ_stress_lowinc$lowinc == "total" &
#                                    occ_stress_lowinc$percent == "30 % ou plus"])
# owner_30plus_pct <- convert_pct(z / denom)


denom <- sum(occ_stress_lowinc$count[occ_stress_lowinc$lowinc == "lowinc" &
                                       occ_stress_lowinc$occupation == "Locataire" &
                                       occ_stress_lowinc$percent %in% c("Moins de 30 %", "30 % ou plus")])
z <- sum(occ_stress_lowinc$count[occ_stress_lowinc$occupation == "Locataire" &
                                   occ_stress_lowinc$lowinc == "lowinc" &
                                   occ_stress_lowinc$percent == "30 % ou plus"])
tenant_lowinc_30plus_pct <- convert_pct(z / denom)
z <- sum(occ_stress_lowinc$count[occ_stress_lowinc$occupation == "Locataire" &
                                   occ_stress_lowinc$lowinc == "lowinc" &
                                   occ_stress_lowinc$percent == "50 % ou plus"])
tenant_lowinc_50plus_pct <- convert_pct(z / denom)

z <- sum(occ_stress_lowinc$count[occ_stress_lowinc$occupation == "Propriétaire" &
                                   occ_stress_lowinc$lowinc == "lowinc" &
                                   occ_stress_lowinc$percent == "50 % ou plus"])
owner_lowinc_50plus_pct <- convert_pct(z / denom)


# 4.2.6 -------------------------------------------------------------------


# 4.2.7 -------------------------------------------------------------------


not_suitable_2021 <- get_census(dataset = "CA21",
           regions = list(CSD = 2465005),
           level = "CSD",
           vectors = c("not_suitable" = "v_CA21_4262",
                       "total" = "v_CA21_4260")
)

not_suitable_2016 <- get_census(dataset = "CA16",
           regions = list(CSD = 2465005),
           level = "CSD",
           vectors = c("not_suitable" = "v_CA16_4861",
                       "total" = "v_CA16_4859")
)

not_suitable_2011 <- get_census(dataset = "CA11",
           regions = list(CSD = 2465005),
           level = "CSD",
           vectors = c("not_suitable" = "v_CA11N_2276",
                       "total" = "v_CA11N_2274")
)

ns_2021 <- convert_pct(not_suitable_2021$not_suitable / not_suitable_2021$total)
ns_2016 <- convert_pct(not_suitable_2016$not_suitable / not_suitable_2016$total)
ns_2011 <- convert_pct(not_suitable_2011$not_suitable / not_suitable_2011$total)


# not_suitable_CT <- get_census(dataset = "CA21",
#                               regions = list(CSD = 2465005),
#                               level = "CT",
#                               vectors = c("not_suitable" = "v_CA21_4262",
#                                           "total" = "v_CA21_4260"),
#                               geo_format = "sf"
# )
# 
# notsuitable <- interpolate(not_suitable_CT, additive_vars = c("not_suitable", "total"))

notsuitable <- get_census(dataset = "CA21",
                              regions = list(CSD = 2465005),
                              level = "DA",
                              vectors = c("not_suitable" = "v_CA21_4262",
                                          "total" = "v_CA21_4260"),
                              geo_format = "sf"
)
notsuitable <- sf::st_transform(notsuitable, crs = 32618)
notsuitable$not_suitable_prop <- notsuitable$not_suitable / notsuitable$total

labels <- c("< 2 %", "2 % - 4 %", "4 % - 6 %", "6 % - 8 %", "> 8 %")
notsuitable$bins <- cut(notsuitable$not_suitable_prop, 
                        breaks = c(-Inf, 0.02, 0.04, 0.06, 0.08, Inf), 
                        labels = labels, 
                        include.lowest = TRUE)
notsuitable$bins <- factor(notsuitable$bins, levels = labels)


plot_4_2_7 <-
  ggplot(notsuitable) +
  gg_cc_tiles +
  geom_sf(aes(fill = bins), lwd = 0, show.legend = TRUE, color = "transparent") + 
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = NULL,#"Proportion de ménages vivant dans\nun logement de taille non convenable",
                    labels = labels,
                    drop = FALSE,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1)) +
  gg_cc_theme

ggsave(plot = plot_4_2_7, "outputs/4/plot_4_2_7.pdf", width = 6.5, height = 6)


# 4.2.8 -------------------------------------------------------------------


# 4.2.9 -------------------------------------------------------------------


# 4.2.10 ------------------------------------------------------------------

# Loyer médian ------------------------------------------------------------

# Setting the years to pull data from
years <- 2010:2023

# Pulling average rent data
avg_rent_cmhc <- function(geoid, years, geoname) {
  map_dfr(years, function(cyear) {
    get_cmhc(survey = "Rms", series = "Median Rent", dimension = "Bedroom Type",
             breakdown = "Census Subdivision", geo_uid = geoid, year = cyear) |> 
      mutate(Geography = geoname) |> 
      filter(str_detect(`Bedroom Type`, "Total")) |> 
      select(Geography, Year, Value)
  })
}

#Grabbing annual average rent data from 2010 to 2023
avg_rent_lvl <- avg_rent_cmhc(2465005, years, "Laval")


# Manually inputting the province of Quebec's data as it's unavailable using the CMHC package
# src = https://www.cmhc-schl.gc.ca/professionals/housing-markets-data-and-research/housing-data/data-tables/rental-market/rental-market-report-data-tables
avg_rent_qc <- data.frame(
  Geography = "Québec",
  Year = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
           2020, 2021, 2022, 2023),
  Value = c(648, 666, 693, 679, 691, 712, 727, 736, 761, 800, 845, 874, 952, 1022)
)

# Preparing the table for the line graph
avg_rent_annual <- bind_rows(avg_rent_lvl, avg_rent_qc) |> 
  mutate(Geography = factor(Geography, levels = c("Laval", "Québec")))
# avg_rent_annual_inf <- bind_rows(avg_rent_lvl_inf, avg_rent_qc_inf) |> 
#   mutate(Geography = factor(Geography, levels = c("Laval", "Québec")))

# avg_rent_annual$indexed <- "Dollars courants (non ajusté)"
# avg_rent_annual_inf$indexed <- "Ajusté à l'inflation ($ de 2023)"
# avg_rent_annual <- rbind(avg_rent_annual_inf, avg_rent_annual)

# Line graph
housing_loyermed_plot <-
  ggplot(avg_rent_annual, aes(x = Year, y = `Value`, group = Geography, color = Geography)) +
  geom_line(linewidth = 1.5) +
  labs(title = element_blank(),
       x = NULL,
       y = "Loyer mensuel médian ($)") +
  scale_color_manual(values = c("Laval" = color_theme("greenecology"), "Québec" = color_theme("blueexplorer"))) +
  scale_y_continuous(labels = convert_number) +
  graph_theme +
  theme(legend.title = element_blank())

ggplot2::ggsave(filename = here::here("outputs/4/plot_4_2_10_loyer.pdf"),
                plot = housing_loyermed_plot, width = 3, height = 3)

housing_loyer_2023 <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2023 & avg_rent_annual$Geography == "Laval"]
housing_loyer_2010 <- avg_rent_annual$Value[
  avg_rent_annual$Year == 2010 & avg_rent_annual$Geography == "Laval"]
housing_loyer_var <- convert_pct((housing_loyer_2023 - housing_loyer_2010) / housing_loyer_2010)


# 4.2.11 ------------------------------------------------------------------

# Créer le dataframe
logements_data <- data.frame(
  Programme = c("Financés par la CMM",
                "En coopératives d'habitation",
                "Gérés par des OBNL",
                "Gérés par l'OMH de Laval",
                "AccèsLogis Québec",
                "Programme LAQ",
                "Logements HLM privés et autochtones"),
  Nombre_de_Logements = c(2571, 933, 2065, 1974, 1108, 271, 80)
)

# Créer le graphique
aide_au_logement <-
  ggplot(logements_data, aes(x = reorder(Programme, Nombre_de_Logements), 
                             y = Nombre_de_Logements)) +
  geom_bar(stat = "identity", fill = "#CD718C") +
  scale_y_continuous(labels = convert_number) +
  coord_flip() +
  labs(title = NULL,
       x = "Programme",
       y = "Nombre de logements",
       fill = "Année") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1))

ggsave(plot = aide_au_logement, "outputs/4/plot_4_2_11_aide_au_logement.pdf", width = 6.5, height = 6)


# 4.2.12 ------------------------------------------------------------------

# 4.2.13 ------------------------------------------------------------------

att <- read_xlsx("data/OMHL_Données délai d'attente moyen par territoire - juillet 2024.xlsx") |> 
  setNames(c("programme", "territoire", "jours"))

# Get the clientele
vec_65 <- grepl("(ans et \\+)|(ans \\+)", att$territoire)
vec_59 <- grepl("ans et moins", att$territoire)
vec_fam <- grepl("Famille", att$territoire)
att <- mutate(att, clientele = case_when(vec_65 ~ "sfplus",
                                         vec_59 ~ "lessfn",
                                         vec_fam ~ "fam",
                                         TRUE ~ "other"))

# Per cliente
att_clientele <- att |> 
  group_by(clientele) |> 
  summarize(jours = mean(jours))

att_fam <- att_clientele$jours[att_clientele$clientele == "fam"] |> round()
att_lessfn <- att_clientele$jours[att_clientele$clientele == "lessfn"] |> round()
att_other <- att_clientele$jours[att_clientele$clientele == "other"] |> round()
att_sfplus <- att_clientele$jours[att_clientele$clientele == "sfplus"] |> round()

att_general_mean <- convert_number(mean(att$jours))
att_programme_table <- 
att |> 
  group_by(programme) |> 
  summarize(jours = round(mean(jours))) |> 
  arrange(-jours) |> 
  mutate(programme = case_when(programme == "COOP" ~ "Logements en coopérative d'habitation (COOP)",
                               programme == "OBNL" ~ "Logements en organisme à but non lucratif (OBNL)",
                               programme == "ACL" ~ "Programme AccèsLogis Québec (ACL)",
                               programme == "HLM" ~ "Programme de logement sans but lucratif (HLM)",
                               programme == "LAQ" ~ "Volet social et communautaire du programme Logement abordable Québec (LAQ)",
                               programme == "COM" ~ "Logement communautaire (COM) (?) (TKTK)",
                               TRUE ~ NA_character_)) |> 
  gt() |> 
  cols_label(
    programme = "Programme",
    jours = "Nombre de jours d'attente",
  ) |>
  data_color(
    columns = "jours",
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
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
    table.width = px(3 * 96)
  )

gtsave(att_programme_table, "outputs/4/4_2_13_attprogrammetable.png", zoom = 1)


# R Markdown --------------------------------------------------------------
qs::qsavem(#plot_4_2_1, plot_4_2_2, map_4_2_2, 
           att_general_mean, att_fam, att_lessfn, att_other, map_4_2_2, table_4_2_2,
           att_sfplus, att_programme_table, occ_rev_comp, rev_fun_421,
           low_laval, low_laval_owner, low_laval_tenant, #core_2006, core_2021, core_diff,
           loyer_fun_421, income_housingcost, tenant_lowinc_30plus_pct, tenant_lowinc_50plus_pct, 
           plot_4_2_1, plot_4_2_4, plot_4_2_5, ns_2021, ns_2016, ns_2011, plot_4_2_7,
           housing_loyermed_plot, housing_loyer_2023, housing_loyer_2010, housing_loyer_var,
           multi_owner_pct, wkids_owner_pct, solo_tenant_pct,
           hou_med_inc, loc_mono_vs_med, loc_solo_vs_med, prop_pay_more_100k,
           owner_lowinc_50plus_pct,
           file = "data/section_4_2.qsm")
