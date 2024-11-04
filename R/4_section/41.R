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

#Pulling numbers from PTO
owner_count_diff <- pto |> 
  filter(type == "owner", Year %in% c(2021, 2001)) |> 
  summarize(difference = count[Year == 2021] - count[Year == 2001]) |> 
  mutate(difference = convert_number(difference)) |> 
  pull(difference)

tenant_count_diff <- pto |> 
  filter(type == "tenant", Year %in% c(2021, 2001)) |> 
  summarize(difference = count[Year == 2021] - count[Year == 2001]) |> 
  mutate(difference = convert_number(difference)) |> 
  pull(difference)

owner_growth <- pto |> 
  filter(type == "owner", Year %in% c(2021, 2001)) |> 
  summarize(difference = count[Year == 2021] / count[Year == 2001] - 1) |> 
  mutate(difference = convert_pct(difference)) |> 
  pull(difference)

tenant_growth <- pto |> 
  filter(type == "tenant", Year %in% c(2021, 2001)) |> 
  summarize(difference = count[Year == 2021] / count[Year == 2001] - 1) |> 
  mutate(difference = convert_pct(difference)) |> 
  pull(difference)

owner_growth_16 <- pto |> 
  filter(type == "owner", Year %in% c(2021, 2016)) |> 
  summarize(difference = count[Year == 2021] / count[Year == 2016] - 1) |> 
  mutate(difference = convert_pct(difference)) |> 
  pull(difference)

tenant_growth_16 <- pto |> 
  filter(type == "tenant", Year %in% c(2021, 2016)) |> 
  summarize(difference = count[Year == 2021] / count[Year == 2016] - 1) |> 
  mutate(difference = convert_pct(difference)) |> 
  pull(difference)

total_hh_21 <- pto |> 
  filter(Year == 2021) |> 
  summarize(total = convert_number(sum(count))) |> 
  pull(total)

total_hh_01 <- pto |> 
  filter(Year == 2001) |> 
  summarize(total = convert_number(sum(count))) |> 
  pull(total)

total_prop_diff <- pto |> 
  filter(Year %in% c(2021, 2001)) |> 
  select(Year, count) |> 
  group_by(Year) |> 
  summarize(count = sum(count)) |> 
  pivot_wider(names_from = Year, values_from = count) |> 
  mutate(diff = convert_pct((`2021` - `2001`) / `2001`)) |> 
  pull(diff)

#Creating a grouped bar chart
plot_4_1_1_1 <- ggplot(pto, aes(x = Year, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = prop_cc), position = position_dodge(width = 0.9),
            vjust = 2, size = 2.5, color = "white") +
  scale_fill_manual(values = c("owner" = "#A3B0D1", "tenant" = "#CD718C"),
                    labels = c("owner" = "Propriétaire", "tenant" = "Locataire")) +
  scale_y_continuous(labels = convert_number) +
  labs(x = "", y = "Nombre de ménages") +
  graph_theme

#Grabbing the data to create a map
pto_map <- get_census(dataset = "CA21",
             regions = list(CSD = 2465005),
             level = "CT",
             vectors = c("total" = "v_CA21_4288", "owner" = "v_CA21_4305",
                         "tenant" = "v_CA21_4313"),
             geo_format = "sf") |> 
    select(total, owner, tenant) |> 
  mutate(owner = if_else(is.na(owner), 0, owner),
         tenant = if_else(is.na(tenant), 0, tenant)) |> 
  mutate(total = if_else(is.na(total), owner + tenant, total))

hh_map <- interpolate(pto_map, additive_vars = c("total", "owner", "tenant"))
hh_map <- hh_map |> 
  mutate(owner_pct = owner / total,
         tenant_pct = tenant / total)

#Finding and creating the breaks
#list(classInt::classIntervals(total_hh_map$total, n = 5, style = "jenks")$brks)
total_hh_breaks <- c(-Inf, 7000, 7500, 8000, 8500, Inf)
total_hh_breaks_lab <- c("< 7 000", "7 000 - 7 500", "7 500 - 8 000", "8 000 - 8 500", "> 8 500")
#list(classInt::classIntervals(owner_hh_map$owner, n = 5, style = "jenks")$brks)
owner_hh_breaks <- c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf)
owner_hh_breaks_lab <- c("< 20 %", "20 % - 40 %", "40 % - 60 %", "60 % - 80 %", "> 80 %")
#list(classInt::classIntervals(tenant_hh_map$tenant, n = 5, style = "jenks")$brks)
tenant_hh_breaks <- c(-Inf, 0.2, 0.4, 0.6, 0.8, Inf)
tenant_hh_breaks_lab <- c("< 20 %", "20 % - 40 %", "40 % - 60 %", "60 % - 80 %", "> 80 %")

#Applying the breaks as new columns to the data frame
total_hh_map <- hh_map |> 
  mutate(total_quantile = cut(total, breaks = total_hh_breaks, include.lowest = TRUE,
                              labels = total_hh_breaks_lab))

owner_hh_map <- hh_map |> 
  mutate(owner_quantile = cut(owner_pct, breaks = owner_hh_breaks, include.lowest = TRUE,
                              labels = owner_hh_breaks_lab))
owner_hh_map$owner_quantile <- factor(owner_hh_map$owner_quantile, levels = owner_hh_breaks_lab)

tenant_hh_map <- hh_map |> 
  mutate(tenant_quantile = cut(tenant_pct, breaks = tenant_hh_breaks, include.lowest = TRUE,
                              labels = tenant_hh_breaks_lab))
tenant_hh_map$tenant_quantile <- factor(tenant_hh_map$tenant_quantile, levels = tenant_hh_breaks_lab)

# pto_map <- pto_map |> 
#   mutate(
#     total_quantile = cut(total, breaks = pto_total_breaks, include.lowest = TRUE,
#                          labels = pto_total_breaks_labels),
#     owner_quantile = cut(total, breaks = pto_owner_breaks, include.lowest = TRUE,
#                          labels = pto_owner_breaks_labels),
#     tenant_quantile = cut(total, breaks = pto_tenant_breaks, include.lowest = TRUE,
#                          labels = pto_tenant_breaks_labels)
#   )

nb_majority_owner_districts <- nrow(owner_hh_map[owner_hh_map$owner_pct >= 0.5,])
nb_districts <- nrow(owner_hh_map)

#Plotting total number of households
map_total_hh <- ggplot(data = total_hh_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = total_quantile), alpha = 0.9, color = "transparent", show.legend = TRUE) +
  scale_fill_manual(values = curbcut_scale,
                    name = "Nombre de ménages",) +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Plotting total number of owner households
map_owner_hh <- ggplot(data = owner_hh_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = owner_quantile), alpha = 0.9, color = "transparent", show.legend = TRUE) +
  scale_fill_manual(values = curbcut_scale,
                    labels = owner_hh_breaks_lab,
                    drop = FALSE,
                    name = "Pourcentage de ménages propriétaires") +
  geom_sf(data = laval_sectors, fill = "transparent", color = "black") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Plotting total number of tenant households
map_tenant_hh <- ggplot(data = tenant_hh_map) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = tenant_quantile), alpha = 0.9, color = "transparent", show.legend = TRUE) +
  scale_fill_manual(values = curbcut_scale,
                    labels = tenant_hh_breaks_lab,
                    drop = FALSE,
                    name = "Pourcentage de ménages locataires") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

table_4_1_1_1_data <- total_hh_map |> 
  st_drop_geometry() |> 
  left_join(owner_hh_map, by = "NOM") |> 
  st_drop_geometry() |> 
  left_join(tenant_hh_map, by = "NOM") |> 
  st_drop_geometry() |> 
  select(NOM, total, owner, tenant) |> 
  mutate(total = round(total),
         owner = round(owner),
         tenant = round(tenant)) |> 
  mutate("Ménages propriétaires (%)" = owner / total,
         "Ménages locataires (%)" = tenant / total) |> 
  rename("District électoral" = "NOM",
         "Nombre total de ménages (n)" = "total",
         "Ménages propriétaires (n)" = "owner",
         "Ménages locataires (n)" = "tenant") |> 
  select("District électoral",
         "Nombre total de ménages (n)",
         "Ménages propriétaires (n)",
         "Ménages propriétaires (%)",
         "Ménages locataires (n)",
         "Ménages locataires (%)")

table_4_1_1_1 <- table_4_1_1_1_data |> 
  gt() |> 
  data_color(
    columns = c(4,6),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |> 
  fmt(columns = c(2,3,5), fns = convert_number) |> 
  fmt(columns = c(4,6), fns = convert_pct) |> 
  tab_style(
    style = cell_text(font = font_local_name, size = px(15)),
    locations = cells_body()) |> 
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()) |> 
  tab_options(
    table.font.size = 14,
    row_group.font.size = 12,
    table.width = px(8 * 88)
  )

#Saving the files as photos
ggsave_pdf_png_pdf_png(plot = plot_4_1_1_1, "outputs/4/1_statutoccupation.pdf", width = 6.5, height = 3)
ggsave_pdf_png_pdf_png(plot = map_total_hh, "outputs/4/2_carte_nbmenages.pdf", width = 6.5, height = 6)
ggsave_pdf_png_pdf_png(plot = map_owner_hh, "outputs/4/3_carte_nb_proprietaire.pdf", width = 6.5, height = 6)
ggsave_pdf_png_pdf_png(plot = map_tenant_hh, "outputs/4/4_carte_nb_locataire.pdf", width = 6.5, height = 6)

gt_save_word(gt_table = table_4_1_1_1, file_path = "outputs/4/1_statutoccupation.docx")

# Correlation between owner and typology?
single_semi_row <- get_census(dataset = "CA21",
                      regions = list(CSD = 2465005),
                      level = "CT",
                      vectors = c(total = "v_CA21_434", 
                                  single = "v_CA21_435", 
                                  semi = "v_CA21_436", 
                                  row = "v_CA21_437"),
                      geo_format = "sf")

ssr_districts <- interpolate(single_semi_row, additive_vars = c("total", "single", "semi", "row")) |> 
  mutate(house = (single + semi + row) / total)


owner_house_cor <- cor(hh_map$owner_pct, ssr_districts$house) |> convert_number()



# 4.1.1.2 Catégorie de revenu et mode d'occupation ------------------------
revenu <- c("none"= "Sans revenu total", "10" = "  Supérieur à zéro, moins de 10 000 $",
            "2" = "10 000 $ à 19 999 $", "40" = "20 000 $ à 39 999 $",
            "60" = "40 000 $ à 59 999 $", "80" = "60 000 $ à 79 999 $",
            "100" = "80 000 $ à 99 999 $", "124" = "100 000 $ à 124 999 $",
            "125" = "125 000 $ et plus")
mode_occupation <- c(owner = "Propriétaire", tenant = "Locataire")

data_4_1_1_2 <- crosstab_get(mode_occupation = mode_occupation, revenu = revenu, scale = "CSD") |> 
  mutate(owner_20 = owner_none + owner_10 + owner_2,
         tenant_20 = tenant_none + tenant_10 + tenant_2) |> 
  select(-owner_none, -tenant_none, -owner_10, -tenant_10, -owner_2, -tenant_2, -CSD_ID) |> 
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

hou_med_inc <- cancensus::get_census("CA21", 
                      vectors = c(med_inc = "v_CA21_906"), 
                      regions = c("CSD" = 2465005))$med_inc |> 
  convert_number() |> 
  paste("$")


less_80 <- data_4_1_1_2$count[data_4_1_1_2$income %in% c("20 - 39 999 $",
                                        "40 - 59 999 $",
                                        "60 - 79 999 $")] |> 
  sum()
tenant_less_80 <- 
  data_4_1_1_2$count[data_4_1_1_2$income %in% c("20 - 39 999 $",
                                                "40 - 59 999 $",
                                                "60 - 79 999 $") &
                       data_4_1_1_2$type == "Locataire"] |> 
  sum()
tenant_pct_less_80 <- (tenant_less_80 / less_80) |> convert_pct()

faible_rev_occ <- 
crosstab_get(mode_occupation = mode_occupation, 
             revenu = c(faible_rev = "Avec un faible revenu fondé sur la Mesure de faible revenu après impôt (MFR-ApI)"), 
                        scale = "CSD")

faible_rev_owner <- convert_pct(faible_rev_occ$owner_faible_rev / (faible_rev_occ$owner_faible_rev + faible_rev_occ$tenant_faible_rev))

data_4_1_1_2 <- 
data_4_1_1_2 |> 
  group_by(income) |> 
  mutate(pct = convert_pct(count / sum(count)))

plot_4_1_1_2 <- ggplot(data_4_1_1_2, aes(x = income, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Nombre de ménages", title = "") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  geom_text(aes(label = pct), position = position_dodge(width = 0.9),
            vjust = 2, size = 2.5, color = "white") +
  graph_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# data_4_1_1_2_2_table <- data_4_1_1_2 |>
#   pivot_wider(names_from = income, values_from = count) |>
#   mutate("< 19 999 $ (n)" = `< 19 999 $`,
#          "< 19 999 $ (%)" = `< 19 999 $` / sum(`< 19 999 $`, na.rm = TRUE),
#          "20 - 39 999 $ (n)" = `20 - 39 999 $`,
#          "20 - 39 999 $ (%)" = `20 - 39 999 $` / sum(`20 - 39 999 $`, na.rm = TRUE),
#          "40 - 59 999 $ (n)" = `40 - 59 999 $`,
#          "40 - 59 999 $ (%)" = `40 - 59 999 $` / sum(`40 - 59 999 $`, na.rm = TRUE),
#          "60 - 79 999 $ (n)" = `60 - 79 999 $`,
#          "60 - 79 999 $ (%)" = `60 - 79 999 $` / sum(`60 - 79 999 $`, na.rm = TRUE),
#          "80 - 99 999 $ (n)" = `80 - 99 999 $`,
#          "80 - 99 999 $ (%)" = `80 - 99 999 $` / sum(`80 - 99 999 $`, na.rm = TRUE),
#          "100 - 124 999 $ (n)" = `100 - 124 999 $`,
#          "100 - 124 999 $ (%)" = `100 - 124 999 $` / sum(`100 - 124 999 $`, na.rm = TRUE),
#          "> 125 000 $ (n)" = `> 125 000 $`,
#          "> 125 000 $ (%)" = `> 125 000 $` / sum(`> 125 000 $`, na.rm = TRUE)) |>
#   select(type, "< 19 999 $ (n)", "< 19 999 $ (%)", "20 - 39 999 $ (n)",
#          "20 - 39 999 $ (%)", "40 - 59 999 $ (n)", "40 - 59 999 $ (%)",
#          "60 - 79 999 $ (n)", "60 - 79 999 $ (%)", "80 - 99 999 $ (n)",
#          "80 - 99 999 $ (%)","100 - 124 999 $ (n)", "100 - 124 999 $ (%)",
#          "> 125 000 $ (n)", "> 125 000 $ (%)") |>
#   rename("Type de ménage" = type)
# 
# table_4_1_1_2_2 <- data_4_1_1_2_2_table |>
#   gt() |>
#   data_color(
#     columns = c(3,5,7,9,11,13,15),
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |>
#   fmt(columns = c(2,4,6,8,10,12,14), fns = convert_number) |>
#   fmt(columns = c(3,5,7,9,11,13,15), fns = convert_pct) |>
#   tab_spanner(
#     label = "Tranche de revenu",
#     columns = c(3:15)
#   ) |>
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |>
#   tab_style(
#     style = cell_text(font = font_local_name),
#     locations = cells_column_labels()) |>
#   tab_style(
#     style = cell_text(size = px(15)),
#     locations = cells_column_spanners(spanners = "Tranche de revenu")
#   ) |>
#   tab_options(
#     table.font.size = 13,
#     table.width = px(6 * 208)
#   ) |>
#   cols_width(
#     columns = c(1) ~ px(70),
#     everything() ~ px(72)
#   )
  

data_4_1_1_2_table_pre <- crosstab_get(mode_occupation = mode_occupation, revenu = revenu, scale = "CT") |> 
  mutate(owner_20 = owner_none + owner_10 + owner_2,
         tenant_20 = tenant_none + tenant_10 + tenant_2) |> 
  select(-owner_none, -tenant_none, -owner_10, -tenant_10, -owner_2, -tenant_2) |> 
  rename("GeoUID" = "CT_ID") |> 
  full_join(laval_ct, by = "GeoUID") |> 
  st_as_sf() |> 
  st_transform(crs = 32618)

data_4_1_1_2_table <- interpolate(from = data_4_1_1_2_table_pre, additive_vars = c("owner_20", "tenant_20", "owner_40", "tenant_40",
                                             "owner_60", "tenant_60", "owner_80", "tenant_80",
                                             "owner_100", "tenant_100", "owner_124", "tenant_124",
                                             "owner_125", "tenant_125")) |> 
  mutate(across(c("owner_20", "tenant_20", "owner_40", "tenant_40",
                  "owner_60", "tenant_60", "owner_80", "tenant_80",
                  "owner_100", "tenant_100", "owner_124", "tenant_124",
                  "owner_125", "tenant_125"), round)) |> 
  st_drop_geometry() |> 
  select(-ID) |> 
  pivot_longer(
    cols = starts_with("owner") | starts_with("tenant"), 
    names_to = c("type", "income"),
    names_pattern = "(owner|tenant)_(.*)",
    values_to = "count"
  ) |> 
  pivot_wider(names_from = income, values_from = count) |> 
  mutate(total = `20` + `40` + `60` + `80` + `100` + `124` + `125`) |> 
  mutate(
    "< 19 999 $ (n)" = `20`,
    "20 - 39 999 $ (n)" = `40`,
    "40 - 59 999 $ (n)" = `60`,
    "60 - 79 999 $ (n)" = `80`,
    "80 - 99 999 $ (n)" = `100`,
    "100 - 124 999 $ (n)" = `124`,
    "> 125 000 $ (n)" = `125`,
  ) |> 
  group_by(NOM) |>
  # mutate(
  #   "< 19 999 $ (%)" = `< 19 999 $ (n)` / sum(`< 19 999 $ (n)`, na.rm = TRUE),
  #   "20 - 39 999 $ (%)" = `20 - 39 999 $ (n)` / sum(`20 - 39 999 $ (n)`, na.rm = TRUE),
  #   "40 - 59 999 $ (%)" = `40 - 59 999 $ (n)` / sum(`40 - 59 999 $ (n)`, na.rm = TRUE),
  #   "60 - 79 999 $ (%)" = `60 - 79 999 $ (n)` / sum(`60 - 79 999 $ (n)`, na.rm = TRUE),
  #   "80 - 99 999 $ (%)" = `80 - 99 999 $ (n)` / sum(`80 - 99 999 $ (n)`, na.rm = TRUE),
  #   "100 - 124 999 $ (%)" = `100 - 124 999 $ (n)` / sum(`100 - 124 999 $ (n)`, na.rm = TRUE),
  #   "> 125 000 $ (%)" = `> 125 000 $ (n)` / sum(`> 125 000 $ (n)`, na.rm = TRUE)
  # ) |>
  mutate(
    "< 19 999 $ (%)" = `< 19 999 $ (n)` / total,
    "20 - 39 999 $ (%)" = `20 - 39 999 $ (n)` / total,
    "40 - 59 999 $ (%)" = `40 - 59 999 $ (n)` / total,
    "60 - 79 999 $ (%)" = `60 - 79 999 $ (n)` / total,
    "80 - 99 999 $ (%)" = `80 - 99 999 $ (n)` / total,
    "100 - 124 999 $ (%)" = `100 - 124 999 $ (n)` / total,
    "> 125 000 $ (%)" = `> 125 000 $ (n)` / total
  ) |>
  ungroup() |>
  mutate(across(where(is.numeric), ~ ifelse(is.nan(.), 0, .))) |>
  mutate(type = case_when(
    type == "owner" ~ "Propriétaire",
    type == "tenant" ~ "Locataire",
    TRUE ~ type
  )) |> 
  # select(NOM, type, total, "< 19 999 $ (n)", "< 19 999 $ (%)", "20 - 39 999 $ (n)",
  #        "20 - 39 999 $ (%)", "40 - 59 999 $ (n)", "40 - 59 999 $ (%)", 
  #        "60 - 79 999 $ (n)", "60 - 79 999 $ (%)", "80 - 99 999 $ (n)",
  #        "80 - 99 999 $ (%)","100 - 124 999 $ (n)", "100 - 124 999 $ (%)", 
  #        "> 125 000 $ (n)", "> 125 000 $ (%)") |> 
  select(NOM, type, total, "< 19 999 $ (%)", "20 - 39 999 $ (%)", "40 - 59 999 $ (%)", 
         "60 - 79 999 $ (%)", "80 - 99 999 $ (%)","100 - 124 999 $ (%)", 
         "> 125 000 $ (%)") |> 
  rename("Type de ménage" = type,
         "Nombre total de ménages (n)" = total,
         "District électoral" = NOM)

data_4_1_1_2_table_tenant <- data_4_1_1_2_table[
  data_4_1_1_2_table$`Type de ménage` == "Locataire", c(1, 3:ncol(data_4_1_1_2_table))]
names(data_4_1_1_2_table_tenant)[2] <- "Nombre de ménages locataires"

table_4_1_1_2_tenant <- data_4_1_1_2_table_tenant |> 
  gt() |> 
  data_color(
    columns = 3:ncol(data_4_1_1_2_table_tenant),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = c(0.023, 0.389), 
    ),
    direction = "row") |> 
  fmt(columns = 2, fns = convert_number) |> 
  fmt(columns = 3:ncol(data_4_1_1_2_table_tenant), fns = convert_pct) |> 
  tab_spanner(
    label = "Tranche de revenu",
    columns = c(3:ncol(data_4_1_1_2_table_tenant))
  ) |> 
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()) |> 
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()) |> 
  tab_style(
    style = cell_text(font = font_local_name, size = px(14)),
    locations = cells_column_spanners(spanners = "Tranche de revenu")
  ) |> 
  tab_options(
    table.font.size = 13,
    table.width = px(6 * 208)
  ) |> 
  cols_width(
    columns = c(1) ~ px(100),
    columns = c(3) ~ px(102),
    everything() ~ px(72)
  )

data_4_1_1_2_table_owner <- data_4_1_1_2_table[
  data_4_1_1_2_table$`Type de ménage` == "Propriétaire", c(1, 3:ncol(data_4_1_1_2_table))]
names(data_4_1_1_2_table_owner)[2] <- "Nombre de ménages propriétaires"

data_4_1_1_2_table_owner$`District électoral`[
(data_4_1_1_2_table_owner$`20 - 39 999 $ (%)` +
  data_4_1_1_2_table_owner$`40 - 59 999 $ (%)` +
  data_4_1_1_2_table_owner$`60 - 79 999 $ (%)`) > 0.4]

table_4_1_1_2_owner <- data_4_1_1_2_table_owner |> 
  gt() |> 
  data_color(
    columns = 3:ncol(data_4_1_1_2_table_owner),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = c(0, 0.51), 
    ),
    direction = "row") |> 
  fmt(columns = 2, fns = convert_number) |> 
  fmt(columns = 3:ncol(data_4_1_1_2_table_owner), fns = convert_pct) |> 
  tab_spanner(
    label = "Tranche de revenu",
    columns = c(3:ncol(data_4_1_1_2_table_owner))
  ) |> 
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()) |> 
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()) |> 
  tab_style(
    style = cell_text(font = font_local_name, size = px(14)),
    locations = cells_column_spanners(spanners = "Tranche de revenu")
  ) |> 
  tab_options(
    table.font.size = 13,
    table.width = px(6 * 208)
  ) |> 
  cols_width(
    columns = c(1) ~ px(100),
    columns = c(3) ~ px(102),
    everything() ~ px(72)
  )

# map_data_4_1_1_2_pre <- crosstab_get(mode_occupation = mode_occupation, revenu = c(medrev = "Revenu total médian ($)")) |> 
#   rename("GeoUID" = "DA_ID")
# 
# map_data_4_1_1_2 <- full_join(laval_da, map_data_4_1_1_2_pre, by = "GeoUID") |> 
#   drop_na()
# 
# #list(classInt::classIntervals(map_data_4_1_1_2$owner_medrev, n = 5, style = "fisher")$brks)
# `4_1_1_2_owner_breaks` <- c(-Inf, 21000, 83000, 107500, 137500, Inf)
# `4_1_1_2_owner_breaks_labels` <- c("< 21000", "21000 - 83000", "83000 - 107500", "107500 - 137500", "> 137500")
# #list(classInt::classIntervals(map_data_4_1_1_2$tenant_medrev, n = 5, style = "fisher")$brks)
# `4_1_1_2_tenant_breaks` <- c(-Inf, 11000, 43000, 56000, 70000, Inf)
# `4_1_1_2_tenant_breaks_labels` <- c("< 11000", "11000 - 43000", "43000 - 56000", "56000 - 70000", "> 70000")
# 
# map_data_4_1_1_2 <- map_data_4_1_1_2 |> 
#   mutate("owner_quant" = cut(owner_medrev, breaks = `4_1_1_2_owner_breaks`, include.lowest = TRUE, labels = `4_1_1_2_owner_breaks_labels`),
#          "tenant_quant" = cut(tenant_medrev, breaks = `4_1_1_2_tenant_breaks`, include.lowest = TRUE, labels = `4_1_1_2_tenant_breaks_labels`))
# 
# map_4_1_1_2_owner <- ggplot(data = map_data_4_1_1_2) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `owner_quant`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                       name = "Revenu médian des ménages propriétaires ($)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))
# 
# map_4_1_1_2_tenant <- ggplot(data = map_data_4_1_1_2) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `tenant_quant`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "Revenu médian des ménages locataires ($)") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5))

#Saving the visuals as images
ggsave_pdf_png_pdf_png(plot = plot_4_1_1_2, "outputs/4/5_statutoccupation_rev.pdf", width = 6.5, height = 3.5)
# gtsave(table_4_1_1_2_2, "outputs/4/table_4_1_1_2_2.png", vwidth = 2400)
# gtsave(table_4_1_1_2_owner, "outputs/4/table_4_1_1_2_owner.png", vwidth = 2400)
# 
# gtsave(table_4_1_1_2_tenant, "outputs/4/table_4_1_1_2_tenant.png", vwidth = 2400)
gt_save_word(table_4_1_1_2_owner, "outputs/4/4_proprietaire_rev_district.docx")
gt_save_word(table_4_1_1_2_tenant, "outputs/4/3_locataire_rev_district.docx")
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
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 18)) +
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
# ggsave_pdf_png("outputs/4/plot_4_1_2.png", plot = plot_4_1_2, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_19999.png", plot = map_4_1_2_19999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_39999.png", plot = map_4_1_2_39999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_59999.png", plot = map_4_1_2_59999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_79999.png", plot = map_4_1_2_79999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_99999.png", plot = map_4_1_2_99999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_124999.png", plot = map_4_1_2_124999, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_2_125000.png", plot = map_4_1_2_125000, width = 800/72, height = 600/72, dpi = 72)

# 4.1.1.3 Type de ménage (taille et composition) selon mode d'occupation --------
#Size cannot be cross-tabbed at the moment, to be looked at in the future
# hh_size <- c("hhsize" = "Nombre de personnes dans les ménages privés")
# 
# data_4_1_1_3_size <- crosstab_get(mode_occupation = mode_occupation, characteristic = hh_size)

composition <- c("wo_kids"= "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple sans enfants",
                 "w_kids" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : couple avec enfants",
                 "mono" = "  Ménage comptant une seule famille de recensement, sans personnes additionnelles : famille monoparentale",
                 "multi" = "Ménage multigénérationnel",
                 "solo" = "Ménage composé d'une seule personne",
                 "other" = "  Ménage sans famille de recensement, composé de deux personnes ou plus")

data_4_1_1_3_comp <- crosstab_get(mode_occupation = mode_occupation, composition = composition, scale = "CSD") |> 
  select(-CSD_ID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "count") |> 
  mutate(comp = case_when(
    str_detect(type, "wo_kids") ~ "Couple sans enfants",
    str_detect(type, "w_kids") ~ "Couple avec enfants",
    str_detect(type, "mono") ~ "Famille mono-parentale",
    str_detect(type, "multi") ~ "Ménage multi-générationnel",
    str_detect(type, "solo") ~ "Personne seule",
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
      "Couple sans enfants",
      "Couple avec enfants",
      "Famille mono-parentale",
      "Ménage multi-générationnel",
      "Personne seule",
      "Deux personnes ou plus"))) |> 
  group_by(type) |> 
  mutate(pct = convert_pct(count / sum(count)))

plot_4_1_1_3_comp <- ggplot(data_4_1_1_3_comp, aes(x = comp, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = data_4_1_1_3_comp[-c(4, 6, 10, 12), ], aes(label = pct), position = position_dodge(width = 0.9),
            vjust = 2, size = 2.5, color = "white") +
  geom_text(data = data_4_1_1_3_comp[c(4, 6, 10, 12), ], aes(label = pct), position = position_dodge(width = 0.9),
            vjust = -1.5, size = 2.5, color = "black") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 8, whitespace_only = FALSE)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  xlab(NULL) +
  ylab("Nombre de ménages")

# table_data_4_1_1_3_comp <- crosstab_get(mode_occupation = mode_occupation, composition = composition) |> 
#   select(-DA_ID) |> 
#   summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
#   rename_with(~ gsub("^owner_", "Propriétaire_", .x), starts_with("owner_")) |>  # Rename owner_ to Propriétaire_
#   rename_with(~ gsub("^tenant_", "Locataire_", .x), starts_with("tenant_")) |> 
#   pivot_longer(
#     cols = starts_with("Propriétaire_") | starts_with("Locataire_"),
#     names_to = c("Type de ménage", ".value"),
#     names_pattern = "(Propriétaire|Locataire)_(.*)") |> 
#   mutate(total = wo_kids + w_kids + mono + multi + solo + other) |> 
#   mutate("Couple sans enfants (n)" = wo_kids,
#          "Couple sans enfants (%)" = wo_kids / sum(wo_kids, na.rm = TRUE),
#          "Couple avec enfants (n)" = w_kids,
#          "Couple avec enfants (%)" = w_kids / sum(w_kids, na.rm = TRUE),
#          "Famille monoparentale (n)" = mono,
#          "Famille monoparentale (%)" = mono / sum(mono, na.rm = TRUE),
#          "Ménage multigénérationnel (n)" = multi,
#          "Ménage multigénérationnel (%)" = multi / sum(multi, na.rm = TRUE),
#          "Personne seule (n)" = solo,
#          "Personne seule (%)" = solo / sum(solo, na.rm = TRUE),
#          "Deux personnes ou plus (n)" = other,
#          "Deux personnes ou plus (%)" = other / sum(other, na.rm = TRUE)) |> 
#   select(-total, -wo_kids, -w_kids, -mono, -multi, -solo, -other)
# 
# table_4_1_1_3_comp <- table_data_4_1_1_3_comp |> 
#   gt() |>
#   cols_label(
#     `Couple sans enfants (n)` = html("<div style='width:95px;'>Couple sans enfants (n)</div>"),
#     `Couple sans enfants (%)` = html("<div style='width:95px;'>Couple sans enfants (%)</div>"),
#     `Couple avec enfants (n)` = html("<div style='width:95px;'>Couple avec enfants (n)</div>"),
#     `Couple avec enfants (%)` = html("<div style='width:95px;'>Couple avec enfants (%)</div>"),
#     `Deux personnes ou plus (n)` = html("<div style='width:100px;'>Deux personnes ou plus (n)</div>"),
#     `Deux personnes ou plus (%)` = html("<div style='width:100px;'>Deux personnes ou plus (%)</div>")
#   ) |> 
#   data_color(
#     columns = c(3,5,7,9,11,13),
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |> 
#   fmt(columns = c(2,4,6,8,10,12), fns = convert_number) |> 
#   fmt(columns = c(3,5,7,9,11,13), fns = convert_pct) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(15)),
#     locations = cells_column_labels())
    

table_data_4_1_1_3_comp_ed_tenant <- crosstab_get(mode_occupation = mode_occupation, composition = composition, scale = "CT") |> 
  rename("GeoUID" = "CT_ID") |> 
  full_join(laval_ct_hou, by = "GeoUID") |> 
  st_as_sf() |> 
  st_transform(crs = 32618) |> 
  interpolate(additive_vars = c("Households", "owner_wo_kids", "owner_w_kids", "owner_mono", "owner_multi", "owner_solo", "owner_other",
                                "tenant_wo_kids", "tenant_w_kids", "tenant_mono", "tenant_multi", "tenant_solo", "tenant_other")) |> 
  mutate(across(c("Households", "owner_wo_kids", "owner_w_kids", "owner_mono", "owner_multi", "owner_solo", "owner_other",
                  "tenant_wo_kids", "tenant_w_kids", "tenant_mono", "tenant_multi", "tenant_solo", "tenant_other"), round)) |> 
  select(-ID) |>
  mutate(wo_kids = tenant_wo_kids,
         w_kids = tenant_w_kids,
         mono = tenant_mono,
         multi = tenant_multi,
         solo = tenant_solo,
         other = tenant_other) |> 
  mutate(total = wo_kids + w_kids + mono + multi + solo + other) |> 
  mutate("Couple sans enfants (n)" = wo_kids,
         "Couple sans enfants (%)" = wo_kids / total,
         "Couple avec enfants (n)" = w_kids,
         "Couple avec enfants (%)" = w_kids / total,
         "Famille monoparentale (n)" = mono,
         "Famille monoparentale (%)" = mono / total,
         "Ménage multigénérationnel (n)" = multi,
         "Ménage multigénérationnel (%)" = multi / total,
         "Personne seule (n)" = solo,
         "Personne seule (%)" = solo / total,
         "Deux personnes ou plus (n)" = other,
         "Deux personnes ou plus (%)" = other / total) |> 
  # select(NOM, "Couple sans enfants (n)", "Couple sans enfants (%)",
  #        "Couple avec enfants (n)", "Couple avec enfants (%)", "Famille monoparentale (n)", "Famille monoparentale (%)", 
  #        "Ménage multigénérationnel (n)", "Ménage multigénérationnel (%)", "Personne seule (n)",
  #        "Personne seule (%)","Deux personnes ou plus (n)", "Deux personnes ou plus (%)") |> 
  transmute(`District électoral` = NOM, `Nombre de ménages` = `Households`, `Couple sans enfants (%)`, `Couple avec enfants (%)`, `Famille monoparentale (%)`, 
            `Ménage multigénérationnel (%)`, `Personne seule (%)`, `Deux personnes ou plus (%)`)

table_data_4_1_1_3_comp_ed_owner <- crosstab_get(mode_occupation = mode_occupation, composition = composition, scale = "CT") |> 
  rename("GeoUID" = "CT_ID") |> 
  full_join(laval_ct_hou, by = "GeoUID") |> 
  st_as_sf() |> 
  st_transform(crs = 32618) |> 
  interpolate(additive_vars = c("Households", "owner_wo_kids", "owner_w_kids", "owner_mono", "owner_multi", "owner_solo", "owner_other",
                                "tenant_wo_kids", "tenant_w_kids", "tenant_mono", "tenant_multi", "tenant_solo", "tenant_other")) |> 
  mutate(across(c("Households", "owner_wo_kids", "owner_w_kids", "owner_mono", "owner_multi", "owner_solo", "owner_other",
                  "tenant_wo_kids", "tenant_w_kids", "tenant_mono", "tenant_multi", "tenant_solo", "tenant_other"), round)) |> 
  select(-ID) |>
  mutate(wo_kids = owner_wo_kids,
         w_kids = owner_w_kids,
         mono = owner_mono,
         multi = owner_multi,
         solo = owner_solo,
         other = owner_other) |> 
  mutate(total = wo_kids + w_kids + mono + multi + solo + other) |> 
  mutate("Couple sans enfants (n)" = wo_kids,
         "Couple sans enfants (%)" = wo_kids / total,
         "Couple avec enfants (n)" = w_kids,
         "Couple avec enfants (%)" = w_kids / total,
         "Famille monoparentale (n)" = mono,
         "Famille monoparentale (%)" = mono / total,
         "Ménage multigénérationnel (n)" = multi,
         "Ménage multigénérationnel (%)" = multi / total,
         "Personne seule (n)" = solo,
         "Personne seule (%)" = solo / total,
         "Deux personnes ou plus (n)" = other,
         "Deux personnes ou plus (%)" = other / total) |> 
  # select(NOM, "Couple sans enfants (n)", "Couple sans enfants (%)",
  #        "Couple avec enfants (n)", "Couple avec enfants (%)", "Famille monoparentale (n)", "Famille monoparentale (%)", 
  #        "Ménage multigénérationnel (n)", "Ménage multigénérationnel (%)", "Personne seule (n)",
  #        "Personne seule (%)","Deux personnes ou plus (n)", "Deux personnes ou plus (%)") |> 
  transmute(`District électoral` = NOM, `Nombre de ménages` = `Households`, `Couple sans enfants (%)`, `Couple avec enfants (%)`, `Famille monoparentale (%)`, 
            `Ménage multigénérationnel (%)`, `Personne seule (%)`, `Deux personnes ou plus (%)`)

table_data_4_1_1_3_comp_ed_tenant <- 
  table_data_4_1_1_3_comp_ed_tenant |> 
  pivot_longer(cols = c("Couple sans enfants (%)", 
                        "Couple avec enfants (%)", 
                        "Famille monoparentale (%)", 
                        "Ménage multigénérationnel (%)", 
                        "Personne seule (%)", 
                        "Deux personnes ou plus (%)"), 
               names_to = "Type_ménage", 
               values_to = "Pourcentage")
table_data_4_1_1_3_comp_ed_tenant$Type_ménage <- 
  gsub(" \\(\\%\\)", "", table_data_4_1_1_3_comp_ed_tenant$Type_ménage)

table_data_4_1_1_3_comp_ed_owner <- 
  table_data_4_1_1_3_comp_ed_owner |> 
  pivot_longer(cols = c("Couple sans enfants (%)", 
                        "Couple avec enfants (%)", 
                        "Famille monoparentale (%)", 
                        "Ménage multigénérationnel (%)", 
                        "Personne seule (%)", 
                        "Deux personnes ou plus (%)"), 
               names_to = "Type_ménage", 
               values_to = "Pourcentage")
table_data_4_1_1_3_comp_ed_owner$Type_ménage <- 
  gsub(" \\(\\%\\)", "", table_data_4_1_1_3_comp_ed_owner$Type_ménage)

plot_4_1_1_3_tenant <-
ggplot(table_data_4_1_1_3_comp_ed_tenant) +
  geom_sf(aes(geometry = geometry, fill = Pourcentage)) +  
  facet_wrap(~ Type_ménage) +
  scale_fill_gradientn(colors = c("#FFFFFF", "#F5D574", "#C9C3FA", "#CD718C"),
                       labels = convert_pct, trans = "sqrt",
                       limits = c(0, 0.5),
                       oob = scales::squish) +
  labs(title = NULL, fill = "Proportion de ménages locataires") +
  gg_cc_theme +
  theme(legend.key.width = unit(3, "cm"),
        legend.title.position = "top")

plot_4_1_1_3_owner <- ggplot(table_data_4_1_1_3_comp_ed_owner) +
  geom_sf(aes(geometry = geometry, fill = Pourcentage)) +  
  facet_wrap(~ Type_ménage) +
  scale_fill_gradientn(colors = c("#FFFFFF", "#F5D574", "#C9C3FA", "#CD718C"),
                       labels = convert_pct, trans = "sqrt",
                       limits = c(0, 0.5),
                       oob = scales::squish) +
  labs(title = NULL, fill = "Proportion de ménages propriétaires") +
  gg_cc_theme +
  theme(legend.key.width = unit(3, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(plot = plot_4_1_1_3_tenant, "outputs/4/7_carte_composition_locataire.pdf", width = 6.5, height =5)
ggsave_pdf_png(plot = plot_4_1_1_3_owner, "outputs/4/8_carte_composition_proprietaire.pdf", width = 6.5, height = 5)



  
# table_4_1_1_3_comp_ed <- table_data_4_1_1_3_comp_ed |> 
#   gt() |> 
#   tab_style(
#     style = cell_fill(color = "#f0f0f0"),
#     locations = cells_body(
#       rows = seq(2, nrow(table_data_4_1_1_3_comp_ed), by = 2)
#     )) |> 
#   data_color(
#     columns = c(3:8),
#     colors = scales::col_numeric(
#       palette = c("transparent", color_theme("purpletransport")),
#       domain = c(0.009, .45)
#     ),
#     direction = "row") |> 
#   fmt(columns = c(3:8), fns = convert_pct) |> 
#   fmt(columns = 2, fns = convert_number_tens) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(14)),
#     locations = cells_column_labels()) |> 
#   tab_options(
#     table.width = px(6 * 252)
#   )

ggsave_pdf_png(plot = plot_4_1_1_3_comp, "outputs/4/6_composition_statutocc.pdf", width = 6.5, height = 3)
# gtsave(table_4_1_1_3_comp, "outputs/4/table_4_1_1_3_comp.png", vwidth = 1600)
# gtsave(table_4_1_1_3_comp_ed, "outputs/4/table_4_1_1_3_comp_ed.png", vwidth = 1600)

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
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 18)) +
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
#                     name = "Nombre de ménages (une Personne seule) (n)") +
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
# ggsave_pdf_png("outputs/4/plot_4_1_3.png", plot = plot_4_1_3, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_wochild.png", plot = map_4_1_3_wochild, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_child.png", plot = map_4_1_3_child, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_single.png", plot = map_4_1_3_single, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_multigen.png", plot = map_4_1_3_multigen, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_other.png", plot = map_4_1_3_other, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_sole.png", plot = map_4_1_3_sole, width = 800/72, height = 600/72, dpi = 72)
# ggsave_pdf_png("outputs/4/map_4_1_3_noncf.png", plot = map_4_1_3_noncf, width = 800/72, height = 600/72, dpi = 72)

# 4.1.1.4 Statut d'immigrant selon mode d'occupation ----------------------
#Percentages based on provided PDF
#https://observatoire.cmm.qc.ca/wp-content/uploads/2022/05/CMM_10e_Cahier_metropolitain_web.pdf

# df_4_1_1_4 <- data.frame(
#   Statut = c("Autochtone", "Autochtone", "Immigrant avant 2006",
#              "Immigrant avant 2006", "Immigrant de 2006 à 2016",
#              "Immigrant de 2006 à 2016", "Résident non permanent",
#              "Résident non permanent", "Non-immigrant / non-autochtone",
#              "Non-immigrant / non-autochtone"),
#   Type = c("Propriétaire", "Locataire", "Propriétaire", "Locataire",
#                        "Propriétaire", "Locataire", "Propriétaire", "Locataire",
#                        "Propriétaire", "Locataire"),
#   Pourcentage = c(0.21, 0.79, 0.51, 0.49, 0.17, 0.83, 0.07, 0.93, 0.42, 0.58)) |> 
#   mutate(prop = convert_pct(Pourcentage),
#          Statut = factor(Statut, levels = c("Autochtone", "Immigrant avant 2006",
#                                             "Immigrant de 2006 à 2016", "Résident non permanent",
#                                             "Non-immigrant / non-autochtone")),
#          Type = factor(Type, levels = c("Propriétaire", "Locataire")))
# 
# plot_4_1_1_4 <- ggplot(df_4_1_1_4, aes(x = Statut, y = Pourcentage, fill = Type)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_text(aes(label = prop), position = position_dodge(width = 0.9),
#             vjust = 2, size = 3, color = "white") +
#   scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
#   scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 18)) +
#   scale_y_continuous(labels = function(x) convert_pct(x)) +
#   labs(x = NULL, 
#        y = "Proportion des ménages") +
#   graph_theme

# Créer les données du graphique
data <- data.frame(
  Groupe = c("Autochtone", "Immigrant avant\n 2006", "Immigrant de\n 2006 à 2016", 
             "Résident non\n permanent", "Non-immigrant/\nnon-autochtone"),
  "Locataires en logement subventionné" = c(4, 2, 3, 2, 2),
  "Locataires en logement non subventionné" = c(53, 19, 51, 71, 30),
  "Propriétaires en copropriété" = c(5, 9, 8, 9, 11),
  "Propriétaires d'un logement en propriété absolue" = c(39, 71, 38, 18, 57)
)

# Transformer les données au format long pour ggplot
data_long <- pivot_longer(data, cols = -Groupe, names_to = "Statut", values_to = "Pourcentage")
data_long$Statut <- gsub("\\.", " ", data_long$Statut)
data_long$Statut <- gsub("d un", "d'un", data_long$Statut)
data_long$Groupe <- factor(data_long$Groupe, levels = c("Autochtone", "Immigrant avant\n 2006", "Immigrant de\n 2006 à 2016", 
                                 "Résident non\n permanent", "Non-immigrant/\nnon-autochtone"))

# Créer le graphique avec les bonnes couleurs et un empilement correct
plot_4_1_1_4 <- 
  ggplot(data_long, aes(x = Groupe, y = Pourcentage, fill = Statut)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = c("Locataires en logement subventionné" = "#CD718C99", 
                               "Locataires en logement non subventionné" = "#CD718C", 
                               "Propriétaires en copropriété" = "#A3B0D199", 
                               "Propriétaires d'un logement en propriété absolue" = "#A3B0D1"),
                    guide = guide_legend(nrow = 2),
                    labels = \(x) str_wrap(x, 36)) +
  scale_y_continuous(labels = percent) +
  labs(title = NULL, x = NULL, y = "Proportion des ménages", fill = NULL) +
  graph_theme +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))  # Ajuster le texte de l'axe des x

ggsave_pdf_png(plot = plot_4_1_1_4, "outputs/4/9_statutimmigration_statutoccupation.pdf", width = 6.5, height = 3.5)

# 4.1.1.5 Âge du principal soutien du ménage selon mote d'occupation------------
#https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=9810023201&pickMembers%5B0%5D=1.1730&pickMembers%5B1%5D=2.1&pickMembers%5B2%5D=3.1&pickMembers%5B3%5D=4.1

data_4_1_1_5 <- read.csv("data/4/4_1_1_5.csv", skip = 12) |> 
  slice(-c(1, 11:24)) |> 
  rename("Age" = "Tenure..4..4") |> 
  select(Age, Owner, Renter, "Total...Tenure") |> 
  mutate(Age = replace(Age, 1, "Total")) |> 
  mutate(across(c(Owner, Renter, `Total...Tenure`), ~ as.numeric(gsub(",", "", .)))) |> 
  mutate(Prop_Own = Owner / `Total...Tenure`,  
         Prop_Ten = Renter / `Total...Tenure`) |> 
  select(-`Total...Tenure`) |> 
  pivot_longer(cols = c(Owner, Renter),
               names_to = "Type",
               values_to = "Value") |> 
  mutate(Prop = ifelse(Type == "Owner", Prop_Own, Prop_Ten)) |> 
  select(Age, Type, Value, Prop) |> 
  slice(-c(1,2)) |> 
  mutate(Type = ifelse(Type == "Owner", "Propriétaire", "Locataire")) |> 
  mutate(Age = case_when(
    Age == "15 to 24 years" ~ "15 à 24 ans",
    Age == "25 to 34 years" ~ "25 à 34 ans",
    Age == "35 to 44 years" ~ "35 à 44 ans",
    Age == "45 to 54 years" ~ "45 à 54 ans",
    Age == "55 to 64 years" ~ "55 à 64 ans",
    Age == "65 to 74 years" ~ "65 à 74 ans",
    Age == "75 to 84 years" ~ "75 à 84 ans",
    Age == "85 years and over" ~ "85 ans et plus",
    TRUE ~ Age)) |> 
  mutate(prop = convert_pct(Prop))

renter_15 <- data_4_1_1_5 |> filter(Age == "15 à 24 ans" & Type == "Locataire") |> pull(prop)
renter_25 <- data_4_1_1_5 |> filter(Age == "25 à 34 ans" & Type == "Locataire") |> pull(prop)
owner_45 <- data_4_1_1_5 |> filter(Age == "45 à 54 ans" & Type == "Propriétaire") |> pull(prop)

plot_4_1_1_5 <- ggplot(data_4_1_1_5, aes(x = Age, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(data = data_4_1_1_5[!data_4_1_1_5$Age %in% c("15 à 24 ans", "85 ans et plus"), ], 
            aes(label = prop), position = position_dodge(width = 0.9),
            vjust = 2, size = 2.5, color = "white") +
  geom_text(data = data_4_1_1_5[data_4_1_1_5$Age %in% c("15 à 24 ans", "85 ans et plus"), ], 
            aes(label = prop), position = position_dodge(width = 0.9),
            vjust = -1.5, size = 2.5, color = "black") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = NULL, 
       y = "Nombre de ménages") +
  graph_theme

ggsave_pdf_png(plot = plot_4_1_1_5, "outputs/4/10_age_statutoccupation.pdf", width = 6.5, height = 4)

# 4.1.1.6 Typologie (unifamilial, multi logement, etc.) selon mode d'occupation----
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=9810024001
#Télécharger les données sélectionnées (pour le chargement de la base de données)

data_4_1_1_6 <- read.csv("data/4/4_1_1_6.csv", sep = ";") |> 
  select("Type.de.construction.résidentielle..10.", "Mode.d.occupation..4.", "VALEUR") |> 
  rename(Build = "Type.de.construction.résidentielle..10.",
         Type = "Mode.d.occupation..4.",
         Value = "VALEUR") |> 
  filter(Build != "Autre logement attenant") |> 
  pivot_wider(names_from = Type, values_from = Value) |>
  rename("Total" = "Total - Mode d’occupation") |> 
  mutate(across(c(Propriétaire, Locataire, `Total`), ~ as.numeric(gsub(",", "", .)))) |> 
  mutate(Prop_Own = Propriétaire / sum(Propriétaire),
         Prop_Ten = Locataire / sum(Locataire)) |> 
  select(-Total) |> 
  mutate(Build = case_when(Build == "Total – Type de construction résidentielle" ~ "Total",
                           TRUE ~ Build)) |> 
  pivot_longer(cols = c(Propriétaire, Locataire),
               names_to = "Type",
               values_to = "Value") |> 
  mutate(Prop = ifelse(Type == "Propriétaire", Prop_Own, Prop_Ten)) |> 
  select(Build, Type, Value, Prop) |> 
  filter(Build != "Total") |> 
  mutate(Prop = convert_pct(Prop)) |> 
  mutate(Build = factor(Build, levels = c("Maison individuelle non attenante", "Appartement dans un immeuble de cinq étages ou plus",
                                          "Appartement ou plain-pied dans un duplex", "Appartement dans un immeuble de moins de cinq étages",
                                          "Autre maison individuelle attenante", "Maison en rangée", "Maison jumelée", "Logement mobile")),
         Type = factor(Type, levels = c("Propriétaire", "Locataire")))

plot_4_1_1_6 <- ggplot(data_4_1_1_6, aes(x = Build, y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Prop), position = position_dodge(width = 0.9),
            vjust = -0.5, size = 2.5, color = "black") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = NULL, 
       y = "Nombre de ménages") +
  graph_theme

# data_4_1_1_6_table <- read.csv("data/4/4_1_1_6.csv", sep = ";") |> 
#   select("Type.de.construction.résidentielle..10.", "Mode.d.occupation..4.", "VALEUR") |> 
#   rename(Build = "Type.de.construction.résidentielle..10.",
#          Type = "Mode.d.occupation..4.",
#          Value = "VALEUR") |> 
#   filter(Build != "Autre logement attenant") |> 
#   pivot_wider(names_from = Type, values_from = Value) |>
#   rename("Total" = "Total - Mode d’occupation") |> 
#   mutate(across(c(Propriétaire, Locataire, `Total`), ~ as.numeric(gsub(",", "", .)))) |> 
#   pivot_longer(cols = c(Propriétaire, Locataire, Total),
#                names_to = "Measure",
#                values_to = "Value") |> 
#   pivot_wider(names_from = Build, values_from = Value) |> 
#   select(Measure, everything()) |> 
#   rename("Total" = "Total – Type de construction résidentielle") |> 
#   mutate(`Maison individuelle non attenante (n)` = `Maison individuelle non attenante`,
#          `Maison individuelle non attenante (%)` = `Maison individuelle non attenante` / Total,
#          `Appartement dans un immeuble de cinq étages ou plus (n)` = `Appartement dans un immeuble de cinq étages ou plus`,
#          `Appartement dans un immeuble de cinq étages ou plus (%)` = `Appartement dans un immeuble de cinq étages ou plus` / Total,
#          `Appartement ou plain-pied dans un duplex (n)` = `Appartement ou plain-pied dans un duplex`,
#          `Appartement ou plain-pied dans un duplex (%)` = `Appartement ou plain-pied dans un duplex` / Total,
#          `Appartement dans un immeuble de moins de cinq étages (n)` = `Appartement dans un immeuble de moins de cinq étages`,
#          `Appartement dans un immeuble de moins de cinq étages (%)` = `Appartement dans un immeuble de moins de cinq étages` / Total,
#          `Autre maison individuelle attenante (n)` = `Autre maison individuelle attenante`,
#          `Autre maison individuelle attenante (%)` = `Autre maison individuelle attenante` / Total,
#          `Maison en rangée (n)` = `Maison en rangée`,
#          `Maison en rangée (%)` = `Maison en rangée` / Total,
#          `Maison jumelée (n)` = `Maison jumelée`,
#          `Maison jumelée (%)` = `Maison jumelée` / Total,
#          `Logement mobile (n)` = `Logement mobile`,
#          `Logement mobile (%)` = `Logement mobile` / Total) |> 
#   select(Measure, `Maison individuelle non attenante (n)`, `Maison individuelle non attenante (%)`,
#     `Appartement dans un immeuble de cinq étages ou plus (n)`, `Appartement dans un immeuble de cinq étages ou plus (%)`,
#     `Appartement ou plain-pied dans un duplex (n)`, `Appartement ou plain-pied dans un duplex (%)`,
#     `Appartement dans un immeuble de moins de cinq étages (n)`, `Appartement dans un immeuble de moins de cinq étages (%)`,
#     `Autre maison individuelle attenante (n)`, `Autre maison individuelle attenante (%)`,
#     `Maison en rangée (n)`, `Maison en rangée (%)`, `Maison jumelée (n)`, `Maison jumelée (%)`,
#     `Logement mobile (n)`, `Logement mobile (%)`
#   ) |> 
#   rename("Type de ménage" = Measure)
# 
# table_4_1_1_6 <- data_4_1_1_6_table |> 
#   gt() |> 
#   tab_spanner(
#     label = "Typologie de logements",
#     columns = c(2:17)
#   ) |>
#   data_color(
#     columns = c(3, 5, 7, 9, 11, 13, 15, 17),
#     colors = scales::col_numeric(
#       palette = c("transparent", color_theme("purpletransport")),
#       domain = NULL
#     )) |> 
#   fmt(columns = c(2, 4, 6, 8, 10, 12, 14, 16), fns = convert_number) |> 
#   fmt(columns = c(3, 5, 7, 9, 11, 13, 15, 17), fns = convert_pct) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |> 
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(14)),
#     locations = cells_column_labels()) |> 
#   tab_options(
#     table.width = px(6 * 282)
#   )

# sfh_laval <- data_4_1_1_6_table |> filter("Type de ménage" == "Total") |> pull(`Maison individuelle non attenante (%)`)
# apt_laval <- data_4_1_1_6_table |> filter("Type de ménage" == "Total") |> pull(`Appartement dans un immeuble de moins de cinq étages (%)`)
# sfh_owner <- data_4_1_1_6_table |> filter("Type de ménage" == "Propriétaire") |> pull(`Maison individuelle non attenante (%)`)

ggsave_pdf_png(plot = plot_4_1_1_6, "outputs/4/11_type_statutoccupation.pdf", width = 7, height = 3.5)
# gtsave(table_4_1_1_6, "outputs/4/table_4_1_1_6.png", vwidth = 2400)

# 4.1.2.1 Projection des ménages X-2046 (PRÉCISER JALONS, C17) -----------------
#https://statistique.quebec.ca/fr/document/projections-de-menages-regions-administratives-et-regions-metropolitaines-rmr

projection_4_1_2_1 <- read_excel("data/4/Ménages_Total_RA_base_2024.xlsx", 
                                 skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |>
  select(4:34) |> 
  mutate(`2021` = as.numeric(`2021`))

census16_4_1_2_1 <- get_census(dataset = "CA16",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = c(`2016` = "v_CA16_418")) |> 
  select(`2016`)

data_4_1_2_1 <- get_census(dataset = "CA11",
                               regions = list(CSD = 2465005),
                               level = "CSD",
                               vectors = c(`2011` = "v_CA11F_182")) |> 
  select(`2011`) |> 
  bind_cols(census16_4_1_2_1, projection_4_1_2_1) |> 
  pivot_longer(cols = everything(), names_to = "Year", values_to = "Households") |> 
  mutate(Year = as.character(Year))

all_years <- data.frame(Year = as.character(2011:2051))

data_4_1_2_1_complete <- all_years %>%
  left_join(data_4_1_2_1, by = "Year") |> 
  mutate(Households = ifelse(is.na(Households), NA, Households))

year_2011 <- data_4_1_2_1_complete |> filter(Year == "2011") |> pull(Households)
year_2021 <- data_4_1_2_1_complete |> filter(Year == "2021") |> pull(Households)
year_2041 <- data_4_1_2_1_complete |> filter(Year == "2041") |> pull(Households)
year_2029 <- data_4_1_2_1_complete |> filter(Year == "2029") |> pull(Households) |> convert_number()
year_2033 <- data_4_1_2_1_complete |> filter(Year == "2033") |> pull(Households) |> convert_number()
year_2037 <- data_4_1_2_1_complete |> filter(Year == "2037") |> pull(Households) |> convert_number()
year_2046 <- data_4_1_2_1_complete |> filter(Year == "2046") |> pull(Households) |> convert_number()
year_2051 <- data_4_1_2_1_complete |> filter(Year == "2051") |> pull(Households) |> convert_number()

diff_2011_2021 <- convert_pct((year_2021-year_2011)/year_2011)
diff_2021_2041 <- convert_pct((year_2041-year_2021)/year_2021)

year_2011 <- convert_number_noround(year_2011)
year_2021 <- convert_number_noround(year_2021)
year_2041 <- convert_number_noround(year_2041)

data_4_1_2_1_complete$Year <- as.numeric(data_4_1_2_1_complete$Year)

# Create a named vector for custom x-axis labels
custom_labels <- c(
  "2010" = "2010", 
  "2020" = "2020", 
  "2029" = "2029", 
  "2033" = "2033", 
  "2037" = "2037", 
  "2040" = "2040",
  "2050" = "2050"
)

plot_4_1_2_1 <-
  ggplot(data_4_1_2_1_complete, aes(x = Year, y = Households)) +
  geom_line(data = data_4_1_2_1_complete %>% filter(!is.na(Households) & Year <= 2021), 
            aes(group = 1, linetype = "Ménages actuels"),
            linewidth = 1.5, 
            na.rm = TRUE) +
  geom_line(data = data_4_1_2_1_complete %>% filter(!is.na(Households) & Year >= 2021), 
            aes(group = 1, linetype = "Ménages projetés"),
            linewidth = 1.5, 
            na.rm = TRUE) +
  geom_vline(xintercept = 2029, color = color_theme("redhousing"), size = 1) +
  geom_vline(xintercept = 2033, color = color_theme("redhousing"), size = 1) +
  geom_vline(xintercept = 2037, color = color_theme("redhousing"), size = 1) +
  scale_x_continuous(breaks = c(2010, 2020, 2029, 2033, 2037, 2040, 2050),
                     limits = c(2010, 2050)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages", linetype = "") +
  graph_theme +
  theme(
    axis.text.x = element_text(color = ifelse(custom_labels %in% c("2029", "2033", "2037"), 
                                              color_theme("redhousing"), "black"))
  )

ggsave_pdf_png(plot = plot_4_1_2_1, "outputs/4/12_projections_2051.pdf", width = 6.5, height = 3)





menage_age <- read_excel("data/4/Ménages_GrAge_RA_base_2024.xlsx", skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |> 
  rename(`Année` = `...4`) |> 
  mutate_all(as.numeric)


# Reshape the data from wide to long format
projection_long <- pivot_longer(menage_age, cols = `15-19`:`90+`, names_to = "Groupe d'âge")
projection_long <- projection_long[projection_long$Année %in% c(2021, 2051), ]
projection_long$`Groupe d'âge` <- factor(projection_long$`Groupe d'âge`,
                                         levels = unique(projection_long$`Groupe d'âge`))

# Create the bar plot using ggplot2
plot_4_1_2_1_hou_age <-
  ggplot(projection_long, aes(x = `Groupe d'âge`, y = value, fill = as.factor(Année))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL, 
       x = NULL, 
       y = "Nombre de ménages") +
  scale_y_continuous(labels = convert_number) +
  scale_fill_manual(values = c("2021" = color_theme("yellowclimate"), "2051" = color_theme("pinkhealth")))+
  graph_theme

ggsave_pdf_png(plot = plot_4_1_2_1_hou_age, "outputs/4/13_projections_menages.pdf", 
               width = 9, height = 6)






# 4.1.2.3 Projections population/ménages selon groupe d'âge ----------------
#https://statistique.quebec.ca/fr/document/projections-de-population-mrc-municipalites-regionales-de-comte
projection_4_1_2_3_1 <- read_excel("data/4/PopGrAS_RA_base_2024.xlsx", skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |> 
  rename(`Année` = `...4`) |> 
  mutate_all(as.numeric)


# Reshape the data from wide to long format
projection_long_raw <- pivot_longer(projection_4_1_2_3_1, cols = `0-4`:`100+`, names_to = "Groupe d'âge")
projection_long <- projection_long_raw[projection_long_raw$Année %in% c(2021, 2051), ]
projection_long$`Groupe d'âge` <- factor(projection_long$`Groupe d'âge`,
                                         levels = unique(projection_long$`Groupe d'âge`))

# Create the bar plot using ggplot2
plot_4_1_2_3_1 <-
  ggplot(projection_long, aes(x = `Groupe d'âge`, y = value, fill = as.factor(Année))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = NULL, 
       x = NULL, 
       y = "Nombre d'individus") +
  scale_y_continuous(labels = convert_number) +
  scale_fill_manual(values = c("2021" = color_theme("yellowclimate"), "2051" = color_theme("pinkhealth")))+
  graph_theme

ggsave_pdf_png(plot = plot_4_1_2_3_1, "outputs/4/14_projections_individus.pdf", 
               width = 9, height = 6)

growth_total <- 
  sum(projection_long_raw$value[projection_long_raw$Année == 2041]) - sum(projection_long_raw$value[projection_long_raw$Année == 2021])
sum_2021 <- 
  sum(projection_long_raw$value[projection_long_raw$Année == 2021 & 
                              projection_long_raw$`Groupe d'âge` %in% c("65-69", "70-74", "75-79", "80-84", "85+")])

sum_2041 <- 
  sum(projection_long_raw$value[projection_long_raw$Année == 2041 & 
                              projection_long_raw$`Groupe d'âge` %in% c("65-69", "70-74", "75-79", "80-84", "85+")])
growth_65p <- sum_2041 - sum_2021

growth_attributed_to_65p <- convert_pct(growth_65p / growth_total)


# table_4_1_2_3_1 <- projection_4_1_2_3_1 |> 
#   gt() |>
#   data_color(
#     columns = c(23),
#     colors = scales::col_numeric(
#       palette = c("white", color_theme("purpletransport")),
#       domain = NULL
#     )
#   ) |>
#   fmt(columns = c(2:22), fns = convert_number) |>
#   fmt(columns = c(23), fns = convert_pct) |>
#   tab_spanner(
#     label = "Année",
#     columns = c(2:22)
#   ) |>
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(13)),
#     locations = cells_body()) |>
#   tab_style(
#     style = cell_text(font = font_local_name, size = px(14)),
#     locations = cells_column_labels()) |>
#   tab_style(
#     style = cell_text(size = px(15)),
#     locations = cells_column_spanners(spanners = "Année")
#   ) |>
#   tab_options(
#     table.font.size = 13,
#     table.width = px(6 * 224))

pop_growth <- (sum(projection_long_raw$value[projection_long_raw$Année == 2041]) - sum(projection_long_raw$value[projection_long_raw$Année == 2021])) / sum(projection_long_raw$value[projection_long_raw$Année == 2021])
pop_growth <- convert_pct(pop_growth)

# change_85 <- projection_4_1_2_3_1 |> filter(`Groupe d'âge` == "85+") |> pull(`Changement`) |> convert_pct()
# change_84 <- projection_4_1_2_3_1 |> filter(`Groupe d'âge` == "80-84") |> pull(`Changement`) |> convert_pct()
# change_59 <- projection_4_1_2_3_1 |> filter(`Groupe d'âge` == "55-59") |> pull(`Changement`) |> convert_pct()

# gtsave(table_4_1_2_3_1, "outputs/4/table_4_1_2_3_1.png", vwidth = 2400)

# 4.1.2.3.2 Composantes de la croissance démographique annuelle  --------
#https://statistique.quebec.ca/fr/document/projections-de-population-regions-administratives-et-regions-metropolitaines-rmr
sheet_4_1_2_3_2 <- read_excel("data/4/Compo_RA_base_2024.xlsx", skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |> 
  rename(`Année` = 4) |> 
  mutate_all(as.numeric)

# data_4_1_2_3_2 <- sheet_4_1_2_3_2 |> #Separate data frame so loading from the spreadsheet constantly isn't needed
#   rename(`Année` = `...4`, `Population (n)` = `...5`, `Naissances (n)` = `...7`,
#          `Indice synthétique de fécondité` = `...8`, `Décès (n)` = `...10`,
#          `Espérance de vie - Hommes` = `...11`,
#          `Espérance de vie - Femmes` = `...12`,
#          `Espérance de vie - Total` = `...13`,
#          `Accroissement naturel (n)` = `...15`, `Immigrants (n)` = `...17`,
#          `Émigration (n)` = `...18`, `Solde migratoire international (n)` = `...19`,
#          `Cette année (n)` = `...21`, `L'année prochaine (n)` = `...22`,
#          `Solde résidents non permanents (n)` = `...23`, `Entrants (n)` = `...25`,
#          `Sortants (n)` = `...26`, `Solde migration interprovinciale (n)` = `...27`, `Solde (n)` = `...31`,
#          `Solde interne des naissances (n)` = `...32`, `Solde interne total (n)` = `...33`,
#          `Ajustement et résidu d'arrondissement (n)` = `...37`, `Accroissement total (n)` = `...38`) |> 
#   select(-starts_with("...")) |> 
#   mutate(across(-`Année`, ~ suppressWarnings(as.numeric(.))))
# table_4_1_2_3_2 <- data_4_1_2_3_2 |> 
#   gt() |>
#   tab_style(
#     style = cell_fill(color = "#f0f0f0"),
#     locations = cells_body(rows = seq(2, nrow(data_4_1_2_3_2), by = 2))
#   ) |> 
#   tab_style(
#     style = cell_borders(sides = "right", color = "lightgrey", weight = px(2)),
#     locations = cells_body(columns = c(2, 4, 8, 9, 12, 15, 18, 21))
#   ) |> 
#   fmt(columns = c(2:22), fns = convert_number) |>
#   tab_spanner(
#     label = "Fécondité",
#     columns = c(3:4),
#   ) |>
#   tab_spanner(
#     label = "Mortalité",
#     columns = c(5:8)
#   ) |>
#   tab_spanner(
#     label = "Migration internationale",
#     columns = c(10:12)
#   ) |>
#   tab_spanner(
#     label = "Résidents non permanents",
#     columns = c(13:15)
#   ) |>
#   tab_spanner(
#     label = "Migration interprovinciale",
#     columns = c(16:18)
#   ) |>
#   tab_spanner(
#     label = "Migration interne",
#     columns = c(19:21)
#   ) |>
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
#     c(1) ~ px(60),
#     c(2) ~ px(100),
#     c(4) ~ px(130),
#     c(12) ~ px(120),
#     c(15) ~ px(135),
#     c(18) ~ px(130),
#     c(20) ~ px(125),
#     c(22) ~ px(140),
#     everything() ~ px(110)
#   ) |> 
#   fmt_missing(
#     columns = c(2:22),
#     missing_text = "-"
#   )

data_4_1_2_3_2_plot <- sheet_4_1_2_3_2 |> #Separate data frame so loading from the spreadsheet constantly isn't needed
  transmute(`Année` = `Année`, 
            `Accroissement naturel` = `naturel`, 
            `Solde migratoire international` = `international`,
            # `Solde résidents non permanents` = `des RNP`, 
            `Solde migration interprovinciale` = `migratoire...24`,
            `Solde interne total` = `interne`) |> 
  pivot_longer(
    cols = -`Année`,
    names_to = "Change",
    values_to = "Value"
  ) |> 
  filter(is.finite(Value)) |> 
  filter(Année >= 2024)

plot_4_1_2_3_2 <- ggplot(data_4_1_2_3_2_plot, aes(x = `Année`, y = Value, fill = Change)) +
  geom_bar(stat = "identity") +
  labs(title = "", x = "", y = "Variation nette de la population par\nrapport à l'année précédente") +
  scale_fill_manual(values = c(
    "Accroissement naturel" = "#A3B0D1",
    "Solde migratoire international" = "#73AD80",
    # "Solde résidents non permanents" = "#E08565",
    "Solde migration interprovinciale" = "#9E9090", 
    "Solde interne total" = "#F5D574"
  ), labels = \(x) str_wrap(x, 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  # theme(axis.text.x = element_text(angle = 315, hjust = 1)) +
  guides(fill = guide_legend(nrow = 2))

ggsave_pdf_png(plot = plot_4_1_2_3_2, filename = "outputs/4/15_projections_composantes.pdf", width = 6.5, height = 6)
# gtsave(table_4_1_2_3_2, "outputs/4/table_4_1_2_3_2.png", vwidth = 3200)

# R Markdown --------------------------------------------------------------
qs::qsavem(plot_4_1_1_1, map_total_hh, map_owner_hh, map_tenant_hh, table_4_1_1_1,
           owner_count_diff, tenant_count_diff, owner_growth, tenant_growth,
           owner_growth_16, tenant_growth_16, total_hh_01, total_hh_21, total_prop_diff,
           plot_4_1_1_2, table_4_1_1_2, plot_4_1_1_3_comp, table_4_1_1_3_comp,
           table_4_1_1_3_comp_ed, plot_4_1_1_4, plot_4_1_1_5, renter_15, renter_25, plot_4_1_1_6,
           table_4_1_1_6, sfh_laval, sfh_owner, apt_laval, diff_2011_2021, diff_2021_2041,
           year_2011, year_2021, year_2029, year_2033, year_2037, year_2041, plot_4_1_2_1,
           change_59, change_84, change_85, plot_4_1_2_3_2,
           table_4_1_1_2_owner, table_4_1_1_2_tenant, pop_growth, growth_attributed_to_65p,
           plot_4_1_1_3_tenant, plot_4_1_1_3_owner, plot_4_1_2_3_1,
           nb_majority_owner_districts, nb_districts, owner_house_cor,
           hou_med_inc, tenant_pct_less_80, faible_rev_owner, year_2046,
           year_2051, plot_4_1_2_1_hou_age,
           file = "data/section_4_1.qsm")
