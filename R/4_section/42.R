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
  mutate(alpha_custom = scales::rescale(nb, to = c(0.4, 1)))

plot_4_2_1 <- 
  ggplot(occ_rev_comp, aes(x = comp_pretty, y = median_revenue, fill = mode_occupation, alpha = alpha_custom)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Revenu annuel médian", fill = NULL) +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 12)) +
  scale_y_continuous(labels = \(x) paste(convert_number(x), "$")) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  graph_theme

ggsave(plot = plot_4_2_1, "outputs/4/plot_4_2_1.pdf", width = 7.5, height = 4)

rev_fun_421 <- function(mode_occupation, composition) {
  z <- occ_rev_comp$median_revenue[occ_rev_comp$mode_occupation == mode_occupation &
                                occ_rev_comp$composition == composition]
  paste(convert_number(z), "$")
}


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
  by = c("occupation", "income"))

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
  by = c("occupation", "income"))


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
  ggplot(income_housingcost, aes(x = income, y = housingcost, fill = occupation, alpha = alpha_custom)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = NULL, y = "Frais de logement", fill = NULL) +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  # scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
  scale_y_continuous(labels = \(x) paste(convert_number(x), "$")) +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  graph_theme

ggsave(plot = plot_4_2_4, "outputs/4/plot_4_2_4.pdf", width = 7.5, height = 4)

loyer_fun_421 <- function(mode_occupation, income) {
  z <- income_housingcost$housingcost[income_housingcost$occupation == mode_occupation &
                                     income_housingcost$income == income]
  paste(convert_number(z), "$")
}

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


# 4.2.6 -------------------------------------------------------------------


# 4.2.7 -------------------------------------------------------------------


# 4.2.8 -------------------------------------------------------------------


# 4.2.9 -------------------------------------------------------------------


# 4.2.10 ------------------------------------------------------------------


# 4.2.11 ------------------------------------------------------------------

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

att_general_mean <- mean(att$jours)
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
qs::qsavem(plot_4_2_1, plot_4_2_2, map_4_2_2, att_general_mean, att_fam, att_lessfn, att_other,
           att_sfplus, att_general_mean, att_programme_table, occ_rev_comp, rev_fun_421,
           loyer_fun_421, income_housingcost,
           sfplus, file = "data/section_4_2.qsm")
