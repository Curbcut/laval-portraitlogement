source("R/utils/startup.R")

# 4.3.1 -------------------------------------------------------------------

# data_4_3_1 <- read_excel("data/4/4_3_1.xlsx") |> 
#   filter(RSS == 13) |> 
#   select(AD, NOTEMAT, NOTESOC) |> 
#   mutate(AD = as.character(AD))
# 
# data_4_3_1_sf <- laval_da |> 
#   left_join(data_4_3_1, join_by("GeoUID" == "AD")) |> 
#   drop_na()
# 
# #list(classInt::classIntervals(data_4_3_1_sf$NOTEMAT, n = 5, style = "jenks")$brks)
# `4_3_1_mat_breaks` <- c(-Inf, -0.049183698, -0.019897522, 0.004244034, 0.032446890, Inf)
# `4_3_1_mat_breaks_labels` <- c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé")
# #list(classInt::classIntervals(data_4_3_1_sf$NOTESOC, n = 5, style = "jenks")$brks)
# `4_3_1_soc_breaks` <- c(-Inf, -0.071559490, -0.034659522, -0.001368799,  0.038467068, Inf)
# `4_3_1_soc_breaks_labels` <- c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé")
# 
# data_4_3_1_sf <- data_4_3_1_sf |> 
#   mutate("mat_quant" = cut(`NOTEMAT`, breaks = `4_3_1_mat_breaks`, include.lowest = TRUE,
#                           labels = `4_3_1_mat_breaks_labels`),
#          "soc_quant" = cut(`NOTESOC`, breaks = `4_3_1_soc_breaks`, include.lowest = TRUE,
#                                   labels = `4_3_1_soc_breaks_labels`))
# 
# map_4_3_1_mat <- ggplot(data = data_4_3_1_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `mat_quant`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "L’indice de défavorisation matérielle") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# map_4_3_1_soc <- ggplot(data = data_4_3_1_sf) +
#   gg_cc_tiles +
#   geom_sf(aes(geometry = geometry, fill = `soc_quant`), alpha = 0.9, color = "transparent") +
#   scale_fill_manual(values = curbcut_scale,
#                     name = "L’indice de défavorisation sociale") +
#   gg_cc_theme +
#   guides(fill = guide_legend(title.position = "top",
#                              title.hjust = 0.5))
# 
# #Saving the maps
# ggsave("outputs/4/map_4_3_1_mat.png", plot = map_4_3_1_mat, width = 600/72, height = 800/72, dpi = 72)
# ggsave("outputs/4/map_4_3_1_soc.png", plot = map_4_3_1_soc, width = 600/72, height = 800/72, dpi = 72)

defav_2021 <- sf::st_read("data/defav/Ad_2021_IndDef.shp") 
defav_2021 <- sf::st_transform(defav_2021, crs = 32618)
defav_2021 <- sf::st_filter(defav_2021, lvl)

defav_2021 <- defav_2021[c("ADIDU", "QuintMatRS", "QuintSocRS")]

defav_2021 <- defav_2021[order(defav_2021$ADIDU), ]

defav <- defav_2021 %>%
  mutate(defav = case_when(
    QuintMatRS %in% c(1, 2) & QuintSocRS %in% c(1, 2) ~ "Favorisés", # Les deux indices favorisés
    (QuintMatRS %in% c(1, 2) & QuintSocRS %in% c(3)) |
      (QuintMatRS %in% c(3) & QuintSocRS %in% c(1, 2)) ~ "Intermédiaires", # Un favorisé, l'autre intermédiaire
    QuintMatRS %in% c(3) & QuintSocRS %in% c(3) ~ "Intermédiaires", # Les deux intermédiaires
    QuintMatRS %in% c(4, 5) & QuintSocRS %in% c(1, 2, 3) ~ "Défavorisés (mat.)", # Matériel défavorisé, social non
    QuintMatRS %in% c(1, 2, 3) & QuintSocRS %in% c(4, 5) ~ "Défavorisés (soc.)", # Social défavorisé, matériel non
    QuintMatRS %in% c(4, 5) & QuintSocRS %in% c(4, 5) ~ "Défavorisés (mat. et soc.)", # Les deux indices défavorisés
    TRUE ~ NA_character_ # Cas où aucune condition n'est remplie
  )) |> 
  select(ADIDU, defav)

defav$defav <- factor(defav$defav, labels = c("Défavorisés (mat. et soc.)",
                                              "Défavorisés (soc.)",
                                              "Défavorisés (mat.)",
                                              "Intermediaires",
                                              "Favorisés"))
defav$defav <- addNA(defav$defav)
lvls <- levels(defav$defav)
lvls <- c(lvls[length(lvls)], rev(lvls[-length(lvls)]))

defav_plot <- 
  ggplot(data = defav) +
  gg_cc_tiles +
  geom_sf(aes(fill = defav), color = "transparent", lwd = 0) +
  scale_fill_manual(
    values = c(curbcut_colors$left_5$fill[1], 
               c("#E8E8E8", "#B7CBCC", "#6C83B5", "#73AE80", "#2A5A5B")),
    na.value = curbcut_colors$left_5$fill[1],
    name = element_blank(),
    breaks = lvls,
    labels = lvls,
    guide = guide_legend(
      title.position = "top",
      label.position = "bottom",
      keywidth = 4,
      keyheight = 0.5
    )
  ) +
  gg_cc_theme


ggsave_pdf_png(filename = here::here("outputs/4/23_defav.pdf"),
                plot = defav_plot, width = 6.5, height = 6)


# 4.3.2 -------------------------------------------------------------------


# 4.3.3 -------------------------------------------------------------------

# R Markdown --------------------------------------------------------------
qs::qsavem(defav_plot, file = "data/section_4_3.qsm")
