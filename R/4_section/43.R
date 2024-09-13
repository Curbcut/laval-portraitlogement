source("R/utils/startup.R")

# 4.3.1 -------------------------------------------------------------------
data_4_3_1 <- read_excel("data/4/4_3_1.xlsx") |> 
  filter(RSS == 13) |> 
  select(AD, NOTEMAT, NOTESOC) |> 
  mutate(AD = as.character(AD))

data_4_3_1_sf <- laval_da |> 
  left_join(data_4_3_1, join_by("GeoUID" == "AD")) |> 
  drop_na()

#list(classInt::classIntervals(data_4_3_1_sf$NOTEMAT, n = 5, style = "jenks")$brks)
`4_3_1_mat_breaks` <- c(-Inf, -0.049183698, -0.019897522, 0.004244034, 0.032446890, Inf)
`4_3_1_mat_breaks_labels` <- c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé")
#list(classInt::classIntervals(data_4_3_1_sf$NOTESOC, n = 5, style = "jenks")$brks)
`4_3_1_soc_breaks` <- c(-Inf, -0.071559490, -0.034659522, -0.001368799,  0.038467068, Inf)
`4_3_1_soc_breaks_labels` <- c("Très faible", "Faible", "Moyen", "Élevé", "Très élevé")

data_4_3_1_sf <- data_4_3_1_sf |> 
  mutate("mat_quant" = cut(`NOTEMAT`, breaks = `4_3_1_mat_breaks`, include.lowest = TRUE,
                          labels = `4_3_1_mat_breaks_labels`),
         "soc_quant" = cut(`NOTESOC`, breaks = `4_3_1_soc_breaks`, include.lowest = TRUE,
                                  labels = `4_3_1_soc_breaks_labels`))

map_4_3_1_mat <- ggplot(data = data_4_3_1_sf) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = `mat_quant`), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "L’indice de défavorisation matérielle") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

map_4_3_1_soc <- ggplot(data = data_4_3_1_sf) +
  gg_cc_tiles +
  geom_sf(aes(geometry = geometry, fill = `soc_quant`), alpha = 0.9, color = "transparent") +
  scale_fill_manual(values = curbcut_scale,
                    name = "L’indice de défavorisation sociale") +
  gg_cc_theme +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5))

#Saving the maps
ggsave("outputs/4/map_4_3_1_mat.png", plot = map_4_3_1_mat, width = 600/72, height = 800/72, dpi = 72)
ggsave("outputs/4/map_4_3_1_soc.png", plot = map_4_3_1_soc, width = 600/72, height = 800/72, dpi = 72)

# 4.3.2 -------------------------------------------------------------------


# 4.3.3 -------------------------------------------------------------------

# R Markdown --------------------------------------------------------------
qs::qsavem(map_4_3_1_mat, map_4_3_1_soc,
           file = "data/section_4_3.qsm")
