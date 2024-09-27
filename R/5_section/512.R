source("R/utils/startup.R")


# 5.1.2.1 -------------------------------------------------------------------

rep_census <- cancensus::get_census(dataset = "CA21",
                                    regions = list(CSD = 2465005),
                                    level = "DA",
                                    vectors = c("total"= "v_CA21_4272",
                                                "majorrep" = "v_CA21_4274"),
                                    geo_format = "sf")

rep_census <- rep_census[c("total", "majorrep")]

rep_sectors <- interpolate(from = rep_census, additive_vars = 
                             c("total", "majorrep"))
rep_sectors$majorrep_pct <- rep_sectors$majorrep / rep_sectors$total

# Create bins
labels <- c("< 2 %", "2 % - 4 %", "4 % - 6 %", "6 % - 8 %", "> 8 %")
rep_sectors$bins_pct <- cut(rep_sectors$majorrep_pct, 
                        breaks = c(-Inf, 0.02, 0.04, 0.06, 0.08, Inf), 
                        labels = labels, 
                        include.lowest = TRUE)

rep_pct_plot <- 
  ggplot(rep_sectors) +
  gg_cc_tiles +
  geom_sf(aes(fill = bins_pct), lwd = 0, color = "transparent", show.legend = TRUE) + 
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Pourcentage des logements nécessitant des réparations majeures",
                    labels = labels,
                    drop = FALSE,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1)) +
  gg_cc_theme

# Create bins
labels <- c("< 250", "250 - 350", "350 - 450", "450 - 550", "> 550")
rep_sectors$bins <- cut(rep_sectors$majorrep, 
                        breaks = c(-Inf, 250, 350, 450, 550, Inf), 
                        labels = labels, 
                        include.lowest = TRUE)

rep_plot <- 
  ggplot(rep_sectors) +
  gg_cc_tiles +
  geom_sf(aes(fill = bins), lwd = 0, color = "transparent", show.legend = TRUE) + 
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Nombre de logements nécessitant des réparations majeures",
                    labels = labels,
                    drop = FALSE,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1)) +
  gg_cc_theme

# library(patchwork)
# 
# rep_plot_bind <- rep_pct_plot + patchwork::plot_spacer() + rep_plot +
#   patchwork::plot_layout(widths = c(1, 0.01, 1))

ggplot2::ggsave(filename = here::here("outputs/5/5_1_2_1_reppct.pdf"), 
                plot = rep_pct_plot, width = 6.5, height = 6)

ggplot2::ggsave(filename = here::here("outputs/5/5_1_2_1_rep.pdf"), 
                plot = rep_plot, width = 6.5, height = 6)

rep_census <- cancensus::get_census(dataset = "CA21",
                                    regions = list(CSD = 2465005),
                                    level = "CSD",
                                    vectors = c("total"= "v_CA21_4272",
                                                "majorrep" = "v_CA21_4274"),
                                    geo_format = "sf")

reparation_majeur_pct <- convert_pct(rep_census$majorrep / rep_census$total)
reparation_majeur <- convert_number_noround(rep_census$majorrep)

# 5.1.2.2 -------------------------------------------------------------------


# 5.1.2.3 -------------------------------------------------------------------


# Save --------------------------------------------------------------------

qs::qsavem(rep_pct_plot, rep_plot, reparation_majeur_pct, reparation_majeur, 
           file = "data/5_1_2.qsm")
