source("R/utils/startup.R")


# 5.1.2.1 -------------------------------------------------------------------

rep_census <- cancensus::get_census(dataset = "CA21",
                                    regions = list(CSD = 2465005),
                                    level = "CT",
                                    vectors = c("total"= "v_CA21_4272",
                                                "majorrep" = "v_CA21_4274"),
                                    geo_format = "sf")

rep_census <- rep_census[c("total", "majorrep", "Population")]

rep_sectors <- interpolate(from = rep_census, additive_vars = 
                             c("total", "majorrep", "Population"))
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


rep_census <- cancensus::get_census(dataset = "CA21",
                                    regions = list(CSD = 2465005),
                                    level = "DA",
                                    vectors = c("total"= "v_CA21_4272",
                                                "majorrep" = "v_CA21_4274"),
                                    geo_format = "sf")
# Create bins
labels <- c("< 5", "5 - 10", "10 - 15", "15 - 20", "> 20")
rep_census$bins <- cut(rep_census$majorrep, 
                       breaks = c(-Inf, 5, 10, 15, 20, Inf), 
                       labels = labels, 
                       include.lowest = TRUE)
rep_census <- sf::st_transform(rep_census, crs = 32618)

rep_plot <- 
  ggplot(rep_census) +
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
                plot = rep_pct_plot, width = 7.5, height = 6)

ggplot2::ggsave(filename = here::here("outputs/5/5_1_2_1_rep.pdf"), 
                plot = rep_plot, width = 7.5, height = 6)

rep_census_CSD <- cancensus::get_census(dataset = "CA21",
                                    regions = list(CSD = 2465005),
                                    level = "CSD",
                                    vectors = c("total"= "v_CA21_4272",
                                                "majorrep" = "v_CA21_4274"),
                                    geo_format = "sf")

reparation_majeur_pct <- convert_pct(rep_census_CSD$majorrep / rep_census_CSD$total)
reparation_majeur <- convert_number_noround(rep_census_CSD$majorrep)

# 5.1.2.2 -------------------------------------------------------------------

rep_census_CSD_2016 <- cancensus::get_census(dataset = "CA16",
                                        regions = list(CSD = 2465005),
                                        level = "CSD",
                                        vectors = c("total"= "v_CA16_4870",
                                                    "majorrep" = "v_CA16_4872"),
                                        geo_format = "sf")
reparation_majeur_pct_2016 <- rep_census_CSD_2016$majorrep / rep_census_CSD_2016$total

rep_census_CSD_2011 <- cancensus::get_census(dataset = "CA11",
                                        regions = list(CSD = 2465005),
                                        level = "CSD",
                                        vectors = c("total"= "v_CA11N_2230",
                                                    "majorrep" = "v_CA11N_2232"),
                                        geo_format = "sf")
reparation_majeur_pct_2011 <- rep_census_CSD_2011$majorrep / rep_census_CSD_2011$total

rep_census_CSD_2006 <- cancensus::get_census(dataset = "CA06",
                                        regions = list(CSD = 2465005),
                                        level = "CSD",
                                        vectors = c("total"= "v_CA06_105",
                                                    "majorrep" = "v_CA06_108"),
                                        geo_format = "sf")
reparation_majeur_pct_2006 <- rep_census_CSD_2006$majorrep / rep_census_CSD_2006$total

rep_census_CSD_2001 <- cancensus::get_census(dataset = "CA01",
                                        regions = list(CSD = 2465005),
                                        level = "CSD",
                                        vectors = c("total"= "v_CA01_96",
                                                    "majorrep" = "v_CA01_104"),
                                        geo_format = "sf")
reparation_majeur_pct_2001 <- rep_census_CSD_2001$majorrep / rep_census_CSD_2001$total

rep_census_CSD_1996 <- cancensus::get_census(dataset = "CA1996",
                                        regions = list(CSD = 2465005),
                                        level = "CSD",
                                        vectors = c("total"= "v_CA1996_1678",
                                                    "majorrep" = "v_CA1996_1687"),
                                        geo_format = "sf")
reparation_majeur_pct_1996 <- rep_census_CSD_1996$majorrep / rep_census_CSD_1996$total

data <- tibble::tibble(years = c(2021, 2016, 2011, 2006, 2001, 1996),
               rep_pct = c(rep_census_CSD$majorrep / rep_census_CSD$total,
                           reparation_majeur_pct_2016,
                           reparation_majeur_pct_2011,
                           reparation_majeur_pct_2006,
                           reparation_majeur_pct_2001,
                           reparation_majeur_pct_1996),
               majorrep_needed = c(
                 rep_census_CSD$majorrep,
                 rep_census_CSD_2016$majorrep,
                 rep_census_CSD_2011$majorrep,
                 rep_census_CSD_2006$majorrep,
                 rep_census_CSD_2001$majorrep,
                 rep_census_CSD_1996$majorrep
               ),
               dwellings = c(rep_census_CSD$Dwellings,
                             rep_census_CSD_2016$Dwellings,
                             rep_census_CSD_2011$Dwellings,
                             rep_census_CSD_2006$Dwellings,
                             rep_census_CSD_2001$Dwellings,
                             rep_census_CSD_1996$Dwellings))

rep_evol <- 
  ggplot(data, aes(x = years, y = rep_pct)) +
  geom_line(color = color_theme("redhousing"), size = 1.2) +
  geom_point(size = 3, color = color_theme("redhousing")) +
  scale_y_continuous(labels = convert_pct, limits = c(0.04, 0.07)) +
  scale_x_continuous(breaks = data$years) +
  
  labs(title = NULL,
       x = "Années",
       y = "Nécessitant des réparations majeures") +
  graph_theme


ggplot2::ggsave(filename = here::here("outputs/5/5_1_2_2_repevol.pdf"), 
                plot = rep_evol, width = 6, height = 4)


# 5.1.2.3 -------------------------------------------------------------------



# Save --------------------------------------------------------------------

qs::qsavem(rep_pct_plot, rep_plot, reparation_majeur_pct, reparation_majeur, 
           rep_evol, file = "data/section_5_1_2.qsm")
