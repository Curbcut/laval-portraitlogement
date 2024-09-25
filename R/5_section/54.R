source("R/utils/startup.R")

# 5.4.1 -------------------------------------------------------------------

# Load census variables ---------------------------------------------------

# Socioprofessionnelles
noc_total <- c(#"total" = "v_CA21_6561", "na" = "v_CA21_6564", 
  "all" = "v_CA21_6567",
  "0" = "v_CA21_6570", "1" = "v_CA21_6573", "2" = "v_CA21_6576",
  "3" = "v_CA21_6579", "4" = "v_CA21_6582", "5" = "v_CA21_6585",
  "6" = "v_CA21_6588", "7" = "v_CA21_6591", "8" = "v_CA21_6594",
  "9" = "v_CA21_6597")
noc_total <- unname(noc_total)
sociopro <- get_census(dataset = "CA21", 
                       regions = list(CSD = 2465005), 
                       level = "DA",
                       vectors = noc_total,
                       geo_format = "sf")
sociopro <- 
  sociopro |> 
  mutate(across(`v_CA21_6570: 0 Legislative and senior management occupations`:`v_CA21_6597: 9 Occupations in manufacturing and utilities`, ~ ./ `v_CA21_6567: All occupations`))

# Cultures
c21 <- cancensus::list_census_vectors("CA21")
ethn_cult <- c21$vector[c21$parent_vector == "v_CA21_4917"]
ethn_cult <- ethn_cult[!is.na(ethn_cult)]
ethn_cult <- get_census(dataset = "CA21", 
                        regions = list(CSD = 2465005), 
                        level = "DA",
                        vectors = c("v_CA21_4917", ethn_cult),
                        geo_format = "sf")
ethn_cult <- 
  ethn_cult |> 
  mutate(across(`v_CA21_4920: Canadian`:`v_CA21_5667: Paraguayan`, ~ ./ `v_CA21_4917: Total - Ethnic or cultural origin for the population in private households`))

# Tranches d'âge
age <- c21$vector[c21$parent_vector %in% c("v_CA21_11", "v_CA21_68", "v_CA21_251")]
age <- age[!is.na(age)]

age <- get_census(dataset = "CA21", 
                  regions = list(CSD = 2465005), 
                  level = "DA",
                  vectors = c("v_CA21_8", age),
                  geo_format = "sf")
age <- 
  age |> 
  mutate(across(`v_CA21_14: 0 to 4 years`:`v_CA21_326: 85 years and over`, ~ ./ `v_CA21_8: Total - Age`))

# Income
income <- c21$vector[c21$parent_vector == "v_CA21_671"]
income <- income[!is.na(income)]
income <- get_census(dataset = "CA21", 
                     regions = list(CSD = 2465005), 
                     level = "DA",
                     vectors = c("v_CA21_671", income),
                     geo_format = "sf")
income <- 
  income |> 
  mutate(across(`v_CA21_674: Under $10,000 (including loss)`:`v_CA21_704: $100,000 and over`, ~ ./ `v_CA21_671: With total income`))

# Education
education <- c21$vector[c21$parent_vector == "v_CA21_5817"]
education <- education[!is.na(education)]
education <- get_census(dataset = "CA21", 
                        regions = list(CSD = 2465005), 
                        level = "DA",
                        vectors = c("v_CA21_5817", education),
                        geo_format = "sf")
education <- 
  education |> 
  mutate(across(`v_CA21_5820: No certificate, diploma or degree`:`v_CA21_5826: Postsecondary certificate, diploma or degree`, ~ ./ `v_CA21_5817: Total - Highest certificate, diploma or degree for the population aged 15 years and over in private households`))


# Do entropy instead ------------------------------------------------------

calculate_entropy <- function(percentages) {
  # Remplacer les zéros par des valeurs très petites pour éviter les problèmes de log(0)
  percentages[percentages == 0] <- .Machine$double.eps
  # Calculer l'entropie de Shannon
  entropy <- -sum(percentages * log(percentages), na.rm = TRUE)
  return(entropy)
}

# Calcul de l'entropie pour chaque catégorie
sociopro$entropy <- apply(sf::st_drop_geometry(sociopro)[(18-2):(27-2)], MARGIN = 1, 
                          FUN = calculate_entropy, simplify = TRUE)
ethn_cult$entropy <- apply(sf::st_drop_geometry(ethn_cult)[(18-2):(264-2)], MARGIN = 1, 
                           FUN = calculate_entropy, simplify = TRUE)
age$entropy <- apply(sf::st_drop_geometry(age)[(18-2):(35-2)], MARGIN = 1, 
                     FUN = calculate_entropy, simplify = TRUE)
income$entropy <- apply(sf::st_drop_geometry(income)[(18-2):(28-2)], MARGIN = 1, 
                        FUN = calculate_entropy, simplify = TRUE)
education$entropy <- apply(sf::st_drop_geometry(education)[(18-2):(20-2)], MARGIN = 1, 
                           FUN = calculate_entropy, simplify = TRUE)

# Normalisation des entropies
normalize_entropy <- function(entropy) {
  (entropy - min(entropy, na.rm = TRUE)) / (max(entropy, na.rm = TRUE) - min(entropy, na.rm = TRUE))
}

sociopro$entropy_norm <- normalize_entropy(sociopro$entropy)
ethn_cult$entropy_norm <- normalize_entropy(ethn_cult$entropy)
age$entropy_norm <- normalize_entropy(age$entropy)
income$entropy_norm <- normalize_entropy(income$entropy)
education$entropy_norm <- normalize_entropy(education$entropy)

# Somme de l'entropie par zone
CTs <- get_census(dataset = "CA21", 
                  regions = list(CSD = 2465005), 
                  level = "DA",
                  geo_format = "sf")

CTs$entropy <- sociopro$entropy_norm + ethn_cult$entropy_norm + age$entropy_norm + income$entropy_norm + education$entropy_norm
CTs <- interpolate(CTs, average_vars = "entropy")

CTs$breaks <- cut(CTs$entropy, 
    breaks = c(-Inf, quantile(CTs$entropy, probs = c(.25, .50, .75, .95)), Inf), 
    labels = c("Mixité basse", "a", "b", "c", "Mixité haute"), 
    include.lowest = TRUE)

labels <- c("Mixité basse", "", "", "", "Mixité haute")

# Plotting the sf map with custom bins
CTs <- sf::st_transform(CTs, crs = 32618)

mixite_sociale <- 
  ggplot(CTs) +
  gg_cc_tiles +
  geom_sf(aes(fill = breaks), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("outputs/5/5_4_1_mixitecomposite.pdf"), 
                plot = mixite_sociale, width = 6.5, height = 6)


# Facet wrap entropy ------------------------------------------------------

# Normalisation des entropies pour chaque catégorie
sociopro$dimension <- "Profession"
ethn_cult$dimension <- "Origine ethnique ou culturelle"
age$dimension <- "Âge"
income$dimension <- "Revenu"
education$dimension <- "Éducation"

# Rassembler toutes les données dans un seul dataframe
all_data <- bind_rows(
  select(sociopro, GeoUID, entropy_norm, dimension),
  select(ethn_cult, GeoUID, entropy_norm, dimension),
  select(age, GeoUID, entropy_norm, dimension),
  select(income, GeoUID, entropy_norm, dimension),
  select(education, GeoUID, entropy_norm, dimension)
)

# Renommer la colonne d'entropie normalisée pour une utilisation cohérente
names(all_data)[2] <- "entropy"

# Interpolate
all_data <- sf::st_transform(all_data, crs = 32618)
all_data <- lapply(unique(all_data$dimension), \(x) {
    interpolate(from = all_data[all_data$dimension == x, ], average_vars = "entropy") |> 
    mutate(dimension = x)
})
all_data <- Reduce(rbind, all_data)

# Create bins
all_data$bins <- cut(all_data$entropy, 
                     breaks = c(-Inf, quantile(all_data$entropy, probs = c(.25, .50, .75, .95)), Inf), 
                     labels = c("Mixité basse", "a", "b", "c", "Mixité haute"), 
                     include.lowest = TRUE)
labels <- c("Mixité basse", "", "", "", "Mixité haute")

mixite_sociale_facet <- 
  ggplot(all_data) +
  # gg_cc_tiles +
  geom_sf(aes(fill = bins), color = "transparent", lwd = 0) +
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Indice de mixité",
                    labels = labels,
                    guide = guide_legend(title.position = "top", label.position = "bottom", nrow = 1)) +
  facet_wrap(~ dimension, nrow = 2) +
  gg_cc_theme +
  theme(legend.spacing.x = unit(2, 'cm'),
        legend.spacing.y = unit(1, 'cm'))

ggplot2::ggsave(filename = here::here("outputs/5/5_4_1_mixitefacet.pdf"), 
                plot = mixite_sociale_facet, width = 9, height = 7)

# 5.4.2 -------------------------------------------------------------------

years <- c(2001, 2006, 2011, 2016, 2021)
dwellings <- lapply(paste0("CA", gsub("^20", "", years)), 
                    get_census, 
                    regions = list(CSD = 2465005), 
                    level = "DB",
                    geo_format = "sf")
dwellings <- lapply(dwellings, interpolate, additive_vars = "Dwellings")
dwellings <- mapply(\(df, year) {
  df <- df[c("NOM", "Dwellings")]
  df[[paste0("density_", year)]] <- df$Dwellings / (cc.buildr::get_area(df) / 1e6)
  names(df)[2] <- paste0("dw_", year)
  df
}, dwellings, years, SIMPLIFY = FALSE)

combined_df <- lapply(dwellings, sf::st_drop_geometry) %>%
  reduce(left_join, by = "NOM")

combined_df <- left_join(combined_df, electoral_districts["NOM"]) |> 
  sf::st_as_sf()

# Create bins
labels <- c("0 - 500", "500 - 1 000", "1 000 - 1 500", "1 500 - 2 000", "> 2 000")
combined_df$bins <- cut(combined_df$density_2021, 
                        breaks = c(-Inf, 500, 1000, 1500, 2000, Inf), 
                        labels = labels, 
                        include.lowest = TRUE)

densite_res_plot <- 
  ggplot(combined_df) +
  gg_cc_tiles +
  geom_sf(aes(fill = bins), lwd = 0) + 
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Unités résidentielles par kilomètre carré",
                    labels = labels,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1)) +
  gg_cc_theme

ggplot2::ggsave(filename = here::here("outputs/5/5_4_2_densiteres.pdf"), 
                plot = densite_res_plot, width = 6.5, height = 6)




# Evolution
combined_with_pct <- combined_df %>%
  mutate(
    pct_2001_2006 = (density_2006 - density_2001) / dw_2001,
    pct_2006_2011 = (density_2011 - density_2006) / dw_2006,
    pct_2011_2016 = (density_2016 - density_2011) / dw_2011,
    pct_2016_2021 = (density_2021 - density_2016) / dw_2016
  )

long_df <- combined_with_pct %>%
  select(NOM, geometry, pct_2001_2006, pct_2006_2011, pct_2011_2016, pct_2016_2021) %>%
  pivot_longer(cols = starts_with("pct_"), names_to = "period", values_to = "pct_change") |> 
    mutate(period = recode(period,
                         "pct_2001_2006" = "2001 à 2006",
                         "pct_2006_2011" = "2006 à 2011",
                         "pct_2011_2016" = "2011 à 2016",
                         "pct_2016_2021" = "2016 à 2021"))

# Create bins
labels <- c("< 0%", "0 - 1 %", "1 - 2 %", "2 - 3 %", "> 3 %")
long_df$bins <- cut(long_df$pct_change, 
                     breaks = c(-Inf, 0, 0.01, 0.02, 0.03, Inf), 
                     labels = labels, 
                     include.lowest = TRUE)

densite_res_evol_plot <- 
  ggplot(long_df) +
  geom_sf(aes(fill = bins), lwd = 0) + 
  scale_fill_manual(values = curbcut_colors$left_5$fill[2:6],
                    name = "Augmentation de la densité résidentielle (%)",
                    labels = labels,
                    guide = guide_legend(title.position = "top", 
                                         label.position = "bottom", 
                                         nrow = 1)) +
  facet_wrap(~ period) + # Facet wrap par période
  gg_cc_theme

ggplot2::ggsave(filename = here::here("outputs/5/5_4_2_densiteresevol.pdf"), 
                plot = densite_res_evol_plot, width = 9, height = 7)

# 5.4.3 -------------------------------------------------------------------

# 5.4.4 -------------------------------------------------------------------

# 5.4.5 -------------------------------------------------------------------

# uef <- tibble::as_tibble(read.csv("data/role-evaluation-2016.csv"))
# pc <- cc.data::bucket_read_object(object = "postal_codes202103.csv",
#                             bucket = "curbcut.rawdata",
#                             objectext = ".csv",
#                             method = utils::read.csv) |>
#   tibble::as_tibble()
# pc <- sf::st_as_sf(pc, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
# pc <- pc |> 
#   mutate(pc = gsub(" .{3}$", "", POSTAL_CODE)) |> 
#   group_by(pc) |> 
#   summarize()
# 
# uef <- uef[c("ra01.code.postal.3.car", "ra01.nb.total.logements")]
# names(uef) <- c("pc", "logements")
# 
# pc <- pc[pc$pc %in% uef$pc, ]

pu <- sf::st_read("data/sad-perimetre-d-urbanisation.geojson")

DB <- get_census(dataset = "CA21", 
                 regions = list(CSD = 2465005), 
                 level = "DB",
                 geo_format = "sf")

# Too poor accuracy
mapview::mapview(list(DB |> sf::st_cast("MULTILINESTRING"), pu))

# Save --------------------------------------------------------------------

qs::qsavem(mixite_sociale, mixite_sociale_facet, densite_res_plot,
           densite_res_evol_plot,
           file = "data/5_4.qsm")

