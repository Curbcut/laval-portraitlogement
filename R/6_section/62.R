source("R/utils/startup.R")


# 6.2.1 -------------------------------------------------------------------

# 6.2.2 -------------------------------------------------------------------

# 6.2.3 -------------------------------------------------------------------

# 6.2.4 -------------------------------------------------------------------

coop <- read_xlsx("data/Liste logement social_20240606.xlsx", 
                  sheet = "COOP", skip = 2) |> 
  filter(!is.na(Adresse)) |> 
  filter(!is.na(`Nb unités`))
omhl <- read_xlsx("data/Liste logement social_20240606.xlsx", 
                  sheet = "OMHL", skip = 2) |> 
  filter(!is.na(Adresse))
obnl <- read_xlsx("data/Liste logement social_20240606.xlsx", 
                  sheet = "OBNL", skip = 2) |> 
  filter(!is.na(Adresse))

# Geocode addresses
coop$Adresse <- gsub(" .\\d. \\d.\\d", ", Laval", coop$Adresse)
coop_locations <- lapply(coop$Adresse, cc.data::geocode_localhost)
coop_locations <- Reduce(rbind, 
                         lapply(coop_locations, \(x) {
                           tibble::tibble(geometry = sf::st_sfc(x))
                         })
) |> sf::st_as_sf(crs = 4326)
coop$geometry <- coop_locations$geometry
coop <- sf::st_as_sf(coop, crs = 4326)
coop <- sf::st_make_valid(coop)

omhl$Adresse <- gsub(" .\\d. \\d.\\d", ", Laval", omhl$Adresse)
omhl_locations <- lapply(omhl$Adresse, cc.data::geocode_localhost)
omhl_locations <- Reduce(rbind, 
                         lapply(omhl_locations, \(x) {
                           tibble::tibble(geometry = sf::st_sfc(x))
                         })
) |> sf::st_as_sf(crs = 4326)
omhl$geometry <- omhl_locations$geometry
omhl <- sf::st_as_sf(omhl, crs = 4326)
omhl <- sf::st_make_valid(omhl)

obnl$Adresse <- gsub(" .\\d. \\d.\\d", ", Laval", obnl$Adresse)
obnl_locations <- lapply(obnl$Adresse, cc.data::geocode_localhost)
obnl_locations <- Reduce(rbind, 
                         lapply(obnl_locations, \(x) {
                           tibble::tibble(geometry = sf::st_sfc(x))
                         })
) |> sf::st_as_sf(crs = 4326)
obnl$geometry <- obnl_locations$geometry
obnl <- sf::st_as_sf(obnl, crs = 4326)
obnl <- sf::st_make_valid(obnl)

all <- rbind(coop[c("Nom du projet", "Nb unités", "Chambre", "Studio", "1 CC", "2 CC", "Unités 3CC+")] |> 
               mutate(type = "COOP"),
             omhl[c("Nom du projet", "Nb unités", "Chambre", "Studio", "1 CC", "2 CC", "Unités 3CC+")] |> 
               mutate(type = "OMHL"),
             obnl[c("Nom du projet", "Nb unités", "Chambre", "Studio", "1 CC", "2 CC", "Unités 3CC+")] |> 
               mutate(type = "OBNL")) |> 
  pivot_longer(cols = c("Chambre", "Studio", "1 CC", "2 CC", "Unités 3CC+"),
               names_to = "Type d'unité",
               values_to = "Nombre d'unités") |> 
  mutate(`Nombre d'unités` = as.numeric(`Nombre d'unités`)) |> 
  sf::st_drop_geometry() |> 
  group_by(type, `Type d'unité`) |> 
  summarize(`Nombre d'unités` = sum(`Nombre d'unités`, na.rm = TRUE))

all$`Type d'unité` <- factor(all$`Type d'unité`, 
                             levels = c("Chambre", "Studio", "1 CC", "2 CC", "Unités 3CC+"))

nonluc_types <- 
  ggplot(all, aes(x = type, y = `Nombre d'unités`, fill = `Type d'unité`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nombre de chambres", y = "Type d'unité", fill = "Type de projet") +
  scale_fill_manual(values = c("Chambre" = curbcut_scale[1], 
                               "Studio" = curbcut_scale[2], 
                               "1 CC" = curbcut_scale[3], 
                               "2 CC" = curbcut_scale[4], 
                               "Unités 3CC+" = curbcut_scale[5]),
                    labels = c("Chambre" = "Chambre\nseulement", 
                               "Studio" = "Studio", 
                               "1 CC" = "1 chambre", 
                               "2 CC" = "2 chambres", 
                               "Unités 3CC+" = "3+ chambres")) +
  graph_theme

ggplot2::ggsave(filename = here::here("outputs/6/6_2_2_nonluctypes.pdf"),
                plot = nonluc_types, width = 6.5, height = 4)

all_all <- all |> 
  group_by(`Type d'unité`) |> 
  summarize(`Nombre d'unités` = sum(`Nombre d'unités`, na.rm = TRUE))

nonluc_types_all <- 
  ggplot(all_all, aes(x = `Type d'unité`, y = `Nombre d'unités`, fill = `Type d'unité`)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Nombre de chambres", y = "Type d'unité", fill = "Type de projet") +
  scale_fill_manual(values = c("Chambre" = curbcut_scale[1], 
                               "Studio" = curbcut_scale[2], 
                               "1 CC" = curbcut_scale[3], 
                               "2 CC" = curbcut_scale[4], 
                               "Unités 3CC+" = curbcut_scale[5]))+
  scale_x_discrete(labels = c("Chambre" = "Chambre\nseulement", 
                              "Studio" = "Studio", 
                              "1 CC" = "1 chambre", 
                              "2 CC" = "2 chambres", 
                              "Unités 3CC+" = "3+ chambres")) +
  scale_y_continuous(labels = convert_number) +
  graph_theme +
  theme(legend.position = "none")


ggplot2::ggsave(filename = here::here("outputs/6/6_2_2_nonluctypesall.pdf"),
                plot = nonluc_types_all, width = 4.5, height = 4)

# 6.2.5 -------------------------------------------------------------------

# 6.2.6 -------------------------------------------------------------------

qs::qsavem(nonluc_types, nonluc_types_all, file = "data/section_6_2.qsm")
