
# Pyramide des âges -------------------------------------------------------

census_vectors <- cancensus::list_census_vectors("CA21")
age_vectors <- cc.data::census_vectors_age
age_vectors <- unlist(age_vectors[4:nrow(age_vectors), ]$vec_2021)
removed_code <- as.numeric(gsub("v_CA21_", "", age_vectors))

# Male
age_vectors_male <- paste0("v_CA21_", removed_code + 1)
all(census_vectors$type[census_vectors$vector %in% age_vectors_male] == "Male")

# Female
age_vectors_female <- paste0("v_CA21_", removed_code + 2)
all(census_vectors$type[census_vectors$vector %in% age_vectors_female] == "Female")

# Get the pyramids
get_pyramid <- function(region, vectors) {
  loc <- 
    get_census("CA21", regions = region, level = unlist(names(region)),
               vectors = vectors)
  names(loc) <- gsub(".*\\: ", "", names(loc))
  loc <- loc[which(names(loc) == "1"):ncol(loc)]
  pivot_longer(loc, cols = names(loc), names_to = "age", values_to = "pop") |> 
    group_by(age) |> 
    reframe(pop = sum(pop, na.rm = T))
}

get_pyramid_mixed <- function(region) {
  m <- get_pyramid(region = region, age_vectors_male) |> 
    mutate(genre = "Hommes")
  
  f <- get_pyramid(region = region, age_vectors_female) |> 
    mutate(genre = "Femmes")
  
  t <- merge(m, f, by = "age")
  t$pop <- t$pop.x + t$pop.y
  t <- t[c("age", "pop")]
  t <- tibble::as_tibble(t)
  t$genre = "Total"
  
  rbind(m, f, t) |> 
    group_by(genre) |> 
    mutate(total_pop = sum(pop)) |> 
    mutate(pop_pct = pop / total_pop) |> 
    ungroup() |> 
    select(-total_pop) |> 
  mutate(age = case_when(age == "100 years and over" ~ "100", TRUE ~ age)) |> 
  mutate(age = as.numeric(age))
}

laval <- get_pyramid_mixed(region = list("CSD" = 2465005)) |> mutate(geo = "Laval")
mtl <- get_pyramid_mixed(region = list("CMA" = 24462)) |> mutate(geo = "RMR de Montréal")
qc <- get_pyramid_mixed(region = list("PR" = 24)) |> mutate(geo = "Québec")

population_data <- rbind(laval, rbind(mtl, qc))

population_data$geo <- factor(population_data$geo,
                              levels = c("RMR de Montréal", "Québec", "Laval"))

age_pyramid <- 
  population_data |> 
  filter(genre != "Total") |> 
  mutate(pop_pct = ifelse(genre == "Hommes", -pop_pct, pop_pct)) |> 
  ggplot(aes(x = age, y = pop_pct, color = geo, group = interaction(geo, genre))) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1) + # Apply smoothing with LOESS
  scale_y_continuous(labels = \(x) convert_pct(abs(x)), limits = c(-max(population_data$pop_pct), max(population_data$pop_pct))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Âge", y = "Proportion de la population") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  coord_flip() +
  scale_color_manual(values = c("#E08565", "#73AD80", curbcut_colors$left_5$fill[6])) +
  annotate("text", x = 95, y = -0.01, label = "Hommes", hjust = 0.5, vjust = 0.5, 
           color = "black", size = 3, family = font_local_name) +
  annotate("text", x = 95, y = 0.01, label = "Femmes", hjust = 0.5, vjust = 0.5, 
           color = "black", size = 3, family = font_local_name) +
  graph_theme


ggsave_pdf_png(age_pyramid, "outputs/7/age_pyramid.pdf", width = 6.5, height = 3.5)
write_excel_csv(population_data, file = "data/complementary_data_output/age_pyramid.csv")


# Structure des âges comparés ---------------------------------------------

# Got to extract age through many CSDs for découpage administratif du Québec
census_csds <- get_census("CA21", regions = list(PR = 24), level = "CSD", geo_format = "sf")
census_csds <- sf::st_transform(csds, crs = 32618)
csds <- census_csds
csds$area <- cc.buildr::get_area(census_csds)

ra <- read_sf("data/region_admin/regio_s.shp")
lanau <- ra[ra$RES_NM_REG == "Lanaudière", ]
lanau <- sf::st_transform(lanau, crs = 32618)

lauren <- ra[ra$RES_NM_REG == "Laurentides", ]
lauren <- sf::st_transform(lauren, crs = 32618)

lanau_csds <- 
  csds %>%
  st_intersection(lanau) %>%
  mutate(
    intersection_area = st_area(.),
    overlap_pct = as.numeric(intersection_area / area)
  ) %>%
  filter(overlap_pct >= 0.9) |> 
  pull(GeoUID)
lanau_csds <- list(CSD = lanau_csds)

lauren_csds <- 
  csds %>%
  st_intersection(lauren) %>%
  mutate(
    intersection_area = st_area(.),
    overlap_pct = as.numeric(intersection_area / area)
  ) %>%
  filter(overlap_pct >= 0.9) |> 
  pull(GeoUID)
lauren_csds <- list(CSD = lauren_csds)

# Bind
longueuil <- get_pyramid_mixed(region = list(CD = 2458)) |> mutate(geo = "Longueuil (Agglo)")
laurentides <- get_pyramid_mixed(region = lauren_csds) |> mutate(geo = "Laurentides")
lanaudieres <- get_pyramid_mixed(region = lanau_csds) |> mutate(geo = "Lanaudière")
population_data <- bind_rows(laval, mtl, qc, longueuil, laurentides, lanaudieres)

population_data$geo <- factor(population_data$geo,
                              levels = c("Laurentides", "Lanaudière", "Longueuil (Agglo)", 
                                         "RMR de Montréal", "Québec", "Laval"))

structure_age_compares <- 
  population_data %>%
  filter(genre == "Total") |> 
  mutate(pop_pct = abs(pop_pct)) |> 
  ggplot(aes(x = age, y = pop_pct, color = geo, group = geo)) +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size = 1) +
  scale_y_continuous(labels = convert_pct)  +
  scale_x_continuous(breaks = seq(0, 100, by = 5)) + 
  labs(x = "Âge", y = "Proportion de la population") +
  graph_theme +
  scale_color_manual(values = c("#9E9090" ,"#ADB033", "#F5D574", "#E08565", 
                                "#73AD80", curbcut_colors$left_5$fill[6]))

ggsave_pdf_png(structure_age_compares, "outputs/7/age_structure_compare.pdf", width = 6.5, height = 3.5)
write_excel_csv(population_data, file = "data/complementary_data_output/age_structure_compare.csv")


# Pyramides âges projetés -------------------------------------------------

projected <- 
  read_excel("data/4/PopGrAS_RA_base_2024.xlsx", skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |> 
  rename(`Année` = `...4`,
         `genre` = ...5) |> 
  filter(genre %in% c(1,2,3)) |> 
  select(Année, genre, `0-4`:`100+`) |> 
  mutate_all(as.numeric) |> 
  pivot_longer(cols = `0-4`:`100+`, names_to = "age", values_to = "pop") |> 
  filter(Année %in% c(2021, 2041)) |> 
  mutate(genre = ifelse(genre == 1, "Hommes", ifelse(genre == 2, "Femmes", "Total")))

age_vectors_male <- paste0("v_CA01_", 7:24)
age_vectors_female <- paste0("v_CA01_", 26:43)

# Get the pyramids
get_pyramid_2001 <- function(region, vectors) {
  loc <- 
    get_census("CA01", regions = region, level = unlist(names(region)),
               vectors = vectors)
  names(loc) <- gsub(".*\\: ", "", names(loc))
  loc <- loc[which(names(loc) == "0-4"):ncol(loc)]
  pivot_longer(loc, cols = names(loc), names_to = "age", values_to = "pop") |> 
    group_by(age) |> 
    reframe(pop = sum(pop, na.rm = T)) |> 
    mutate(Année = 2001)
}


m <- get_pyramid_2001(region = list("CSD" = 2465005), vectors = age_vectors_male) |> 
  mutate(genre = "Hommes")
f <- get_pyramid_2001(region = list("CSD" = 2465005), vectors = age_vectors_female) |> 
  mutate(genre = "Femmes")

t <- merge(m, f, by = "age")
t$pop <- t$pop.x + t$pop.y
t <- t[c("age", "pop")]
t <- tibble::as_tibble(t)
t$genre = "Total"
t$`Année` <- 2001

population_data_raw <- bind_rows(m, f, t)


population_data_raw <- 
  rbind(population_data_raw, projected) |> 
  group_by(genre, Année) |> 
  mutate(total_pop = sum(pop)) |> 
  mutate(pop_pct = pop / total_pop) |> 
  ungroup() |> 
  select(-total_pop) |> 
  arrange(`Année`, `genre`)

population_data <- population_data_raw

population_data$age <- 
  stringr::str_extract_all(population_data$age, "\\d*") |> 
  sapply(\(x) as.numeric(x) |> mean(na.rm=T))

population_data$Année <- factor(as.character(population_data$Année),
                                levels = c("2001", "2021", "2041"))

age_pyramid_project <- 
population_data |> 
  filter(genre != "Total") |> 
  mutate(pop_pct = pop_pct / 4) |> 
  mutate(pop_pct = ifelse(genre == "Femmes", -pop_pct, pop_pct)) |> 
  ggplot(aes(x = age, y = pop_pct, color = as.factor(Année), group = interaction(Année, genre))) +
  geom_line(size = 1) +
  scale_y_continuous(labels = \(x) convert_pct(abs(x))) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Âge", y = "Proportion de la population", color = "Année") +
  scale_color_manual(values = curbcut_colors$left_5$fill[c(2,4,6)]) +
  coord_flip() +
  annotate("text", x = 95, y = -0.01, label = "Hommes", hjust = 0.5, vjust = 0.5, 
           color = "black", size = 3, family = font_local_name) +
  annotate("text", x = 95, y = 0.01, label = "Femmes", hjust = 0.5, vjust = 0.5, 
           color = "black", size = 3, family = font_local_name) +
  graph_theme


ggsave_pdf_png(age_pyramid_project, "outputs/7/age_pyramid_project.pdf", width = 6.5, height = 3.5)
population_data_raw_tosave <- population_data_raw
population_data_raw_tosave$age <- paste("Groupe", population_data_raw_tosave$age)
write_excel_csv(population_data_raw_tosave, file = "data/complementary_data_output/age_pyramid_project.csv")


# Proportions groupe d'âge ------------------------------------------------

# Get historical age groups
age_vectors <- cc.data::census_vectors_age
vec_young <- age_vectors[age_vectors$var_code %in% c("age_0_4", "age_5_9", "age_10_14", "age_15_19"), ]
vec_middle <- age_vectors[age_vectors$var_code %in% c("age_20_24", "age_25_29", "age_30_34", "age_35_39",
                                        "age_40_44", "age_45_49", "age_50_54", "age_55_59",
                                        "age_60_64"), ]
vec_old <- age_vectors[age_vectors$var_code %in% c("age_65_69", "age_70_74", "age_75_79", "age_80_84"), ]
vec_older <- age_vectors[age_vectors$var_code %in% c("age_85"), ]

years <- c("vec_2001", "vec_2006", "vec_2011", "vec_2016", "vec_2021")
data_list <- 
  sapply(years, \(y) {
    sapply(list(vec_young, vec_middle, vec_old, vec_older), \(age_group) {
      sapply(age_group[[y]], \(vecs) {
        names(vecs) <- seq_along(vecs)
        z <- get_census("CA01", region = list("CSD" = 2465005), vectors = vecs)
        sum(z[grepl("^[0-9]+$", names(z))])
      }) |> sum()
    }, simplify = TRUE, USE.NAMES = TRUE)
  }, simplify = FALSE, USE.NAMES = TRUE)

df <- tibble::as.tibble(do.call(rbind, data_list))
colnames(df) <- c("0-19", "20-64", "65-84", "85+")
row.names(df) <- NULL
df$`Année` <- c(2001, 2006, 2011, 2016, 2021)

# Projected age groups
projected <-
  read_excel("data/4/PopGrAS_RA_base_2024.xlsx", skip = 5) |> 
  filter(`...3` == "Laval",
         `...1` == "Référence A2024") |> 
  rename(`Année` = `...4`,
         `genre` = ...5) |> 
  filter(genre %in% 3) |> 
  select(Année, `0-4`:`100+`) |> 
  mutate_all(as.numeric) |> 
  transmute(Année,
            `0-19` = `0-4` + `5-9` + `10-14` + `15-19`,
            `20-64` = `20-24` + `25-29` + `30-34` + `35-39` + `40-44` + `45-49` + 
              `50-54` + `55-59` + `60-64`,
            `65-84`= `65-69` + `70-74` + `75-79`+ `80-84`,
            `85+`= `85-89`+ `90-94` + `95-99` + `100+`,
            #`65+` = `65-84` + `85+`
            )

population_data <-  
  bind_rows(df, projected[projected$Année != 2021, ]) |> 
  group_by(Année) |>
  mutate(`65+` = `65-84` + `85+`) |> 
  pivot_longer(
    cols = c("0-19", "20-64", "65-84", "85+", "65+"),
    names_to = "age",
    values_to = "pop"
  )

population_data <- 
population_data |> 
  group_by(Année) |> 
  mutate(total_pop = sum(pop)) |> 
  mutate(pop_pct = pop / total_pop) |> 
  ungroup() |> 
  select(-total_pop) |> 
  mutate(type = ifelse(Année <= 2021, "Réel", "Projetée"))

age_proportion_group <- 
  population_data |> 
  ggplot(aes(x = Année, y = pop_pct, color = age, linetype = type, group = interaction(age, type))) +
  geom_line(size = 1) +
  scale_color_manual(values = curbcut_colors$left_5$fill[c(2,3, 4,5,6)]) + 
  scale_linetype_manual(values = c("Réel" = "solid", "Projetée" = "dotdash")) + 
  geom_text(
    data = population_data %>% filter(Année == 2006 & age != "65-84"),
    aes(label = paste(age, "ans")),
    hjust = 0, # Adjust horizontal position for label placement
    vjust = -1, # Center vertically on the line
    size = 3,
    color = "black"
  ) +
  geom_text(
    data = population_data %>% filter(Année == 2006 & age == "65-84"),
    aes(label = paste(age, "ans")),
    hjust = 0, # Adjust horizontal position for label placement
    vjust = 2, # Center vertically on the line
    size = 3,
    color = "black"
  ) +
  ylab("Proportion de la population") +
  scale_y_continuous(labels = convert_pct) +
  xlab(NULL) +
  ylim(0,0.6) +
  graph_theme


ggsave_pdf_png(age_proportion_group, "outputs/7/age_proportion_group.pdf", width = 6.5, height = 3.5)
write_excel_csv(population_data, file = "data/complementary_data_output/age_proportion_group.csv")

  