source("R/startup.R")


# Census vectors ----------------------------------------------------------

vec_2001_511 <- c(
  type_total = "v_CA01_112",
  type_single = "v_CA01_113",
  type_semi = "v_CA01_114",
  type_row = "v_CA01_115",
  type_duplex = "v_CA01_116",
  type_apart_small = "v_CA01_118",
  type_apart_large = "v_CA01_117",
  type_other_single = "v_CA01_119",
  type_movable = "v_CA01_120",
  tenure_total = "v_CA01_96",
  tenure_owner = "v_CA01_99",
  tenure_renter = "v_CA01_100",
  # age_total = "v_CA01_96", # Same as tenure parent, so just duplicate
  age_1946 = "v_CA01_105",
  age_1960 = "v_CA01_106",
  age_1970 = "v_CA01_107",
  age_1980 = "v_CA01_108",
  age_1990 = "v_CA01_109",
  age_1995 = "v_CA01_110",
  age_2000 = "v_CA01_111")

vec_2006_511 <- c(
  type_total = "v_CA06_119",
  type_single = "v_CA06_120",
  type_semi = "v_CA06_121",
  type_row = "v_CA06_122",
  type_duplex = "v_CA06_123",
  type_apart_small = "v_CA06_125",
  type_apart_large = "v_CA06_124",
  type_other_single = "v_CA06_126",
  type_movable = "v_CA06_127",
  tenure_total = "v_CA06_101",
  tenure_owner = "v_CA06_102",
  tenure_renter = "v_CA06_103",
  age_total = "v_CA06_109",
  age_1946 = "v_CA06_110",
  age_1960 = "v_CA06_111",
  age_1970 = "v_CA06_112",
  age_1980 = "v_CA06_113",
  age_1985 = "v_CA06_114",
  age_1990 = "v_CA06_115",
  age_1995 = "v_CA06_116",
  age_2000 = "v_CA06_117",
  age_2005 = "v_CA06_118")

vec_2011_511 <- c(
  type_total = "v_CA11F_199",
  type_single = "v_CA11F_200",
  type_semi = "v_CA11F_204",
  type_row = "v_CA11F_205",
  type_duplex = "v_CA11F_206",
  type_apart_small = "v_CA11F_207",
  type_apart_large = "v_CA11F_201",
  type_other_single = "v_CA11F_208",
  type_movable = "v_CA11F_202",
  condo_total = "v_CA11N_2256",
  condo_condo = "v_CA11N_2257",
  bedroom_total = "v_CA11N_2247",
  bedroom_one = "v_CA11N_2248", # Zero or one
  bedroom_two = "v_CA11N_2249",
  bedroom_three = "v_CA11N_2250",
  bedroom_four = "v_CA11N_2251",
  tenure_total = "v_CA11N_2252",
  tenure_owner = "v_CA11N_2253",
  tenure_renter = "v_CA11N_2254",
  age_total = "v_CA11N_2233",
  age_1960 = "v_CA11N_2234",
  age_1980 = "v_CA11N_2235",
  age_1990 = "v_CA11N_2236",
  age_2000 = "v_CA11N_2237",
  age_2005 = "v_CA11N_2238",
  age_2010 = "v_CA11N_2239",
  subsid_total = "v_CA11N_2288",
  subsid = "v_CA11N_2289")

vec_2016_511 <- c(
  type_total = "v_CA16_408",
  type_single = "v_CA16_409",
  type_semi = "v_CA16_412",
  type_row = "v_CA16_413",
  type_duplex = "v_CA16_414",
  type_apart_small = "v_CA16_415",
  type_apart_large = "v_CA16_410",
  type_other_single = "v_CA16_416",
  type_movable = "v_CA16_417",
  condo_total = "v_CA16_4840",
  condo_condo = "v_CA16_4841",
  bedroom_total = "v_CA16_4843",
  bedroom_zero = "v_CA16_4844",
  bedroom_one = "v_CA16_4845",
  bedroom_two = "v_CA16_4846",
  bedroom_three = "v_CA16_4847",
  bedroom_four = "v_CA16_4848",
  tenure_total = "v_CA16_4836",
  tenure_owner = "v_CA16_4837",
  tenure_renter = "v_CA16_4838",
  age_total = "v_CA16_4862",
  age_1960 = "v_CA16_4863",
  age_1980 = "v_CA16_4864",
  age_1990 = "v_CA16_4865",
  age_2000 = "v_CA16_4866",
  age_2005 = "v_CA16_4867",
  age_2010 = "v_CA16_4868",
  age_2015 = "v_CA16_4869",
  subsid_total = "v_CA16_4897",
  subsid = "v_CA16_4898")

vec_2021_511 <- c(
  type_total = "v_CA21_434",
  type_single = "v_CA21_435",
  type_semi = "v_CA21_436",
  type_row = "v_CA21_437",
  type_duplex = "v_CA21_438",
  type_apart_small = "v_CA21_439",
  type_apart_large = "v_CA21_440",
  type_other_single = "v_CA21_441",
  type_movable = "v_CA21_442",
  condo_total = "v_CA21_4241",
  condo_condo = "v_CA21_4242",
  bedroom_total = "v_CA21_4244",
  bedroom_zero = "v_CA21_4245",
  bedroom_one = "v_CA21_4246",
  bedroom_two = "v_CA21_4247",
  bedroom_three = "v_CA21_4248",
  bedroom_four = "v_CA21_4249",
  tenure_total = "v_CA21_4237",
  tenure_owner = "v_CA21_4238",
  tenure_renter = "v_CA21_4239",
  tenure_gov = "v_CA21_4240",
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271",
  subsid_total = "v_CA21_4313",
  subsid = "v_CA21_4314")


# Get census data ---------------------------------------------------------

housing_01 <- get_census("CA01", regions = list(CSD = "2465005"), level = "CT",
                         vectors = vec_2001_511, geo_format = "sf") |> 
  mutate(year = 2001, .after = GeoUID) |> 
  mutate(age_total = tenure_total, .before = age_1946) |> 
  mutate(age_2005 = age_2000 / 6, .after = age_2000) |> 
  mutate(age_2000 = age_2000 * 5 / 6)

housing_06 <- get_census("CA06", regions = list(CSD = "2465005"), level = "CT",
                         vectors = vec_2006_511, geo_format = "sf") |> 
  mutate(year = 2006, .after = GeoUID) |> 
  mutate(age_2010 = age_2005 / 6, .after = age_2005) |> 
  mutate(age_2005 = age_2005 * 5 / 6)

housing_11 <- get_census("CA11", regions = list(CSD = "2465005"), level = "CT",
                         vectors = vec_2011_511, geo_format = "sf") |> 
  mutate(year = 2011, .after = GeoUID) |> 
  mutate(age_2015 = age_2010 / 6, .after = age_2010) |> 
  mutate(age_2010 = age_2010 * 5 / 6)

housing_16 <- get_census("CA16", regions = list(CSD = "2465005"), level = "CT",
                         vectors = vec_2016_511, geo_format = "sf") |> 
  mutate(year = 2016, .after = GeoUID) |> 
  mutate(age_2021 = age_2015 / 6, .after = age_2015) |> 
  mutate(age_2015 = age_2015 * 5 / 6)

housing_21 <- get_census("CA21", regions = list(CSD = "2465005"), level = "CT",
                         vectors = vec_2021_511, geo_format = "sf") |> 
  mutate(year = 2021, .after = GeoUID)

housing <-
  bind_rows(housing_01, housing_06, housing_11, housing_16, housing_21) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(-key, -`NHS Non-Return Rate`, -`NHS Non Return Rate`, -`Shape Area`, 
         -Type, -Households, -`Quality Flags`, -name, -CSD_UID, -Population, 
         -`Adjusted Population (previous Census)`, -c(CD_UID:`Area (sq km)`),
         -CMA_UID, -PR_UID) |> 
  rename(dwellings = Dwellings) |> 
  relocate(dwellings, .after = year) |> 
  relocate(age_2005, age_2010, age_2015, age_2021, .after = age_2000) |> 
  relocate(bedroom_zero, .before = bedroom_one) |>
  relocate(bedroom_four, .after = bedroom_three) |> 
  relocate(tenure_gov, .after = tenure_renter) |> 
  relocate(subsid_total, subsid, .after = bedroom_four) |> 
  mutate(across(c(age_total:age_2021, age_1985), replace_na, replace = 0)) |> 
  mutate(age_1960 = age_1946 + age_1960,
         age_1980 = age_1970 + age_1980,
         age_1990 = age_1985 + age_1990,
         age_2000 = age_1995 + age_2000) |> 
  select(-age_1946, -age_1970, -age_1985, -age_1995)
    
# Interpolate
housing <- 
  housing |> 
  mutate(subsid = subsid / 100 * subsid_total) |> 
  interpolate(group = "year", additive_vars = c(
    "dwellings", "type_total", "type_single", "type_semi", "type_row", 
    "type_duplex", "type_apart_small", "type_apart_large", "type_other_single",
    "type_movable", "age_total", "age_1960", "age_1980", "age_1990", "age_2000", 
    "age_2005", "age_2010", "age_2015", "age_2021", "tenure_total", 
    "tenure_owner", "tenure_renter", "tenure_gov", "condo_total", "condo_condo", 
    "bedroom_total", "bedroom_zero", "bedroom_one", "bedroom_two", 
    "bedroom_three", "bedroom_four", "subsid_total", "subsid"))


# 5.1.1.1 Répartition des logements selon le typologie --------------------

can_dwellings <- 
  bind_rows(
  get_census("CA01", regions = list(C = "1")),
  get_census("CA06", regions = list(C = "1")),
  get_census("CA11", regions = list(C = "1")),
  get_census("CA16", regions = list(C = "1")),
  get_census("CA21", regions = list(C = "1"))) |> 
  select(dwellings = Dwellings) |> 
  mutate(yoy = convert_pct(slider::slide_dbl(
    dwellings, \(x) (x[2] - x[1]) / x[1], .before = 1, .complete = TRUE), 0.1))
  
can_dwellings$dwellings[5] - can_dwellings$dwellings[1] * 1.0131 ^ 20

laval_dwellings <- 
  housing |> 
  st_drop_geometry() |> 
  summarize(dwellings = sum(dwellings, na.rm = TRUE), .by = year)

laval_dwellings$dwellings[5] - laval_dwellings$dwellings[1] * 1.0133 ^ 20

plot_5_1_1_1_facet <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(type_total:type_movable, sum, na.rm = TRUE), .by = year) |> 
  mutate(across(type_single:type_movable, \(x) x / type_total)) |> 
  select(-type_other_single, -type_movable) |> 
  pivot_longer(type_single:type_apart_large) |> 
  mutate(name = str_remove(name, "type_")) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line() +
  gghighlight::gghighlight(use_direct_label = FALSE) +
  scale_y_continuous("Share of all private dwelling units",
                     limits = c(0, 0.5), labels = convert_pct) +
  scale_x_continuous("Year") +
  facet_wrap(vars(name)) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = here::here("outputs/5/plot_5_1_1_1_facet.pdf"),
                plot = plot_5_1_1_1_facet, width = 7.5, height = 6)

table_5_1_1_1 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(type_total:type_movable, sum, na.rm = TRUE), .by = year) |> 
  mutate(across(type_single:type_movable, \(x) paste0(
    convert_number(x, 10), " (", convert_pct(x / type_total, 0.1), ")"))) |> 
  select(-type_other_single, -type_movable) |>
  mutate(yoy = convert_pct(slider::slide_dbl(
    type_total, \(x) (x[2] - x[1]) / x[1], .before = 1, .complete = TRUE), 0.1), 
    .after = type_total) |> 
  mutate(type_total = convert_number(type_total, 10)) |> 
  set_names(c("Year", "Total", "Inter-census growth", "Single-detached", 
              "Semi-detached", "Row", "Duplex"), 
            "Apartment (fewer than 5 storeys)", 
            "Apartment (5 or more storeys)") |> 
  gt::gt() |> 
  gt::tab_header("Privately occupied dwelling units by type")

gtsave(table_5_1_1_1, "outputs/5/table_5_1_1_1.png", zoom = 1)

map_5_1_1_1_single <-
  housing |> 
    mutate(single_pct = type_single / type_total) |> 
    ggplot(aes(fill = single_pct)) +
    geom_sf(colour = "white", lwd = 0.1) +
    scale_fill_viridis_b(
      "Share of all private dwelling units which are single-detached homes",
      labels = convert_pct, n.breaks = 6) +
    facet_wrap(vars(year)) +
    theme_void() +
    theme(legend.position = "bottom", legend.key.width = unit(40, "points"))

ggplot2::ggsave(filename = here::here("outputs/5/map_5_1_1_1_single.pdf"),
                plot = map_5_1_1_1_single, width = 7.5, height = 6)

# 5.1.1.2 -------------------------------------------------------------------

plot_5_1_1_2_facet <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(tenure_total:tenure_renter, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(tenure_owner:tenure_renter, \(x) x / tenure_total)) |> 
  pivot_longer(tenure_owner:tenure_renter) |> 
  mutate(name = str_remove(name, "tenure_")) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Share of all private dwelling units",
                     limits = c(0.2, 0.8), labels = convert_pct) +
  scale_x_continuous("Year") +
  scale_colour_discrete("Tenure") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = here::here("outputs/5/plot_5_1_1_2_facet.pdf"),
                plot = plot_5_1_1_2_facet, width = 7.5, height = 6)

table_5_1_1_2 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(tenure_total:tenure_renter, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(tenure_owner:tenure_renter, \(x) paste0(
    convert_number(x, 10), " (", convert_pct(x / tenure_total, 0.1), ")"))) |> 
  mutate(tenure_total = convert_number(tenure_total, 10)) |> 
  set_names(c("Year", "Total", "Owner-occupied", "Tenant-occupied")) |> 
  gt::gt() |> 
  gt::tab_header("Privately occupied dwelling units by tenure")

gtsave(table_5_1_1_2, "outputs/5/table_5_1_1_2.png", zoom = 1)

map_5_1_1_2_owner <-
  housing |> 
  mutate(owner_pct = tenure_owner / tenure_total) |> 
  ggplot(aes(fill = owner_pct)) +
  geom_sf(colour = "white", lwd = 0.1) +
  scale_fill_viridis_b(
    "Share of all private dwelling units which are owner-occupied",
    labels = convert_pct, n.breaks = 6) +
  facet_wrap(vars(year)) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(40, "points"))

ggplot2::ggsave(filename = here::here("outputs/5/map_5_1_1_2_owner.pdf"),
                plot = map_5_1_1_2_owner, width = 7.5, height = 6)

# 5.1.1.3 -------------------------------------------------------------------

plot_5_1_1_3_facet <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(bedroom_total:bedroom_four, sum, na.rm = TRUE), 
            .by = year) |> 
  filter(year >= 2011) |> 
  mutate(across(bedroom_zero:bedroom_four, \(x) x / bedroom_total)) |> 
  pivot_longer(bedroom_zero:bedroom_four) |> 
  mutate(value = if_else(value == 0, NA, value)) |> 
  mutate(name = str_remove(name, "bedroom_")) |> 
  mutate(name = case_when(
    name == "zero" ~ "Zero bedrooms",
    name == "one" ~ "One bedroom",
    name == "two" ~ "Two bedrooms",
    name == "three" ~ "Three bedrooms",
    name == "four" ~ "Four or more bedrooms")) |> 
  mutate(name = factor(name, levels = c(
    "Zero bedrooms", "One bedroom", "Two bedrooms", "Three bedrooms", 
    "Four or more bedrooms"))) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Share of all private dwelling units",
                     labels = convert_pct) +
  scale_x_continuous("Year") +
  scale_colour_discrete(NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = here::here("outputs/5/plot_5_1_1_3_facet.pdf"),
                plot = plot_5_1_1_3_facet, width = 7.5, height = 6)

table_5_1_1_3 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(bedroom_total:bedroom_four, sum, na.rm = TRUE), 
            .by = year) |> 
  filter(year >= 2011) |> 
  mutate(across(bedroom_zero:bedroom_four, \(x) paste0(
    convert_number(x, 10), " (", convert_pct(
      x / bedroom_total, 0.1), ")"))) |> 
  mutate(bedroom_total = convert_number(bedroom_total, 10)) |> 
  mutate(bedroom_zero = c(NA, bedroom_zero[2:3])) |> 
  set_names(c("Year", "Total", "Zero bedrooms", "One bedroom", "Two bedrooms", 
              "Three bedrooms", "Four or more bedrooms")) |> 
  gt::gt() |> 
  gt::tab_header("Privately occupied dwelling units by bedroom count")

gtsave(table_5_1_1_3, "outputs/5/table_5_1_1_3.png", zoom = 1)

map_5_1_1_3_four <-
  housing |> 
  mutate(four_pct = bedroom_four / bedroom_total) |> 
  filter(year >= 2011) |> 
  ggplot(aes(fill = four_pct)) +
  geom_sf(colour = "white", lwd = 0.1) +
  scale_fill_viridis_b(
    "Share of all private dwelling units which have four or more bedrooms",
    labels = convert_pct, n.breaks = 6) +
  facet_wrap(vars(year)) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(40, "points"))

ggplot2::ggsave(filename = here::here("outputs/5/map_5_1_1_3_four.pdf"),
                plot = map_5_1_1_3_four, width = 7.5, height = 6)

# 5.1.1.4 -------------------------------------------------------------------

plot_5_1_1_4 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(age_total:age_2021, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(age_1960:age_2021, \(x) x / age_total)) |> 
  pivot_longer(age_1960:age_2021) |> 
  mutate(value = if_else(value == 0, NA, value)) |> 
  mutate(name = str_remove(name, "age_")) |> 
  mutate(name = case_when(
    name == "1960" ~ "1960 or earlier",
    name == "1980" ~ "1961-1980",
    name == "1990" ~ "1981-1990",
    name == "2000" ~ "1991-2000",
    name == "2005" ~ "2001-2005",
    name == "2010" ~ "2006-2010",
    name == "2015" ~ "2011-2015",
    name == "2021" ~ "2016-2021")) |>
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line() +
  scale_y_continuous("Share of all private dwelling units",
                     labels = convert_pct) +
  scale_x_continuous("Year") +
  scale_colour_discrete(NULL) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot2::ggsave(filename = here::here("outputs/5/plot_5_1_1_4.pdf"),
                plot = plot_5_1_1_4, width = 7.5, height = 6)

table_5_1_1_4 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(age_total:age_2021, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(age_1960:age_2021, \(x) paste0(
    convert_number(x, 10), " (", convert_pct(
      x / age_total, 0.1), ")"))) |> 
  mutate(age_total = convert_number(age_total, 10)) |> 
  set_names(c("Year", "Total", "1960 or earlier", "1961-1980", "1981-1990", 
              "1991-2000", "2001-2005", "2006-2010", "2011-2015", 
              "2016-2021")) |> 
  gt::gt() |> 
  gt::tab_header(
    "Privately occupied dwelling units by building year of construction")

gtsave(table_5_1_1_4, "outputs/5/table_5_1_1_4.png", zoom = 1)

map_5_1_1_4 <-
  housing |> 
  filter(year == 2021) |> 
  select(NOM, age_total:age_2021) |> 
  mutate(across(age_1960:age_2021, \(x) x / age_total)) |> 
  pivot_longer(age_1960:age_2021) |> 
  mutate(name = str_remove(name, "age_")) |> 
  mutate(name = case_when(
    name == "1960" ~ "1960 or earlier",
    name == "1980" ~ "1961-1980",
    name == "1990" ~ "1981-1990",
    name == "2000" ~ "1991-2000",
    name == "2005" ~ "2001-2005",
    name == "2010" ~ "2006-2010",
    name == "2015" ~ "2011-2015",
    name == "2021" ~ "2016-2021")) |>
  ggplot(aes(fill = value)) +
  geom_sf(colour = "white", lwd = 0.1) +
  scale_fill_viridis_b(
    "Share of 2021 private dwelling units by construction year",
    labels = convert_pct, n.breaks = 10) +
  facet_wrap(vars(name)) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(50, "points"))

ggplot2::ggsave(filename = here::here("outputs/5/map_5_1_1_4.pdf"),
                plot = map_5_1_1_4, width = 7.5, height = 6)

# 5.1.1.5 -------------------------------------------------------------------

can_subsid <- get_census("CA21", list(C = "01"), vectors = c(
  subsid_total = "v_CA21_4313", subsid = "v_CA21_4314"))

qc_subsid <- get_census("CA21", list(PR = "24"), vectors = c(
  subsid_total = "v_CA21_4313", subsid = "v_CA21_4314"))

# plot_5_1_1_5 <-
#   housing |> 
#   st_drop_geometry() |> 
#   filter(year >= 2011) |> 
#   select(NOM, year, subsid_total, subsid) |> 
#   summarize(across(subsid_total:subsid, sum), .by = year) |> 
#   mutate(pct = subsid / subsid_total) |> 
#   ggplot(aes(year, pct)) +
#   geom_line() +
#   scale_y_continuous("Share of tenant households in subsidized housing",
#                      labels = convert_pct, limits = c(0.05, 0.08)) +
#   scale_x_continuous("Year") +
#   theme_minimal()

table_5_1_1_5 <-
  housing |> 
  st_drop_geometry() |>
  filter(year >= 2011) |> 
  select(NOM, year, subsid_total, subsid) |> 
  summarize(across(subsid_total:subsid, sum, na.rm = TRUE), .by = year) |> 
  mutate(subsid = paste0(convert_number(subsid, 10), " (", 
                         convert_pct(subsid / subsid_total, 0.1), ")")) |> 
  mutate(subsid_total = convert_number(subsid_total, 10)) |> 
  set_names(c("Year", "Total tenant households", "In subsidized housing")) |> 
  gt::gt() |> 
  gt::tab_header("Share of tenant households in subsidized housing")

gtsave(table_5_1_1_5, "outputs/5/table_5_1_1_5.png", zoom = 1)

map_5_1_1_5 <-
  housing |> 
  filter(year >= 2011) |> 
  select(NOM, year, subsid_total, subsid) |> 
  mutate(pct = subsid / subsid_total) |> 
  ggplot(aes(fill = pct)) +
  geom_sf(colour = "white", lwd = 0.1) +
  scale_fill_viridis_b(
    "Share of tenant households in subsidized housing",
    labels = convert_pct, n.breaks = 6) +
  facet_wrap(vars(year)) +
  theme_void() +
  theme(legend.position = "bottom", legend.key.width = unit(40, "points"))

ggplot2::ggsave(filename = here::here("outputs/5/map_5_1_1_5.pdf"),
                plot = map_5_1_1_5, width = 7.5, height = 6)

# Save --------------------------------------------------------------------

qs::qsavem(housing, plot_5_1_1_1_facet, table_5_1_1_1, map_5_1_1_1_single, 
           plot_5_1_1_2_facet, table_5_1_1_2, map_5_1_1_2_owner,
           plot_5_1_1_3_facet, table_5_1_1_3, map_5_1_1_3_four,
           plot_5_1_1_4, table_5_1_1_4, map_5_1_1_4, can_subsid, qc_subsid,
           table_5_1_1_5, map_5_1_1_5,
           file = "data/section_5_1_1.qsm")
