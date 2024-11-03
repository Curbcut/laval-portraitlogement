source("R/utils/startup.R")


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


housing_01_CSD <- get_census("CA01", regions = list(CSD = "2465005"), level = "CSD",
                         vectors = vec_2001_511, geo_format = "sf") |> 
  mutate(year = 2001, .after = GeoUID) |> 
  mutate(age_total = tenure_total, .before = age_1946) |> 
  mutate(age_2005 = age_2000 / 6, .after = age_2000) |> 
  mutate(age_2000 = age_2000 * 5 / 6)

housing_06_CSD <- get_census("CA06", regions = list(CSD = "2465005"), level = "CSD",
                         vectors = vec_2006_511, geo_format = "sf") |> 
  mutate(year = 2006, .after = GeoUID) |> 
  mutate(age_2010 = age_2005 / 6, .after = age_2005) |> 
  mutate(age_2005 = age_2005 * 5 / 6)

housing_11_CSD <- get_census("CA11", regions = list(CSD = "2465005"), level = "CSD",
                         vectors = vec_2011_511, geo_format = "sf") |> 
  mutate(year = 2011, .after = GeoUID) |> 
  mutate(age_2015 = age_2010 / 6, .after = age_2010) |> 
  mutate(age_2010 = age_2010 * 5 / 6)

housing_16_CSD <- get_census("CA16", regions = list(CSD = "2465005"), level = "CSD",
                         vectors = vec_2016_511, geo_format = "sf") |> 
  mutate(year = 2016, .after = GeoUID) |> 
  mutate(age_2021 = age_2015 / 6, .after = age_2015) |> 
  mutate(age_2015 = age_2015 * 5 / 6)

housing_21_CSD <- get_census("CA21", regions = list(CSD = "2465005"), level = "CSD",
                         vectors = vec_2021_511, geo_format = "sf") |> 
  mutate(year = 2021, .after = GeoUID)

housing_CSD <-
  bind_rows(housing_01_CSD, housing_06_CSD, housing_11_CSD, 
            housing_16_CSD, housing_21_CSD) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(-key, -`NHS Non-Return Rate`, -`NHS Non Return Rate`, -`Shape Area`, 
         -Type, -Households, -`Quality Flags`, -name, -Population, 
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
    dwellings, \(x) (x[2] - x[1]) / x[1], .before = 1, .complete = TRUE)))
  
can_dwellings$dwellings[5] - can_dwellings$dwellings[1] * 1.0131 ^ 20

laval_dwellings <- 
  housing |> 
  st_drop_geometry() |> 
  summarize(dwellings = sum(dwellings, na.rm = TRUE), .by = year)

laval_dwellings$dwellings[5] - laval_dwellings$dwellings[1] * 1.0133 ^ 20

plot_5_1_1_1_facet <-
  housing_CSD |> 
  st_drop_geometry() |> 
  summarize(across(type_total:type_movable, sum, na.rm = TRUE), .by = year) |> 
  mutate(across(type_single:type_movable, \(x) x / type_total)) |> 
  select(-type_other_single, -type_movable) |> 
  pivot_longer(type_single:type_apart_large) |> 
  mutate(name = str_remove(name, "type_")) |> 
  mutate(name = case_when(name == "single" ~ "Unifamilial",
                          name == "apart_small" ~ "Appartement (<5 étages)",
                          name == "apart_large" ~ "Appartement (5+ étages)",
                          name == "duplex" ~ "Duplex",
                          name == "row" ~ "En rangée",
                          name == "semi" ~ "Jumelé")) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line(lwd=1) +
  gghighlight::gghighlight(use_direct_label = FALSE, unhighlighted_colour = "grey90") +
  scale_y_continuous("Proportion des unités de logement",
                     limits = c(0, 0.5), labels = convert_pct) +
  scale_colour_manual("Type de logement", 
                      values = curbcut_colors$brandbook$color[c(2:4, 7,8,9)]) +
  scale_x_continuous("Year") +
  facet_wrap(vars(name)) +
  theme_minimal() +
  default_theme +
  theme(legend.title = element_blank())

ggsave_pdf_png(filename = here::here("outputs/5/24_proptypologie.pdf"),
                plot = plot_5_1_1_1_facet, width = 9, height = 6)

table_5_1_1_1 <-
  housing_CSD |>
  st_drop_geometry() |>
  summarize(across(type_total:type_movable, sum, na.rm = TRUE), .by = year) |>
  
  # Separate numeric columns for each type, calculating percentage as numeric values
  mutate(across(type_single:type_movable, 
                list(n = ~., pct = ~. / type_total), 
                .names = "{.col}_{.fn}")) |>
  
  # Remove unnecessary columns
  select(year, type_total, type_total_n = type_total, 
         ends_with("_n"), ends_with("_pct"), -type_other_single, -type_movable) |>
  
  # Calculate YoY growth as a numeric percentage
  mutate(yoy = slider::slide_dbl(
    type_total_n, \(x) (x[2] - x[1]) / x[1], .before = 1, .complete = TRUE), 
    .after = type_total_n) |> 
    transmute("Année" = year,
              "Logements" = type_total_n,
              "Croissance inter-recensement" = yoy,
              "Unifamilial (n)" = type_single_n, 
              "Unifamilial (%)" = type_single_pct,
              "Appartement (<5 étages) (n)" = type_apart_small_n,
              "Appartement (<5 étages) (%)" = type_apart_small_pct,
              "Appartement (5+ étages) (n)" = type_apart_large_n,
              "Appartement (5+ étages) (%)" = type_apart_large_pct,
              "Duplex (n)" = type_duplex_n,
              "Duplex (%)" = type_duplex_pct,
              "En rangée (n)" = type_row_n,
              "En rangée (%)" = type_row_pct,
              "Jumelé (n)" = type_semi_n,
              "Jumelé (%)" = type_semi_pct
              ) |> 
  gt::gt() |> 
    data_color(
      columns = c(3, 5,7,9,11,13,15),
      colors = scales::col_numeric(
        palette = c("white", color_theme("purpletransport")),
        domain = NULL, 
      )) |> 
    fmt(columns = c(2, 4,6,8,10,12,14), fns = convert_number) |> 
    fmt(columns = c(3, 5,7,9,11,13,15), fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))) |> 
    tab_style(
      style = cell_text(font = font_local_name, size = px(13)),
      locations = cells_body()) |> 
    tab_style(
      style = cell_text(font = font_local_name),
      locations = cells_column_labels()) |> 
    tab_options(
      table.font.size = 12,
    )

gt_save_word(table_5_1_1_1, "outputs/5/7_dwellingtype.docx")

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
  theme(legend.position = "bottom", legend.key.width = unit(40, "points")) +
  scale_fill_stepsn("Proportion d'unifamilial", 
                    labels = convert_pct,
                    limits = c(0.2, 0.8),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/25_carte_unifamilial.pdf"),
               plot = map_5_1_1_1_single, width = 6.5, height = 4.5)

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
    convert_number(x), " (", convert_pct(x / tenure_total), ")"))) |> 
  mutate(tenure_total = convert_number(tenure_total)) |> 
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
  housing_CSD |> 
  st_drop_geometry() |> 
  summarize(across(bedroom_total:bedroom_four, sum, na.rm = TRUE), 
            .by = year) |> 
  filter(year >= 2011) |> 
  mutate(across(bedroom_zero:bedroom_four, \(x) x / bedroom_total)) |> 
  pivot_longer(bedroom_zero:bedroom_four) |> 
  mutate(value = if_else(value == 0, NA, value)) |> 
  mutate(name = str_remove(name, "bedroom_")) |> 
  mutate(name = case_when(
    name == "zero" ~ "0 CC",
    name == "one" ~ "1 CC",
    name == "two" ~ "2 CC",
    name == "three" ~ "3 CC",
    name == "four" ~ "4+ CC")) |> 
  mutate(name = factor(name, levels = c(
    "0 CC", "1 CC", "2 CC", "3 CC", 
    "4+ CC"))) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line(lwd=1) +
  scale_y_continuous("Proportion des unités de logement",
                     labels = convert_pct) +
  scale_x_continuous(NULL) +
  scale_colour_manual(NULL, 
                      values = curbcut_colors$brandbook$color[c(2:4, 8,9)]) +
  theme_minimal() +
  default_theme

ggsave_pdf_png(filename = here::here("outputs/5/26_tailleCC.pdf"),
                plot = plot_5_1_1_3_facet, width = 6.5, height = 3)


table_5_1_1_3 <-
  housing_CSD |>
  st_drop_geometry() |>
  summarize(across(bedroom_total:bedroom_four, sum, na.rm = TRUE), .by = year) |>
  filter(year >= 2011) |>
  mutate(across(bedroom_zero:bedroom_four, 
                list(n = ~., pct = ~. / bedroom_total), 
                .names = "{.col}_{.fn}")) |>
  mutate(bedroom_total = convert_number(bedroom_total)) |>
  mutate(bedroom_zero_n = c(NA, bedroom_zero_n[2:3])) |>
  transmute("Year" = year,
            "Total" = bedroom_total,
            "0 CC (n)" = bedroom_zero_n, 
            "0 CC (%)" = bedroom_zero_pct,
            "1 CC (n)" = bedroom_one_n,
            "1 CC (%)" = bedroom_one_pct,
            "2 CC (n)" = bedroom_two_n,
            "2 CC (%)" = bedroom_two_pct,
            "3 CC (n)" = bedroom_three_n,
            "3 CC (%)" = bedroom_three_pct,
            "4+ CC (n)" = bedroom_four_n,
            "4+ CC (%)" = bedroom_four_pct)

table_5_1_1_3$`0 CC (%)`[1] <- NA
table_5_1_1_3$`0 CC (n)`[1] <- NA
table_5_1_1_3 <- 
table_5_1_1_3 |> 
  gt::gt() |>
  data_color(
    columns = c(4,6,8,10,12),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    )
  ) |>
  fmt(columns = c(3,5,7,9,11),
      fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_number(y))) |>
  fmt(columns = c(4,6,8,10,12),
      fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))) |>
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()
  ) |>
  tab_options(table.font.size = 12)


gt_save_word(table_5_1_1_3, "outputs/5/8_CC.docx")

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
  theme(legend.position = "bottom", legend.key.width = unit(40, "points")) +
  scale_fill_stepsn("Proportion de 4+ CC", 
                    labels = convert_pct,
                    limits = c(0.05, 0.3),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/27_carte_4CC.pdf"),
                plot = map_5_1_1_3_four, width = 6.5, height = 2.75)

# 5.1.1.4 -------------------------------------------------------------------

plot_5_1_1_4 <-
  housing_CSD |> 
  st_drop_geometry() |> 
  summarize(across(age_total:age_2021, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(age_1960:age_2021, \(x) x / age_total)) |> 
  pivot_longer(age_1960:age_2021) |> 
  mutate(value = if_else(value == 0, NA, value)) |> 
  mutate(name = str_remove(name, "age_")) |> 
  mutate(name = case_when(
    name == "1960" ~ "1960 ou avant",
    name == "1980" ~ "1961-1980",
    name == "1990" ~ "1981-1990",
    name == "2000" ~ "1991-2000",
    name == "2005" ~ "2001-2005",
    name == "2010" ~ "2006-2010",
    name == "2015" ~ "2011-2015",
    name == "2021" ~ "2016-2021")) |>
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  geom_line(lwd=1) +
  scale_y_continuous("Proportion des unités de logement",
                     labels = convert_pct) +
  scale_x_continuous(NULL) +
  scale_colour_manual(NULL, 
                      values = curbcut_colors$brandbook$color[c(2:9)]) +
  theme_minimal() +
  default_theme

ggsave_pdf_png(filename = here::here("outputs/5/28_anneeconstruction.pdf"),
                plot = plot_5_1_1_4, width = 6.5, height = 3.5)

table_5_1_1_4 <-
  housing |> 
  st_drop_geometry() |> 
  summarize(across(age_total:age_2021, sum, na.rm = TRUE), 
            .by = year) |> 
  mutate(across(age_1960:age_2021, \(x) paste0(
    convert_number(x), " (", convert_pct(
      x / age_total), ")"))) |> 
  mutate(age_total = convert_number(age_total)) |> 
  set_names(c("Year", "Total", "1960 or earlier", "1961-1980", "1981-1990", 
              "1991-2000", "2001-2005", "2006-2010", "2011-2015", 
              "2016-2021")) |> 
  gt::gt() |> 
  gt::tab_header(
    "Privately occupied dwelling units by building year of construction")

housing_age_table <- 
  housing_CSD |>
  st_drop_geometry() |>
  summarize(across(age_total:age_2021, sum, na.rm = TRUE), .by = year) |>
  
  # Separate numeric values and percentages for each age range
  mutate(across(age_1960:age_2021, 
                list(n = ~replace(., . == 0, NA), # Replace 0 with NA in counts
                     pct = ~replace(. / age_total, . == 0, NA)), # Replace 0 percentages with NA
                .names = "{.col}_{.fn}")) |>
  
  # Convert age_total and individual age range counts to formatted numbers
  mutate(age_total = convert_number(age_total)) |>
  
  # Rename columns for clarity
  transmute("Year" = year,
            "Total" = age_total,
            "1960 ou avant (n)" = age_1960_n,
            "1960 ou avant (%)" = age_1960_pct,
            "1961-1980 (n)" = age_1980_n,
            "1961-1980 (%)" = age_1980_pct,
            "1981-1990 (n)" = age_1990_n,
            "1981-1990 (%)" = age_1990_pct,
            "1991-2000 (n)" = age_2000_n,
            "1991-2000 (%)" = age_2000_pct,
            "2001-2005 (n)" = age_2005_n,
            "2001-2005 (%)" = age_2005_pct,
            "2006-2010 (n)" = age_2010_n,
            "2006-2010 (%)" = age_2010_pct,
            "2011-2015 (n)" = age_2015_n,
            "2011-2015 (%)" = age_2015_pct,
            "2016-2021 (n)" = age_2021_n,
            "2016-2021 (%)" = age_2021_pct) |>
  
  # Apply gt formatting
  gt::gt() |>

  # Add color to percentage columns
  data_color(
    columns = c("1960 ou avant (%)", "1961-1980 (%)", "1981-1990 (%)",
                "1991-2000 (%)", "2001-2005 (%)", "2006-2010 (%)", 
                "2011-2015 (%)", "2016-2021 (%)"),
    colors = scales::col_numeric(
      palette = c("white", color_theme("purpletransport")),
      domain = NULL
    ), direction = "row"
  ) |>
  
  # Format numeric columns
  fmt(columns = c("1960 ou avant (n)", "1961-1980 (n)", 
                  "1981-1990 (n)", "1991-2000 (n)", "2001-2005 (n)", 
                  "2006-2010 (n)", "2011-2015 (n)", "2016-2021 (n)"),
      fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_number(y))) |>
  
  # Format percentage columns
  fmt(columns = c("1960 ou avant (%)", "1961-1980 (%)", "1981-1990 (%)",
                  "1991-2000 (%)", "2001-2005 (%)", "2006-2010 (%)", 
                  "2011-2015 (%)", "2016-2021 (%)"),
      fns = \(x) sapply(x, \(y) if (is.na(y)) NA else convert_pct(y))) |>
  
  # Apply custom font and styling
  tab_style(
    style = cell_text(font = font_local_name, size = px(13)),
    locations = cells_body()
  ) |>
  tab_style(
    style = cell_text(font = font_local_name),
    locations = cells_column_labels()
  ) |>
  
  # General table options
  tab_options(table.font.size = 12)


gt_save_word(housing_age_table, "outputs/5/9_ageconstruction.docx")

map_5_1_1_4 <-
  housing |> 
  filter(year == 2021) |> 
  select(NOM, age_total:age_2021) |> 
  mutate(across(age_1960:age_2021, \(x) x / age_total)) |> 
  pivot_longer(age_1960:age_2021) |> 
  mutate(name = str_remove(name, "age_")) |> 
  mutate(name = case_when(
    name == "1960" ~ "1960 ou avant",
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
  scale_fill_stepsn("Année de construction", 
                    labels = convert_pct,
                    limits = c(0, 0.3),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/29_carte_anneesconstruction.pdf"),
                plot = map_5_1_1_4, width = 7.5, height = 7)

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
  mutate(subsid = paste0(convert_number(subsid), " (", 
                         convert_pct(subsid / subsid_total), ")")) |> 
  mutate(subsid_total = convert_number(subsid_total)) |> 
  set_names(c("Year", "Ménages locataires", "Dans les logements subventionnés")) |> 
  gt::gt()

gt_save_word(table_5_1_1_5, "outputs/5/10_logementssubventionnes.docx")

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
  theme(legend.position = "bottom", legend.key.width = unit(40, "points")) +
  scale_fill_stepsn("Dans les logements subventionnés", 
                    labels = convert_pct,
                    limits = c(0, 0.15),
                    colours = curbcut_colors$left_5$fill[2:6]) +
  gg_cc_theme +
  theme(legend.key.width = unit(2, "cm"),
        legend.title.position = "top")

ggsave_pdf_png(filename = here::here("outputs/5/30_carte_subventionnes.pdf"),
                plot = map_5_1_1_5, width = 6.5, height = 3)

# Save --------------------------------------------------------------------

qs::qsavem(housing, housing_CSD, plot_5_1_1_1_facet, table_5_1_1_1, map_5_1_1_1_single, 
           plot_5_1_1_2_facet, table_5_1_1_2, map_5_1_1_2_owner,
           plot_5_1_1_3_facet, table_5_1_1_3, map_5_1_1_3_four,
           plot_5_1_1_4, table_5_1_1_4, map_5_1_1_4, can_subsid, qc_subsid,
           table_5_1_1_5, map_5_1_1_5,
           file = "data/section_5_1_1.qsm")
