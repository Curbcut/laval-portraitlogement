#### Target calculations #######################################################

source("R/utils/startup.R")


# Households --------------------------------------------------------------

# Import ISQ projections
isq <-
  read_excel("data/isq.xlsx", skip = 5) |> 
  suppressMessages() |> 
  set_names(c("scenario", "code", "region", paste0("year_", 2021:2051))) |> 
  slice(-1) |> 
  filter(region == "Laval") |> 
  select(-code, -region) |> 
  mutate(year_2021 = as.numeric(year_2021)) |> 
  pivot_longer(-scenario, names_to = "year") |> 
  mutate(year = as.numeric(str_remove(year, "year_"))) |> 
  relocate(year) |> 
  mutate(scenario = case_when(
    scenario == "Faible D2024" ~ "weak",
    scenario == "Référence A2024" ~ "reference",
    scenario == "Fort E2024" ~ "strong"))

# Visualization
plot_isq_households <- 
  isq |> 
  ggplot(aes(year, value, colour = scenario)) +
  geom_point() +
  scale_x_continuous(NULL) + 
  scale_y_continuous("Households", labels = scales::comma) +
  scale_colour_manual(NULL, labels = c("Reference scenario", "Strong scenario", 
                                       "Weak scenario"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Occupancy rate ----------------------------------------------------------

# Get raw occupancy ratios from 2001 through 2021
occ_raw <- 
  map(c("CA01", "CA06", "CA11", "CA16", "CA21"), \(x) {
    get_census(x, regions = list(CSD = "2465005")) |> 
      suppressMessages() |> 
      mutate(year = x)}) |> 
  bind_rows() |> 
  mutate(year = c(2001, 2006, 2011, 2016, 2021),
         occ_rate = Households / Dwellings, .keep = "none",
         type = "actual")

# Create high scenario with linear relationship between year and occupancy rate
occ_model <- lm(occ_rate ~ year, data = occ_raw)

# Predict new annual data points using this linear trend
occ_strong <- 
  tibble(year = 2022:2051, 
         occ_rate = predict(occ_model, tibble(year = 2022:2051)),
         type = "strong")

# Create weak scenario with exponential decay trend
occ_weak <- 
  tibble(year = 2022:2051, 
         occ_rate = 0.01 * 0.958 ^ (year - 2021) + 0.954,
         type = "weak")

# Bind rows
occ_rate <- bind_rows(occ_raw, occ_strong, occ_weak)

# Visualization
plot_occ_rate <- 
  occ_rate |> 
  ggplot(aes(year, occ_rate, colour = type)) +
  stat_function(fun = \(x) -0.0006591 * x + 2.2956686, lwd = 0.2,
                colour = curbcut_colors$brandbook$color[3]) +
  stat_function(fun = \(x) 0.01 * 0.958 ^ (x - 2021) + 0.954, lwd = 0.2,
                colour = curbcut_colors$brandbook$color[2]) +
  geom_point() +
  scale_x_continuous(NULL) + 
  scale_y_continuous("Occupancy rate", limits = c(0.92, 1), 
                     labels = scales::percent) +
  scale_colour_manual(NULL, labels = c("Actual values", "Strong scenario", 
                                       "Weak scenario"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Total required dwelling units -------------------------------------------

# Join ISQ and occupancy data
dwelling_targets <- 
  isq |> 
  pivot_wider(names_from = scenario, names_prefix = "hh_") |> 
  inner_join({
    occ_rate |> 
      filter(type != "actual") |> 
      pivot_wider(names_from = type, names_prefix = "occ_", 
                  values_from = occ_rate)}, by = "year")

# Create six scenarios
dwelling_targets <- 
  dwelling_targets |> 
  mutate(
    scn_ref_weak = hh_reference / occ_weak,
    scn_ref_strong = hh_reference / occ_strong,
    scn_weak_weak = hh_weak / occ_weak,
    scn_weak_strong = hh_weak / occ_strong,
    scn_strong_weak = hh_strong / occ_weak,
    scn_strong_strong = hh_strong / occ_strong) |> 
  select(-c(hh_reference:occ_weak))

# Visualizations
plot_dwelling_targets <- 
  dwelling_targets |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Total needed dwelling units", labels = scales::comma) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, values = curbcut_colors$brandbook$color[c(
    3, 8, 2, 6, 5, 4)], labels = c(
      "Ref. ISQ, strong occ. rate change",
      "Ref. ISQ, weak occ. rate change",
      "Strong ISQ, strong occ. rate change",
      "Strong ISQ, weak occ. rate change",
      "Weak ISQ, strong occ. rate change",
      "Weak ISQ, weak occ. rate change")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Total 2021 dwellings ----------------------------------------------------

dwellings_2021 <- 
  get_census("CA21", regions = list(CSD = "2465005")) |> 
  pull(Dwellings)


# Dwelling attrition rate -------------------------------------------------

# Obtain total private dwellings from each census period
dwellings <- 
  map(c("CA1996", "CA01", "CA06", "CA11", "CA16", "CA21"), \(x) {
    get_census(x, regions = list(CSD = "2465005"))}) |> 
  bind_rows() |> 
  select(dwellings = Dwellings) |> 
  mutate(year = c(1996, 2001, 2006, 2011, 2016, 2021), .before = dwellings)

# Obtain monthly completions
completions <- 
  get_cmhc("Scss", "Completions", "Dwelling Type", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Dwelling Type` == "All") |> 
  select(date = Date, value = Value)

# Consolidate completions into census periods
completions <- 
  completions |> 
  filter(date <= "2021-04-01", date >= "1991-05-01") |> 
  mutate(year = case_when(
    date >= "2016-05-01" ~ 2021,
    date >= "2011-05-01" ~ 2016,
    date >= "2006-05-01" ~ 2011,
    date >= "2001-05-01" ~ 2006,
    date >= "1996-05-01" ~ 2001,
    date >= "1991-05-01" ~ 1996)) |> 
  summarize(completions = sum(value), .by = year)

# Estimate attrition rate as the difference between current census units and 
# previous census units plus completions
attrition <- 
  dwellings |> 
  inner_join(completions, by = "year") |> 
  mutate(dwellings_add = lag(dwellings) + completions) |> 
  filter(year >= 2006) |> 
  mutate(attrition = dwellings_add - dwellings)

# Generate average annual attrition rate
attrition_pct <- 
  attrition |> 
  summarize(attrition_rate = mean(attrition) / mean(dwellings) / 5) |> 
  pull()


# Generate annual housing targets -----------------------------------------

# Calculate cumulative attrition estimates
attrition_targets <- 
  dwelling_targets |> 
  mutate(across(-year, \(x) x * attrition_pct)) |> 
  mutate(across(-year, cumsum))

# Add attrition then remove 2021 dwellings and take dif to create annual targets
completion_targets <- 
  (dwelling_targets + attrition_targets) |> 
  as_tibble() |> 
  mutate(year = year / 2) |> 
  mutate(across(-year, \(x) x - dwellings_2021)) |> 
  add_row(year = 2021, scn_ref_weak = 0, scn_ref_strong = 0, scn_weak_weak = 0,
          scn_weak_strong = 0, scn_strong_weak = 0, scn_strong_strong = 0) |> 
  arrange(year) |> 
  mutate(across(-year, \(x) slider::slide_dbl(x, \(y) y[2] - y[1], 
                                              .before = 1))) |> 
  filter(year >= 2025)


# Final global housing targets --------------------------------------------

# Visualization
completion_targets |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Needed completions", labels = scales::comma) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, values = curbcut_colors$brandbook$color[c(
    3, 8, 2, 6, 5, 4)], labels = c(
      "Ref. ISQ, strong occ. rate change",
      "Ref. ISQ, weak occ. rate change",
      "Strong ISQ, strong occ. rate change",
      "Strong ISQ, weak occ. rate change",
      "Weak ISQ, strong occ. rate change",
      "Weak ISQ, weak occ. rate change")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Starts and completions for general model --------------------------------

# Obtain monthly starts and convert to annual
starts <- 
  get_cmhc("Scss", "Starts", "Dwelling Type", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Dwelling Type` == "All") |> 
  select(date = Date, value = Value) |> 
  mutate(year = year(date)) |> 
  summarize(starts = sum(value), .by = year) |> 
  filter(year != max(year))

# Obtain monthly completions and convert to annual
completions <- 
  get_cmhc("Scss", "Completions", "Dwelling Type", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Dwelling Type` == "All") |> 
  select(date = Date, value = Value) |> 
  mutate(year = year(date)) |> 
  summarize(completions = sum(value), .by = year) |> 
  filter(year != max(year))

# Create comparison table
starts_completions <- 
  starts |> 
  inner_join(completions, by = "year") |> 
  mutate(starts_1 = lag(starts),
         starts_2 = lag(starts, 2),
         starts_3 = lag(starts, 3)) |> 
  rename(starts_0 = starts) |> 
  relocate(completions, .after = year)

# Examine lagged correlations
starts_completions |> 
  select(-year) |> 
  cor(use = "complete.obs")

# Examine scatterplots
starts_completions |> 
  mutate(across(starts_0:starts_3, \(x) x / completions)) |> 
  pivot_longer(starts_0:starts_3) |> 
  mutate(name = case_when(
    name == "starts_0" ~ "No lag",
    name == "starts_1" ~ "One year",
    name == "starts_2" ~ "Two years",
    name == "starts_3" ~ "Three years")) |> 
  mutate(name = factor(name, levels = c("No lag", "One year", "Two years", 
                                        "Three years"))) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_line() +
  facet_wrap(vars(name)) +
  scale_x_continuous(NULL) + 
  scale_y_continuous("Ratio of annual starts to completions", 
                     labels = scales::percent) +
  scale_colour_manual("Lag period",
                      values = curbcut_colors$brandbook$color[c(4, 3, 2, 5)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

# Regression diagnostics
lm(completions ~ starts_2, data = starts_completions) |> plot(which = 1)


# Typology projections ----------------------------------------------------

# Vectors for census download
type_2006 <- c(
  type_total = "v_CA06_119",
  type_single = "v_CA06_120",
  type_semi = "v_CA06_121",
  type_row = "v_CA06_122",
  type_duplex = "v_CA06_123",
  type_apart_small = "v_CA06_125",
  type_apart_large = "v_CA06_124",
  type_other_single = "v_CA06_126",
  type_movable = "v_CA06_127")

type_2011 <- c(
  type_total = "v_CA11F_199",
  type_single = "v_CA11F_200",
  type_semi = "v_CA11F_204",
  type_row = "v_CA11F_205",
  type_duplex = "v_CA11F_206",
  type_apart_small = "v_CA11F_207",
  type_apart_large = "v_CA11F_201",
  type_other_single = "v_CA11F_208",
  type_movable = "v_CA11F_202")

type_2016 <- c(
  type_total = "v_CA16_408",
  type_single = "v_CA16_409",
  type_semi = "v_CA16_412",
  type_row = "v_CA16_413",
  type_duplex = "v_CA16_414",
  type_apart_small = "v_CA16_415",
  type_apart_large = "v_CA16_410",
  type_other_single = "v_CA16_416",
  type_movable = "v_CA16_417")

type_2021 <- c(
  type_total = "v_CA21_434",
  type_single = "v_CA21_435",
  type_semi = "v_CA21_436",
  type_row = "v_CA21_437",
  type_duplex = "v_CA21_438",
  type_apart_small = "v_CA21_439",
  type_apart_large = "v_CA21_440",
  type_other_single = "v_CA21_441",
  type_movable = "v_CA21_442")

# Get census data
census_type <- bind_rows(
  get_census("CA06", regions = list(CSD = "2465005"), vectors = type_2006),
  get_census("CA11", regions = list(CSD = "2465005"), vectors = type_2011),
  get_census("CA16", regions = list(CSD = "2465005"), vectors = type_2016),
  get_census("CA21", regions = list(CSD = "2465005"), vectors = type_2021)) |>
  mutate(year = c(2006, 2011, 2016, 2021)) |> 
  select(year, type_total:type_movable)

# Scenario 1: preserve 2021 dwelling type ratios
scenario_1_vals <- 
  census_type |> 
  filter(year == 2021) |> 
  summarize(
    single = type_single / type_total,
    apart = (type_apart_small + type_apart_large) / type_total,
    other = 1 - single - apart)

dwellings_2021_typology <- scenario_1_vals * dwellings_2021

# Scenario 2: preserve recent absolute number of non-apartment starts
scenario_2_vals <-
  get_cmhc("Scss", "Starts", "Dwelling Type", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Dwelling Type` != "All") |> 
  select(date = Date, value = Value, type = `Dwelling Type`) |> 
  mutate(year = year(date)) |> 
  summarize(starts = sum(value), .by = c(year, type)) |> 
  filter(year != max(year)) |> 
  filter(year >= 2014) |> 
  summarize(
    single = mean(starts[type == "Single"]),
    apart = mean(starts[type == "Apartment"]),
    other = mean(starts[type %in% c("Semi-Detached", "Row")]))
  
# Scenario 3: preserve recent start percentages
scenario_3_vals <-
  get_cmhc("Scss", "Starts", "Dwelling Type", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Dwelling Type` != "All") |> 
  select(date = Date, value = Value, type = `Dwelling Type`) |> 
  mutate(year = year(date)) |> 
  summarize(starts = sum(value), .by = c(year, type)) |> 
  filter(year != max(year)) |> 
  filter(year >= 2014) |> 
  summarize(
    single = sum(starts[type == "Single"] / sum(starts)),
    apart = sum(starts[type == "Apartment"] / sum(starts)),
    other = 1 - single - apart)


# Dwelling/completion targets by typology: scenario 1 ---------------------

dwelling_targets_typology_1 <- 
  dwelling_targets |> 
  mutate(across(scn_ref_weak:scn_strong_strong, list(
    single = \(x) x * scenario_1_vals$single,
    apart = \(x) x * scenario_1_vals$apart,
    other = \(x) x * scenario_1_vals$other))) |> 
  select(-c(scn_ref_weak:scn_strong_strong))

attrition_targets_typology_1 <- 
  dwelling_targets_typology_1 |> 
  mutate(across(-year, \(x) x * attrition_pct)) |> 
  mutate(across(-year, cumsum))

# Add attrition then remove 2021 dwellings and take dif to create annual targets
completion_targets_typology_1 <-
  (dwelling_targets_typology_1 + attrition_targets_typology_1) |> 
  as_tibble() |> 
  mutate(year = year / 2) |> 
  mutate(across(ends_with("_single"), \(x) x - dwellings_2021_typology$single),
         across(ends_with("_apart"), \(x) x - dwellings_2021_typology$apart),
         across(ends_with("_other"), \(x) x - dwellings_2021_typology$other)) |> 
  add_row(year = 2021, scn_ref_weak_single = 0, scn_ref_weak_apart = 0, 
          scn_ref_weak_other = 0, scn_ref_strong_single = 0, 
          scn_ref_strong_apart = 0, scn_ref_strong_other = 0, 
          scn_weak_weak_single = 0, scn_weak_weak_apart = 0, 
          scn_weak_weak_other = 0, scn_weak_strong_single = 0, 
          scn_weak_strong_apart = 0, scn_weak_strong_other = 0, 
          scn_strong_weak_single = 0, scn_strong_weak_apart = 0, 
          scn_strong_weak_other = 0, scn_strong_strong_single = 0, 
          scn_strong_strong_apart = 0, scn_strong_strong_other = 0) |> 
  arrange(year) |> 
  mutate(across(-year, \(x) slider::slide_dbl(x, \(y) y[2] - y[1], 
                                              .before = 1))) |> 
  filter(year >= 2025)

# Total units visualization
dwelling_targets_typology_1 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

# Completions visualization
completion_targets_typology_1 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Dwelling/completion targets by typology: scenario 2 ---------------------

completion_targets_typology_2 <- 
  completion_targets |> 
  mutate(across(scn_ref_weak:scn_strong_strong, list(
    single = \(x) scenario_2_vals$single,
    apart = \(x) x - scenario_2_vals$single - scenario_2_vals$other,
    other = \(x) scenario_2_vals$other))) |> 
  select(-c(scn_ref_weak:scn_strong_strong))

# Completions visualization
completion_targets_typology_2 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

# Dwelling/completion targets by typology: scenario 3 ---------------------

completion_targets_typology_3 <- 
  completion_targets |> 
  mutate(across(scn_ref_weak:scn_strong_strong, list(
    single = \(x) x * scenario_3_vals$single,
    apart = \(x) x * scenario_3_vals$apart,
    other = \(x) x * scenario_3_vals$other))) |> 
  select(-c(scn_ref_weak:scn_strong_strong))

# Completions visualization
completion_targets_typology_3 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Starts versus completions by typology -----------------------------------

# Get average construction length by typology
con_length <- 
  get_cmhc("Scss", "Length of Construction", "Dwelling Type", 
           "Historical Time Periods", frequency = "Annual", 
           geo_uid = "2465005") |> 
  mutate(year = year(Date)) |> 
  select(year, type = `Dwelling Type`, con_length = Value) |> 
  filter(year >= 2002)

# Get average construction lengths for non-apartments >= 2014
con_length_type <- 
  con_length |> 
  filter(year >= 2014, !type %in% c("Apartment", "All")) |> 
  summarize(single = mean(con_length[type == "Single"]),
            other = mean(con_length[type != "Single"]))

# Model construction length for apartments >= 2014
con_length_apart_model <- 
  con_length |> 
  filter(year >= 2014, type == "Apartment") |> 
  lm(formula = con_length ~ year, data = _)

# Create table with predicted construction lengths
con_length_predicted <- 
  tibble(year = 2022:2051, 
         single = con_length_type$single,
         apart = predict(con_length_apart_model, tibble(year = 2022:2051)),
         other = con_length_type$other) |> 
  filter(year >= 2025)

# Get CT-level values to construct probability distributions
con_length_CT <- 
  map(2010:2023, \(x) get_cmhc(
    "Scss", "Length of Construction", "Dwelling Type", "Census Tracts", 
    year = x, frequency = "Annual", geo_uid = "2465005")) |> 
  bind_rows() |> 
  select(year = Year, type = `Dwelling Type`, value = Value) |> 
  filter(!is.na(value))

# Per-type standard deviation of construction time
con_length_SD <- 
  con_length_CT |> 
  summarize(
    single = sd(value[type == "Single"]),
    apart = sd(value[type == "Apartment"]),
    other = sd(value[type %in% c("Semi-Detached", "Row")]))
 
# Histograms
con_length_CT |> 
  filter(type != "All") |> 
  ggplot(aes(value)) +
  geom_histogram() +
  facet_wrap(vars(type))

# Proportion of starts in each month
starts_monthly <- 
  get_cmhc("Scss", "Starts", "Dwelling Type", "Historical Time Periods",
         geo_uid = "2465005") |> 
  filter(`Dwelling Type` != "All") |> 
  select(date = Date, type = `Dwelling Type`, value = Value) |> 
  mutate(month = month(date),
         type = if_else(type %in% c("Semi-Detached", "Row"), "Other", type)) |> 
  summarize(value = sum(value), .by = c(month, type)) |> 
  mutate(pct = value / sum(value), .by = type) |> 
  select(-value) |> 
  pivot_wider(names_from = type, values_from = pct) |> 
  select(month, single = Single, apart = Apartment, other = Other)

# Visualization
starts_monthly |> 
  pivot_longer(-month) |> 
  ggplot(aes(month, value, colour = name)) +
  geom_line()

# Create month-specific distribution function for completions -> starts
allocate_completions <- function(units, mn, stdv, month_dist, year) {
  comp_0 <- pnorm(11:0, mn, stdv)
  comp_1 <- pnorm(23:12, mn, stdv) - comp_0
  comp_2 <- pnorm(35:24, mn, stdv) - comp_1 - comp_0
  comp_3 <- pnorm(47:36, mn, stdv) - comp_2 - comp_1 - comp_0
  comp_4 <- pnorm(59:48, mn, stdv) - comp_3 - comp_2 - comp_1 - comp_0
  comp_5 <- pnorm(71:60, mn, stdv) - comp_4 - comp_3 - comp_2 - comp_1 - comp_0
  tibble(year = (year - 5):year, 
         pct = c(sum(comp_0 * month_dist * units), 
                 sum(comp_1 * month_dist * units), 
                 sum(comp_2 * month_dist * units), 
                 sum(comp_3 * month_dist * units), 
                 sum(comp_4 * month_dist * units), 
                 sum(comp_5 * month_dist * units)))
}

# Helper function to simplify application to completion targets tables
build_starts <- function(comp_table) {
  
  # Get correct columns for each housing type
  singles <- str_which(names(comp_table), "_single")
  aparts <- str_which(names(comp_table), "_apart")
  others <- str_which(names(comp_table), "_other")
  
  # iterate over column numbers
  singles_out <-
    map(singles, \(col) {
    pmap(list(comp_table$year,
              comp_table[[col]],
              con_length_predicted$single),
         \(year, units, mn) allocate_completions(
           units, mn, con_length_SD$single, starts_monthly$single, year)) |> 
      bind_rows() |> 
      summarize(starts = sum(pct), .by = year) |> 
      arrange(year)}) |> 
    reduce(inner_join, by = "year") |> 
    set_names(c("year", names(comp_table)[singles]))
  
  aparts_out <- map(aparts, \(col) {
    pmap(list(comp_table$year,
              comp_table[[col]],
              con_length_predicted$apart),
         \(year, units, mn) allocate_completions(
           units, mn, con_length_SD$apart, starts_monthly$apart, year)) |> 
      bind_rows() |> 
      summarize(starts = sum(pct), .by = year) |> 
      arrange(year)}) |> 
    reduce(inner_join, by = "year") |> 
    set_names(c("year", names(comp_table)[aparts]))
  
  others_out <- map(others, \(col) {
    pmap(list(comp_table$year,
              comp_table[[col]],
              con_length_predicted$other),
         \(year, units, mn) allocate_completions(
           units, mn, con_length_SD$other, starts_monthly$other, year)) |> 
      bind_rows() |> 
      summarize(starts = sum(pct), .by = year) |> 
      arrange(year)}) |> 
    reduce(inner_join, by = "year") |> 
    set_names(c("year", names(comp_table)[others]))
  
  # Assemble output
  out <- 
    singles_out |> 
    inner_join(aparts_out, by = "year") |> 
    inner_join(others_out, by = "year")
  
  # Reorder columns and return output
  out[names(comp_table)]
  
}

# Create annual schedule of starts necessary to produce targeted completions
start_targets_typology_1 <- 
  build_starts(completion_targets_typology_1) |> 
  filter(year >= 2025)

start_targets_typology_2 <- 
  build_starts(completion_targets_typology_2) |> 
  filter(year >= 2025)

start_targets_typology_3 <- 
  build_starts(completion_targets_typology_3) |> 
  filter(year >= 2025)

# Visualizations
start_targets_typology_1 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

start_targets_typology_2 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

start_targets_typology_3 |> 
  pivot_longer(-year) |> 
  mutate(type = case_when(str_detect(name, "apart") ~ "apart",
                          str_detect(name, "single") ~ "single",
                          str_detect(name, "other") ~ "other"),
         name = str_remove(name, "(_apart)|(_single)|(_other)")) |> 
  ggplot(aes(year, value, colour = type)) +
  geom_line() +
  facet_wrap(vars(name), nrow = 3) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Apartments", "Semi-detached, row, etc.", 
                                       "Single-detached"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Bedroom counts ----------------------------------------------------------

br_2011 <- c(
  br_total = "v_CA11N_2247",
  br_one = "v_CA11N_2248", # Zero or one
  br_two = "v_CA11N_2249",
  br_three = "v_CA11N_2250",
  br_four = "v_CA11N_2251")

br_2016 <- c(
  br_total = "v_CA16_4843",
  br_zero = "v_CA16_4844",
  br_one = "v_CA16_4845",
  br_two = "v_CA16_4846",
  br_three = "v_CA16_4847",
  br_four = "v_CA16_4848")

br_2021 <- c(
  br_total = "v_CA21_4244",
  br_zero = "v_CA21_4245",
  br_one = "v_CA21_4246",
  br_two = "v_CA21_4247",
  br_three = "v_CA21_4248",
  br_four = "v_CA21_4249")

census_br <- bind_rows(
  get_census("CA11", regions = list(CSD = "2465005"), vectors = br_2011),
  get_census("CA16", regions = list(CSD = "2465005"), vectors = br_2016),
  get_census("CA21", regions = list(CSD = "2465005"), vectors = br_2021)) |>
  mutate(year = c(2011, 2016, 2021)) |> 
  select(year, br_total, br_zero, br_one:br_four)

census_br <- 
  census_br |> 
  replace_na(list(br_zero = 0)) |> 
  mutate(br_one = br_zero + br_one) |> 
  select(-br_zero)

census_br |> 
  mutate(across(br_one:br_four, \(x) x / br_total)) |> 
  select(-br_total) |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_line()



# Dedicated old-age housing -----------------------------------------------

# Import ISQ projections
isq_age <-
  read_excel("data/isq_age.xlsx", skip = 5) |> 
  select(1, 3:5, a75 = `75-79`, a80 = `80-84`, a85 = `85-89`, a90 = `90+`) |> 
  set_names(c("scenario", "region", "year", "total", "a75", "a80", "a85", 
              "a90")) |> 
  slice(-1) |> 
  filter(region == "Laval") |> 
  select(-region) |> 
  mutate(scenario = case_when(
    scenario == "Faible D2024" ~ "weak",
    scenario == "Référence A2024" ~ "reference",
    scenario == "Fort E2024" ~ "strong")) |> 
  mutate(total = as.numeric(total))

# Isolate number of households headed by 75+
isq_age <- 
  isq_age |> 
  mutate(value = a75 + a80 + a85 + a90) |> 
  select(scenario, year, value)

# Visualization
isq_age |> 
  ggplot(aes(year, value, colour = scenario)) +
  geom_line() +
  scale_x_continuous(NULL) + 
  scale_y_continuous("Households headed by 75+", labels = scales::comma) +
  scale_colour_manual(NULL, labels = c("Reference scenario", "Strong scenario", 
                                       "Weak scenario"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

# 2023 RPAs
rpa_2023 <- 12038

# Ratio of 75+ to RPAs in 2023
rpa_ratio <- 
  isq_age |> 
  filter(scenario == "reference", year == 2023) |> 
  pull(value) |> 
  (\(x) rpa_2023 / x)()

# Get RPA net dwelling targets
dwelling_targets_rpa <- 
  isq_age |> 
  pivot_wider(names_from = scenario) |> 
  mutate(across(-year, \(x) x * rpa_ratio))

# Visualization
dwelling_targets_rpa |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Needed completions", labels = scales::comma) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Reference scenario", "Strong scenario", 
                                       "Weak scenario"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))

# Calculate cumulative attrition estimates
attrition_targets_rpa <- 
  dwelling_targets_rpa |> 
  mutate(across(-year, \(x) x * attrition_pct)) |> 
  mutate(across(-year, cumsum))

# Add attrition then remove 2021 dwellings and take dif to create annual targets
completion_targets_rpa <- 
  (dwelling_targets_rpa + attrition_targets_rpa) |> 
  as_tibble() |> 
  mutate(year = year / 2) |> 
  mutate(across(-year, \(x) x - rpa_2023)) |> 
  mutate(across(-year, \(x) slider::slide_dbl(x, \(y) y[2] - y[1], 
                                              .before = 1))) |> 
  filter(year >= 2025)

# Visualization
completion_targets_rpa |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Needed completions", labels = scales::comma) +
  scale_x_continuous(NULL) + 
  scale_colour_manual(NULL, labels = c("Reference scenario", "Strong scenario", 
                                       "Weak scenario"),
                      values = curbcut_colors$brandbook$color[c(4, 3, 2)]) +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Save outputs ------------------------------------------------------------

qsavem(isq, plot_isq_households, occ_rate, occ_model, plot_occ_rate, 
       plot_dwelling_targets, 
       file = "data/targets.qsm")
