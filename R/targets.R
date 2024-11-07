#### Target calculations #######################################################

source("R/utils/startup.R")


# Households --------------------------------------------------------------

# Import ISQ projections
isq <-
  read_excel("data/isq.xlsx", skip = 5) |> 
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

# Generate annual attrition estimate
attrition_annual <- mean(attrition$attrition) / 5


# Generate annual housing targets -----------------------------------------

# Join ISQ and occupancy data
targets <- 
  isq |> 
  pivot_wider(names_from = scenario, names_prefix = "hh_") |> 
  inner_join({
    occ_rate |> 
      filter(type != "actual") |> 
      pivot_wider(names_from = type, names_prefix = "occ_", 
                  values_from = occ_rate)}, by = "year")

# Create six scenarios
targets <- 
  targets |> 
  mutate(
    scn_ref_weak = hh_reference / occ_weak,
    scn_ref_strong = hh_reference / occ_strong,
    scn_weak_weak = hh_weak / occ_weak,
    scn_weak_strong = hh_weak / occ_strong,
    scn_strong_weak = hh_strong / occ_weak,
    scn_strong_strong = hh_strong / occ_strong) |> 
  select(-c(hh_reference:occ_weak)) |> 
  mutate(across(-year, \(x) x - dwellings_2021 + attrition_annual * 1:30))

# Take the difference of each series to create annual targets
targets <- 
  targets |> 
  add_row(year = 2021, scn_ref_weak = 0, scn_ref_strong = 0, scn_weak_weak = 0,
          scn_weak_strong = 0, scn_strong_weak = 0, scn_strong_strong = 0) |> 
  arrange(year) |> 
  mutate(across(-year, \(x) slider::slide_dbl(x, \(y) y[2] - y[1], 
                                              .before = 1))) |> 
  filter(year >= 2025)

# Visualization
targets |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Needed units", labels = scales::comma) +
  scale_x_continuous(NULL) + 
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = "KMR Apparat"))


# Starts and completions --------------------------------------------------

# Obtain monthly starts and convert to annual
starts <- 
  get_cmhc("Scss", "Starts", "Intended Market", "Historical Time Periods",
           geo_uid = "2465005") |> 
  filter(`Intended Market` == "All") |> 
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
lm(completions ~ starts_2, data = starts_completions) |> plot()


# Final global housing targets --------------------------------------------

# Lag targets by two years to convert completions to starts
targets <- 
  targets |> 
  mutate(year = year - 2) |> 
  filter(year >= 2025)

# Visualization
targets |> 
  pivot_longer(-year) |> 
  ggplot(aes(year, value, colour = name)) +
  geom_point() +
  scale_y_continuous("Needed starts", labels = scales::comma) +
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

