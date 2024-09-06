source("R/utils/startup.R")

# 4.1.1 -------------------------------------------------------------------
#Vectors for tenure type
pto_21v <- c("total" = "v_CA21_4288", "owner" = "v_CA21_4305", "tenant" = "v_CA21_4313")
pto_16v <- c("total" = "v_CA16_4886", "owner" = "v_CA16_4890", "tenant" = "v_CA16_4897")
pto_11v <- c("total" = "v_CA11N_2277", "owner" = "v_CA11N_2281", "tenant" = "v_CA11N_2288")
pto_06v <- c("total" = "v_CA06_2048", "owner" = "v_CA06_2053", "tenant" = "v_CA06_2049")
pto_01v <- c("owner" = "v_CA01_1670", "tenant" = "v_CA01_1666")

#Census Grabbing Function for Laval
pto_census <- function(datayear, pto_year, cyear){
  get_census(dataset = datayear,
             regions = list(CSD = 2465005),
             level = "CSD",
             vectors = pto_year
  ) |> 
    mutate(Year = cyear) |> 
    select(-GeoUID, -Type, -`Region Name`, -`Area (sq km)`, -Population,
           -Dwellings, -Households, -CD_UID, -PR_UID, -CMA_UID)
}

#Grabbing data for 2001-2021 for Laval
pto_21 <- pto_census("CA21", pto_21v, "2021")
pto_16 <- pto_census("CA16", pto_16v, "2016")
pto_11 <- pto_census("CA11", pto_11v, "2011") |> 
  select(-`NHS Non Return Rate`)
pto_06 <- pto_census("CA06", pto_06v, "2006")
pto_01 <- pto_census("CA01", pto_01v, "2001") |> 
  mutate(total = owner + tenant)

pto <- bind_rows(pto_21, pto_16, pto_11, pto_06, pto_01) |>
  pivot_longer(cols = c("owner", "tenant"), 
               names_to = "type", 
               values_to = "count") |>
  group_by(type) |>
  arrange(Year) |>
  mutate(
    count_cc = convert_number(count),
    prop = count / total,
    prop_cc = paste0(
      convert_pct(prop),
      ifelse(is.na(lag(prop)), "", 
             ifelse(prop > lag(prop), "▲", "▼"))
    )
  ) |>
  ungroup()

#Creating a grouped bar chart
plot_4_1_1 <- ggplot(pto, aes(x = Year, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = prop_cc), position = position_dodge(width = 0.9),
            vjust = 2, size = 5, color = "white") +
  scale_fill_manual(values = c("owner" = "#A3B0D1", "tenant" = "#CD718C"),
                    labels = c("owner" = "Propriétaire", "tenant" = "Locataire")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages") +
  graph_theme

ggsave("outputs/4/plot_4_1_1.png", plot = plot_4_1_1, width = 800/72, height = 600/72, dpi = 72)

# 4.1.2 -------------------------------------------------------------------

# 4.1.3 -------------------------------------------------------------------

# 4.1.4 -------------------------------------------------------------------

# 4.1.5 -------------------------------------------------------------------

# 4.1.6 -------------------------------------------------------------------

# 4.1.7 -------------------------------------------------------------------
data_4_1_7 <- read_excel("data/4/4_1_7.xlsx") |> 
  filter(RA == 13)

ggplot(data_4_1_7, aes(x = Year, y = Total)) +
  geom_line(color = "#A3B0D1", linewidth = 1.5) +
  geom_point(color = "blue") +
  labs(x = "Year", y = "Total") +
  graph_theme
# 4.1.8 -------------------------------------------------------------------

# 4.1.9 -------------------------------------------------------------------

# R Markdown --------------------------------------------------------------
qs::qsavem(plot_4_1_1, file = "data/section_4_1.qsm")
