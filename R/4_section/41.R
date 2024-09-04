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
  mutate("count_cc" = convert_number(count),
         "prop" = count / total,
         "prop_cc" = convert_pct(count / total))

#Creating a grouped bar chart
ggplot(pto, aes(x = Year, y = count, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("owner" = "#A3B0D1", "tenant" = "#CD718C")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages", fill = "Year") +
  graph_theme
