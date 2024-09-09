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
data_4_1_2 <- read_excel("data/4/mode_occupation_revenu_SR.xlsx") |> #Edited version of the spreadsheet
  select(-GeoUID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "households") |> 
  mutate(income = case_when(
    str_detect(type, "19999") ~ "< 19 999 $",
    str_detect(type, "39999") ~ "20 - 39 999 $",
    str_detect(type, "59999") ~ "40 - 59 999 $",
    str_detect(type, "79999") ~ "60 - 79 999 $",
    str_detect(type, "99999") ~ "80 - 99 999 $",
    str_detect(type, "124999") ~ "100 - 124 999 $",
    str_detect(type, "125000") ~ "> 125 000 $",
    TRUE ~ NA_character_)) |> 
  mutate(type = case_when(
    str_detect(type, "Owner") & !str_detect(type, "Condo Owner") ~ "Propriétaire",
    str_detect(type, "Tenant") & !str_detect(type, "Condo Tenant") ~ "Locataire",
    TRUE ~ type
    )) |>
  filter(type %in% c("Propriétaire", "Locataire")) |> 
  mutate(type = factor(type, levels = c(
    "Propriétaire", 
    "Locataire")),
    income = factor(income, levels = c(
      "< 19 999 $", 
      "20 - 39 999 $", 
      "40 - 59 999 $", 
      "60 - 79 999 $", 
      "80 - 99 999 $", 
      "100 - 124 999 $", 
      "> 125 000 $")))


plot_4_1_2 <- ggplot(data_4_1_2, aes(x = income, y = households, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Nombre de ménages (n)", title = "") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/4/plot_4_1_2.png", plot = plot_4_1_2, width = 800/72, height = 600/72, dpi = 72)

# 4.1.3 -------------------------------------------------------------------
data_4_1_3 <- read_excel("data/4/mode_occupation_composition_SR.xlsx") |> #Edited version of the spreadsheet
  select(-GeoUID) |> 
  summarise(across(everything(), \(x) sum(x, na.rm = TRUE))) |> 
  pivot_longer(cols = everything(), names_to = "type", values_to = "households") |> 
  mutate(composition = case_when(
    str_detect(type, "wo child") ~ "Couple sans enfants*",
    str_detect(type, "child") ~ "Couple avec enfants*",
    str_detect(type, "single parent") ~ "Famille monoparentale*",
    str_detect(type, "multigen") ~ "Ménage multigénérationnel",
    str_detect(type, "other") ~ "Autres ménages comptant une famille de recensement",
    str_detect(type, "sole") ~ "Ménage composé d'une seule personne",
    str_detect(type, "non cf") ~ "Ménage composé de deux personnes ou plus",
    TRUE ~ NA_character_)) |> 
  mutate(type = case_when(
    str_detect(type, "Owner") & !str_detect(type, "Condo Owner") ~ "Propriétaire",
    str_detect(type, "Tenant") & !str_detect(type, "Condo Tenant") ~ "Locataire",
    TRUE ~ type
  )) |>
  filter(type %in% c("Propriétaire", "Locataire")) |>  
  mutate(type = factor(type, levels = c(
    "Propriétaire", 
    "Locataire")),
    composition = factor(composition, levels = c(
      "Couple avec enfants*", 
      "Couple sans enfants*", 
      "Famille monoparentale*", 
      "Ménage multigénérationnel", 
      "Autres ménages comptant une famille de recensement", 
      "Ménage composé d'une seule personne", 
      "Ménage composé de deux personnes ou plus")))

plot_4_1_3 <- ggplot(data_4_1_3, aes(x = composition, y = households, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Nombre de ménages (n)", title = "", caption = "*Ménage comptant une seule famille de recensement, sans personnes additionnelles") +
  scale_fill_manual(values = c("Propriétaire" = "#A3B0D1", "Locataire" = "#CD718C")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  graph_theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.caption = element_text(hjust = 0))

ggsave("outputs/4/plot_4_1_3.png", plot = plot_4_1_3, width = 800/72, height = 600/72, dpi = 72)
# 4.1.4 -------------------------------------------------------------------

# 4.1.5 -------------------------------------------------------------------

# 4.1.6 -------------------------------------------------------------------

# 4.1.7 -------------------------------------------------------------------
data_4_1_7 <- read_excel("data/4/4_1_7.xlsx") |> 
  filter(RA == 13) |> 
  mutate(period = factor(ifelse(Year < 2024, "Avant 2024", "Après 2024"),
                         levels = c("Avant 2024", "Après 2024")))

plot_4_1_7 <- ggplot(data_4_1_7, aes(x = Year, y = Total, linetype = period)) +
  geom_line(color = "black", linewidth = 1.75) +
  scale_linetype_manual(values = c("Avant 2024" = "solid", "Après 2024" = "dotted"),
                        labels = c("Avant 2024", "Après 2024")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Nombre de ménages (n)", linetype = "Période") +
  graph_theme

ggsave("outputs/4/plot_4_1_7.png", plot = plot_4_1_7, width = 800/72, height = 600/72, dpi = 72)
# 4.1.8 -------------------------------------------------------------------

# 4.1.9 -------------------------------------------------------------------
data_4_1_9 <- read_excel("data/4/4_1_9.xlsx") |> 
  filter(RA == 13 & Sexe == 3) |> 
  mutate("0-24" = `0-19` + `20-24`,
         "25-64" = `20-64` - `20-24`)

data_4_1_9_2 <- data_4_1_7 |> 
  

plot_4_1_7_pop <- ggplot(data_4_1_9, aes(x = Year)) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `0-24`, color = "0-24", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `0-24`, color = "0-24", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `25-64`, color = "25-64", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `25-64`, color = "25-64", linetype = "After 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year < 2024), aes(y = `65+`, color = "65+", linetype = "Before 2024"), linewidth = 1.75) +
  geom_line(data = subset(data_4_1_9, Year >= 2024), aes(y = `65+`, color = "65+", linetype = "After 2024"), linewidth = 1.75) +
  scale_color_manual(values = c("0-24" = "#A3B0D1", "25-64" = "#CD718C", "65+" = "#73AD80"),
                     labels = c("0-24", "25-64", "65+")) +
  scale_linetype_manual(values = c("Before 2024" = "solid", "After 2024" = "dotted"),
                        labels = c("Avant 2024", "Après 2024")) +
  scale_y_continuous(labels = function(x) convert_number(x)) +
  labs(x = "", y = "Population (n)", color = "Age Group", linetype = "Period") +
  graph_theme


# R Markdown --------------------------------------------------------------
qs::qsavem(plot_4_1_1, plot_4_1_2, plot_4_1_3, file = "data/section_4_1.qsm")
