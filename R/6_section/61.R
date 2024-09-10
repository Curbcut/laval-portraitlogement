#### 6.1 #######################################################################

source("R/utils/startup.R")

list_cmhc_dimensions("Scss", "Starts")


# 6.1.1 Dév rés récent (répartition territoriale par type de logem --------


# 6.1.2 Mises en chantier par typologie -----------------------------------

starts_by_type <- 
  map(2010:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Dwelling Type",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows()


# 6.1.3 Mises en chantier par mode d'occupation ---------------------------

starts_by_market <- 
  map(2010:2023, \(x) {
    get_cmhc(
      survey = "Scss",
      series = "Starts", 
      dimension = "Intended Market",
      breakdown = "Survey Zones", 
      geo_uid = "2465005",
      year = x)}) |> 
  bind_rows()
