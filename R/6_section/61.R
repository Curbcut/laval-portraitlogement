#### 6.1 #######################################################################

source("R/utils/startup.R")


# 6.1.2. Mises en chantier par typologie ----------------------------------

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

