source("R/startup.R")


# Census vectors ----------------------------------------------------------

list_census_vectors("CA16") |> view()

vec_2001_511 <- c(
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
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271")

vec_2006_511 <- c(
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
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271")

vec_2011_511 <- c(
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
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271")

vec_2016_511 <- c(
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
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271")

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
  age_total = "v_CA21_4263",
  age_1960 = "v_CA21_4264",
  age_1980 = "v_CA21_4265",
  age_1990 = "v_CA21_4266",
  age_2000 = "v_CA21_4267",
  age_2005 = "v_CA21_4268",
  age_2010 = "v_CA21_4269",
  age_2015 = "v_CA21_4270",
  age_2021 = "v_CA21_4271")


# Get census data ---------------------------------------------------------

housing_01 <- get_census("CA01", regions = list(CSD = "2465005"), level = "DA",
                         vectors = vec_2001_511, geo_format = "sf") |> 
  mutate(year = 2001, .after = GeoUID)
housing_06 <- get_census("CA06", regions = list(CSD = "2465005"), level = "DA",
                         vectors = vec_2006_511, geo_format = "sf") |> 
  mutate(year = 2006, .after = GeoUID)
housing_11 <- get_census("CA11", regions = list(CSD = "2465005"), level = "DA",
                         vectors = vec_2011_511, geo_format = "sf") |> 
  mutate(year = 2011, .after = GeoUID)
housing_16 <- get_census("CA16", regions = list(CSD = "2465005"), level = "DA",
                         vectors = vec_2016_511, geo_format = "sf") |> 
  mutate(year = 2016, .after = GeoUID)
housing_21 <- get_census("CA21", regions = list(CSD = "2465005"), level = "DA",
                         vectors = vec_2021_511, geo_format = "sf") |> 
  mutate(year = 2021, .after = GeoUID)

bind_rows(housing_01, housing_06, housing_11, housing_16, housing_21) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(-`Shape Area`, -Type, -Households, -`Quality Flags`, -name, -CSD_UID,
         -Population, -CT_UID, -c(CD_UID:`Area (sq km)`)) |> 
  rename(dwellings = Dwellings)


# 5.1.1.1 Répartition des logements selon le typologie --------------------







# 5.1.1.2 -------------------------------------------------------------------


# 5.1.1.3 -------------------------------------------------------------------


# 5.1.1.4 -------------------------------------------------------------------


# 5.1.1.5 -------------------------------------------------------------------



# Save --------------------------------------------------------------------

qs::qsavem(file = "data/5_1_1.qsm")
