source("R/startup.R")


# Census vectors ----------------------------------------------------------

list_census_vectors("CA11") |> view()

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
  bedroom_zero = "v_CA11N_2248", # Zero or one
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
  age_2010 = "v_CA11N_2239")

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
  age_2015 = "v_CA16_4869")

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

bind_rows(housing_11, housing_16, housing_21) |> 
  as_tibble() |> 
  st_as_sf() |> 
  select(-`NHS Non-Return Rate`, -`NHS Non Return Rate`, -`Shape Area`, -Type, 
         -Households, -`Quality Flags`, -name, -CSD_UID, -Population, -CT_UID, 
         -c(CD_UID:`Area (sq km)`)) |> 
  rename(dwellings = Dwellings) |> 
  relocate(dwellings, .after = year)


# 5.1.1.1 Répartition des logements selon le typologie --------------------







# 5.1.1.2 -------------------------------------------------------------------


# 5.1.1.3 -------------------------------------------------------------------


# 5.1.1.4 -------------------------------------------------------------------


# 5.1.1.5 -------------------------------------------------------------------



# Save --------------------------------------------------------------------

qs::qsavem(file = "data/5_1_1.qsm")
