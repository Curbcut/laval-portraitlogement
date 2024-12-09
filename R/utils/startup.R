#### STARTUP SCRIPT ############################################################

# Loading Libraries -------------------------------------------------------

library(cmhc)
library(cancensus)
library(scales)
library(classInt)
library(tidyverse)
library(curbcut)
library(extrafont)
library(showtext)
library(sf)
library(gt)
library(readxl)
library(qs)
library(officer)
library(tsibble)

source(here::here("R/utils/crosstab.R"))
source(here::here("R/utils/gt_save_word.R"))
source(here::here("R/utils/interpolation.R"))
source(here::here("R/utils/ggsave_pdf_png.R"))

if (.Platform$OS.type == "windows") {
  # font_import()
  # loadfonts(device = "win", quiet = TRUE)
  # windowsFonts(`KMR Apparat Regular`=windowsFont(font_local_name))
  # font_local_name %in% names(windowsFonts())
  tryCatch({
    font_add(family = "KMR Apparat Regular", regular = here::here("data/fonts/KMR-Apparat-Regular.ttf"))
    font_add(family = "KMR-Apparat-Regular", regular = here::here("data/fonts/KMR-Apparat-Regular.ttf"))
  }, error = function(e) cat("Fonts didn't load"))
  showtext_auto()
  font_local_name <- "KMR Apparat Regular"
} else font_local_name <- "KMR Apparat"


# Checking vectors for Canada ---------------------------------------------

cmhc_breakdown <- list_cmhc_breakdowns()
cancensus_21_vectors <- list_census_vectors("CA21")


# Shapefiles -------------------------------------------------------------

laval_da <- get_census(dataset = "CA21",
                       regions = list(CSD = 2465005),
                       level = "DA",
                       geo_format = "sf") |> 
  select(GeoUID) |> 
  st_transform(crs = 4326)

laval_ct <- get_census(dataset = "CA21",
                       regions = list(CSD = 2465005),
                       level = "CT",
                       geo_format = "sf") |> 
  select(GeoUID) |> 
  st_transform(crs = 4326)

laval_ct_hou <- get_census(dataset = "CA21",
                       regions = list(CSD = 2465005),
                       level = "CT",
                       geo_format = "sf") |> 
  select(GeoUID, Households) |> 
  st_transform(crs = 4326)

lvl <- 
  get_census("CA21", regions = list(CSD = 2465005), level = "CSD", 
             geo_format = "sf") |> 
  st_transform(crs = 32618)

electoral_districts <- sf::st_read(here::here("data/limite-district-electoral.geojson"))
electoral_districts <- sf::st_transform(electoral_districts, crs = 32618)
electoral_districts <- sf::st_intersection(electoral_districts, lvl["geometry"])


# Addition of colours -----------------------------------------------------

curbcut_colors <- cc.buildr::build_colours()
curbcut_scale <- curbcut_colors$left_5$fill[2:6]

# Colours from brandbook
curbcut_colors$brandbook <- tibble(
  theme = c("greydata", "blueexplorer", "greenecology", "redhousing", 
            "pinkhealth", "purpletransport", "yellowclimate",
            "greenurbanlife", "browndemographics"),
  color = c("#F0F0F0", "#A3B0D1", "#73AD80", "#E08565", "#CD718C", 
            "#C9C3FA", "#F5D574", "#ADB033", "#9E9090"))

color_show <- \() scales::show_col(curbcut_colors$brandbook$color)
color_theme <- function(theme) {
  curbcut_colors$brandbook$color[curbcut_colors$brandbook$theme == theme]
}

# Themes ------------------------------------------------------------------

graph_theme <- 
  theme_minimal() +
  theme(legend.title = element_blank(), 
        legend.position = "bottom",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 11))

graph_theme_w_legendtitle <- 
  theme_minimal() +
  theme(legend.title.position = "top",
        legend.position = "bottom",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family=font_local_name),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 11))


# Map Theme ---------------------------------------------------------------

lvlbbox <- sf::st_bbox(lvl)

laval_sectors <- qs::qread(here::here("data/geom_context/secteur.qs"))

tiles <- mapboxapi::get_static_tiles(
  location = lvlbbox, 
  username = "curbcut",
  zoom = 11,##
  style_id = "clz1cxvvi021f01nxef327y5p",#"cljkciic3002h01qveq5z1wrp",
  access_token = paste0("pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAz",
                        "M2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg"),
  scaling_factor = "2x")

gg_cc_tiles <- list(ggspatial::layer_spatial(tiles, alpha = 0.7))

default_theme <- theme(
  legend.position = "bottom",
  legend.box = "vertical",
  legend.title = element_text(size = 11, family=font_local_name),
  legend.text = element_text(size = 10, family=font_local_name),
  legend.title.align = 0.5,
  legend.text.align = 0.5,
  text=element_text(size = 10, family=font_local_name), 
  legend.box.margin = margin(t = -10))

# Converting the bounding box to be compatible with EPSG:4326
bbox <- tibble(
  x = c(lvlbbox["xmin"], lvlbbox["xmax"]),
  y = c(lvlbbox["ymin"], lvlbbox["ymax"])) |> 
  st_as_sf(coords = c("x", "y"), crs = 32618) |> 
  # st_transform(crs = 4326) |> 
  st_bbox()

x_range <- bbox["xmax"] - bbox["xmin"]
y_range <- bbox["ymax"] - bbox["ymin"]

# Zooming out the bbox a little bit so it's not right on the borders
padding <- 0.09

xlim_zoomed_out <- c(
  bbox["xmin"] - padding * x_range,
  bbox["xmax"] + padding * x_range
)

ylim_zoomed_out <- c(
  bbox["ymin"] - padding * y_range,
  bbox["ymax"] + padding * y_range
)

# Map theme
gg_cc_theme <- c(list(
  geom_sf(data = electoral_districts, fill = "transparent", color = "black"),
  coord_sf(xlim = xlim_zoomed_out, 
           ylim = ylim_zoomed_out,
           expand = FALSE)),
  list(theme_void()),
  list(default_theme),
  list(theme(legend.box.margin = margin(t = 0)))
)

gg_cc_theme_nodistricts <- c(list(
  coord_sf(xlim = xlim_zoomed_out, 
           ylim = ylim_zoomed_out,
           expand = FALSE)),
  list(theme_void()),
  list(default_theme),
  list(theme(legend.box.margin = margin(t = 0)))
)

table_font_size <- 9.5

# Number functions --------------------------------------------------------

convert_hundreds <- function(x) {
  curbcut:::round_big_marks(
    x = x,
    min_dig = 5,
    scale_fun = scales::comma
  )
}

convert_number <- function(x) {
  x[!is.na(x)] <- curbcut::convert_unit(x = x[!is.na(x)])
  gsub(",", " ", x)
}

convert_number_tens <- \(x) scales::comma(round(x / 10) * 10)

convert_number_noround <- function(x) {
  scales::comma(x, 1, accuracy = 1, big.mark = " ")
}

convert_pct <- function(x) {
  out <- curbcut:::convert_unit.pct(x = x, decimal = 1)
  out <- gsub("\\.", ",", out)
  out <- gsub("\\%", " %", out)
  for (i in seq_along(out)) {
    if (!grepl("\\,", out[[i]])) out[[i]] <- gsub(" %", ",0 %", out[[i]])
  }
  out
}

convert_dollar <- \(x) paste(convert_number(x), "$")
