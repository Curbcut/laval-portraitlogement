# Loading Libraries -------------------------------------------------------
library(cmhc)
library(cancensus)
library(tidyverse)
library(curbcut)
library(extrafont)
library(showtext)
library(sf)
library(gt)
library(readxl)

# Checking vectors for Canada ---------------------------------------------
cmhc_breakdown <- list_cmhc_breakdowns()
cancensus_21_vectors <- list_census_vectors("CA21")

# Addition of colors ------------------------------------------------------

curbcut_colors <- cc.buildr::build_colours()

# Colors from brandbook
curbcut_colors$brandbook <- 
  tibble::tibble(theme = c("greydata", "blueexplorer", "greenecology", "redhousing",
                           "pinkhealth", "purpletransport", "yellowclimate",
                           "greenurbanlife", "browndemographics"),
                 color = c("#F0F0F0", "#A3B0D1", "#73AD80", "#E08565", "#CD718C", 
                           "#C9C3FA", "#F5D574", "#ADB033", "#9E9090"))

# Themes ------------------------------------------------------------------

graph_theme <- theme_minimal() +
  theme(legend.title = element_blank(), legend.position = "bottom",
        legend.margin = margin(t = -5, r = 0, b = 5, l = 0),
        text=element_text(family="KMR Apparat Regular"),
        legend.text = element_text(size = 10),
        axis.title.y = element_text(size = 11))

# Map Theme ---------------------------------------------------------------

lvl <- cancensus::get_census("CA21", regions = list(CSD = 2465005), 
                             level = "CSD",
                             geo_format = "sf")
lvlbbox <- sf::st_bbox(lvl)
laval_sectors <- qs::qread("data/geom_context/secteur.qs")

tiles <- mapboxapi::get_static_tiles(
  location = lvlbbox, 
  username = "curbcut",
  zoom = 11,##
  style_id = "clz1cxvvi021f01nxef327y5p",#"cljkciic3002h01qveq5z1wrp",
  access_token = "pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg",
  scaling_factor = "2x")

gg_cc_tiles <- list(ggspatial::layer_spatial(tiles, alpha = 0.7))
default_theme <- theme(legend.position = "bottom",
                       legend.box = "horizontal",
                       legend.title = element_text(size = 11, 
                                                   family="KMR Apparat Regular"),
                       legend.text = element_text(size = 10, 
                                                  family="KMR Apparat Regular"),
                       legend.title.align = 0.5,
                       legend.text.align = 0.5,
                       text=element_text(size = 10, family="KMR Apparat Regular"), 
                       legend.box.margin = margin(t = -10))
gg_cc_theme_no_sf <- list(
  theme_minimal(),
  default_theme
)

gg_cc_theme <- c(list(
  geom_sf(data = laval_sectors, fill = "transparent", color = "black"),
  coord_sf(xlim = c(lvlbbox["xmin"], lvlbbox["xmax"]), 
           ylim = c(lvlbbox["ymin"], lvlbbox["ymax"]))),
  gg_cc_theme_no_sf,
  list(theme_void()),
  list(default_theme),
  list(theme(legend.box.margin = margin(t = 0)))
)

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

convert_number_tens <- \(x) convert_number(round(x / 10) * 10)

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
