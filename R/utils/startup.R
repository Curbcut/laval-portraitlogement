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
        legend.text = element_text(size = 11),
        axis.title.y = element_text(size = 12))

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
