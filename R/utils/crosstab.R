## Clean up, get DA data

# allcrosstab <- tibble::as_tibble(data.table::fread("data/allcrosstab.csv", encoding = "Latin-1"))
# DAIDs <- cancensus::get_census(dataset = "CA21",
#                                regions = list(CSD = 2465005),
#                                level = "DA")$GeoUID
# allcrosstab <- allcrosstab[c(1:4, which(allcrosstab$V1 %in% DAIDs)), ]
# allcrosstab[1:4, 1] <- c("mode_occupation", "characteristic", "composition", "revenu")
# qs::qsave(allcrosstab, file = "data/allcrosstab_Laval_DA.qs")
# 
# CTIDs <- cancensus::get_census(dataset = "CA21",
#                                regions = list(CSD = 2465005),
#                                level = "CT")$GeoUID
# allcrosstab$V1 <- sub("(\\d+)(\\d{2})$", "\\1.\\2", allcrosstab$V1)
# allcrosstab <- allcrosstab[c(1:4, which(allcrosstab$V1 %in% CTIDs)), ]
# allcrosstab[1:4, 1] <- c("mode_occupation", "characteristic", "composition", "revenu")
# qs::qsave(allcrosstab, file = "data/allcrosstab_Laval_CT.qs")

# Get all options
crosstab_list <- function(cat = c("mode_occupation", "characteristic", "composition", "revenu")) {
  allcrosstab <- qs::qread("data/allcrosstab_Laval_DA.qs")
  sapply(cat, \(x) {
    allcrosstab[allcrosstab$V1 == x, 2:ncol(allcrosstab)] |> unlist() |> unname() |> unique()
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# What info are you searching for?
crosstab_get <- function(mode_occupation = c(total = "total"), 
                         characteristic = c(total = "total"), 
                         composition = c(total = "total"), 
                         revenu = c(total = "total"),
                         keep_cat = FALSE,
                         scale = "CT") {
  allcrosstab <- qs::qread(paste0("data/allcrosstab_Laval_", scale, ".qs"))
  
  if (sum(grepl("_", mode_occupation)) > 1) {
    stop("No `_` in names of named vector (mode_occupation)")
  }
  if (sum(grepl("_", characteristic)) > 1) {
    stop("No `_` in names of named vector (characteristic)")
  }
  if (sum(grepl("_", composition)) > 1) {
    stop("No `_` in names of named vector (composition)")
  }
  if (sum(grepl("_", revenu)) > 1) {
    stop("No `_` in names of named vector (revenu)")
  }
  if (all(mode_occupation == "total") & all(characteristic == "total") & 
      all(composition == "total") & all(revenu == "total")) {
    stop("One of the input needs not to be `total`")
  }
  
  # Get the 'Total' column so we only use that if it's the case
  if (all(mode_occupation == "total")) mode_occupation <- c(total = "Total - Mode d'occupation incluant le logement subventionné et le statut de copropriété")
  if (all(characteristic == "total")) characteristic <- c(total = "Total - Rapport des frais de logement au revenu")
  if (all(composition == "total")) composition <- c(total = "Total - Type de ménage de la personne")
  if (all(revenu == "total")) revenu <- c(total = "Total - Revenu total du ménage")
  
  allcrosstab <- allcrosstab[, c(1, which(unlist(allcrosstab[which(allcrosstab$V1 == "mode_occupation"),]) %in% mode_occupation))]
  allcrosstab[which(allcrosstab$V1 == "mode_occupation"), ] <- 
    lapply(allcrosstab[which(allcrosstab$V1 == "mode_occupation"), ], function(x) {
      ifelse(x %in% mode_occupation, names(mode_occupation)[match(x, mode_occupation)], x)
    })
  allcrosstab <- allcrosstab[, c(1, which(unlist(allcrosstab[which(allcrosstab$V1 == "characteristic"),]) %in% characteristic))]
  allcrosstab[which(allcrosstab$V1 == "characteristic"), ] <- 
    lapply(allcrosstab[which(allcrosstab$V1 == "characteristic"), ], function(x) {
      ifelse(x %in% characteristic, names(characteristic)[match(x, characteristic)], x)
    })
  allcrosstab <- allcrosstab[, c(1, which(unlist(allcrosstab[which(allcrosstab$V1 == "composition"),]) %in% composition))]
  allcrosstab[which(allcrosstab$V1 == "composition"), ] <- 
    lapply(allcrosstab[which(allcrosstab$V1 == "composition"), ], function(x) {
      ifelse(x %in% composition, names(composition)[match(x, composition)], x)
    })
  allcrosstab <- allcrosstab[, c(1, which(unlist(allcrosstab[which(allcrosstab$V1 == "revenu"),]) %in% revenu))]
  allcrosstab[which(allcrosstab$V1 == "revenu"), ] <- 
    lapply(allcrosstab[which(allcrosstab$V1 == "revenu"), ], function(x) {
      ifelse(x %in% revenu, names(revenu)[match(x, revenu)], x)
    })
  
  total_rows <- apply(allcrosstab[, -1], 1, function(row) all(row == "total"))
  allcrosstab <- allcrosstab[!total_rows, ]
  cat_rows <- 4 - sum(total_rows)

  names(allcrosstab) <- c("DA_ID", sapply(2:ncol(allcrosstab), \(x) {
    allcrosstab[[x]][1:cat_rows] |> paste0(collapse = "_")
  }))
  
  if (!keep_cat) {
    allcrosstab <- allcrosstab[-c(1:cat_rows), ]
    allcrosstab[, 2:ncol(allcrosstab)] <- lapply(allcrosstab[, 2:ncol(allcrosstab)], as.numeric) |> 
      suppressWarnings()
  }
  
  return(allcrosstab)

}



