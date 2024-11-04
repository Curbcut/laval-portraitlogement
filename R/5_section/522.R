library(tsibble)
laval_STR <- qs::qread("data/laval_STR.qs")

str_plot <- 
laval_STR |> 
  ggplot(aes(x = month, y = value, colour = name)) +
  geom_line(size = 1) +
  scale_colour_manual(labels = c(active = "Annonces actives",
                                 FREH = "Logements entiers fréquemment loués"),
                                 values = curbcut_colors$left_5$fill[c(3,6)]) +
  labs(
    x = NULL,
    y = "Nombre d'annonces",
    colour = NULL
  ) +
  graph_theme

ggsave_pdf_png(str_plot, filename = "outputs/5/short_term_rentals.pdf",
               width= 6.5, height = 4)
