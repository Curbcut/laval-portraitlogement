ggsave_pdf_png <- \(plot, filename, width, height, ...) {
  ggsave(plot = plot, filename = filename, width = width, height = height, ...)
  
  filename_png <- gsub("\\.pdf", "\\.png", filename)
  
  # Use magick to read PDF with specified DPI and exact inch size
  # Set density for high-quality conversion and use geometry for inch scaling
  image <- magick::image_read_pdf(filename)
  
  # Save the PNG version with high quality
  magick::image_write(image, path = filename_png, format = "png")
}
