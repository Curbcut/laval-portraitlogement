gt_save_word <- function(gt_table, file_path) {
  # Convert gt table to OOXML string
  ooxml_str <- as_word(gt_table)
  
  # Create a new Word document
  doc <- read_docx()
  
  # Add the OOXML table to the document
  doc <- body_add_xml(doc, ooxml_str)
  
  # Save the document to the specified file path
  print(doc, target = file_path)
}
