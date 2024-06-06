library(pdftools)
library(stringr)

# Function to search for keywords in a single PDF file and extract variations
search_keywords_in_pdf <- function(pdf_path, keywords) {
  # Extract text from PDF
  text <- pdf_text(pdf_path)
  
  # Combine text pages into a single string
  combined_text <- paste(text, collapse = " ")
  
  # Create a list to store keyword occurrences and variations
  keyword_variations <- list()
  
  # Search for each keyword and extract occurrences
  for (keyword in keywords) {
    pattern <- paste0("\\b(", keyword, "\\w*)")
    matches <- str_extract_all(tolower(combined_text), pattern)[[1]]
    if (length(matches) > 0) {
      variations_count <- table(matches)
      keyword_variations[[keyword]] <- as.list(variations_count)
    }
  }
  
  return(keyword_variations)
}

# Function to search through multiple PDF files and gather results
search_keywords_in_pdfs <- function(pdf_folder, keywords) {
  # List all PDF files in the folder
  pdf_files <- list.files(pdf_folder, pattern = "\\.pdf$", full.names = TRUE)
  
  # Create a list to store results
  results <- list()
  
  # Loop through each PDF file and search for keywords
  for (pdf_file in pdf_files) {
    keyword_variations <- search_keywords_in_pdf(pdf_file, keywords)
    if (length(keyword_variations) > 0) {
      results[[basename(pdf_file)]] <- keyword_variations
    }
  }
  
  return(results)
}

# Define the folder containing your PDF files
pdf_folder <- "C:/Users/skl448/Desktop/phd/data/sagas"

# Define the keywords to search for
keywords <- c("sailcloth")

# Search for keywords in the PDF files
results <- search_keywords_in_pdfs(pdf_folder, keywords)

# Filter results to exclude PDFs without the keyword
filtered_results <- results[lengths(results) > 0]

# Print the filtered results
print(filtered_results)

# Create a summary of keyword counts per saga
keyword_summary <- data.frame(file = character(), variation = character(), count = integer(), stringsAsFactors = FALSE)

for (saga in names(filtered_results)) {
  for (keyword in names(filtered_results[[saga]])) {
    for (variation in names(filtered_results[[saga]][[keyword]])) {
      count <- filtered_results[[saga]][[keyword]][[variation]]
      keyword_summary <- rbind(keyword_summary, data.frame(file = saga, variation = variation, count = count, stringsAsFactors = FALSE))
    }
  }
}

# Print the keyword summary
print(keyword_summary)
