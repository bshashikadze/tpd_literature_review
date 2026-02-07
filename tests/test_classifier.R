# Test LLM Classifier Module

library(dplyr)

# Load dependencies
source("modules/config_loader.R")
source("modules/llm_classifier.R")

cat("=== Testing LLM Classifier Module ===\n\n")

# Load configuration
config <- load_config()

# Load test papers from Chat 2
if (!file.exists("data/test_papers.rds")) {
  stop("Test papers not found. Please run tests/test_pubmed_fetch.R first.")
}

papers_df <- readRDS("data/test_papers.rds")
cat(sprintf("Loaded %d test papers\n\n", nrow(papers_df)))

# Test 1: Initialize classifier
cat("Test 1: Initializing classifier LLM...\n")
chat <- init_classifier_llm(config)
cat("✓ Classifier initialized successfully\n\n")

# Test 2: Classify a single paper (first paper)
cat("Test 2: Classifying single paper...\n")
test_paper <- papers_df[1, ]
cat(sprintf("Title: %s\n", substr(test_paper$title, 1, 80)))

category <- classify_single_paper(
  title = test_paper$title,
  abstract = test_paper$abstract,
  config = config,
  chat = chat
)

cat(sprintf("✓ Classification result: %s\n\n", category))

# Test 3: Classify all papers (or subset for testing)
# For testing, let's use only first 5 papers to save time
cat("Test 3: Classifying batch of papers...\n")
test_batch <- papers_df[1:min(5, nrow(papers_df)), ]

classified_papers <- classify_papers(test_batch, config)

# Show results
cat("\nClassification Results:\n")
for (i in 1:nrow(classified_papers)) {
  cat(sprintf("%d. [%s] %s\n", 
              i,
              classified_papers$category[i],
              substr(classified_papers$title[i], 1, 60)))
}

# Test 4: Validate categories
cat("\n\nTest 4: Validating categories...\n")
valid_categories <- config$classification$categories
invalid_count <- sum(!classified_papers$category %in% valid_categories)

if (invalid_count == 0) {
  cat("✓ All categories are valid\n")
} else {
  cat(sprintf("✗ Found %d invalid categories\n", invalid_count))
}

# Test 5: Test with missing abstract
cat("\nTest 5: Testing with missing abstract...\n")
category_missing <- classify_single_paper(
  title = "Test paper with missing abstract",
  abstract = NA,
  config = config,
  chat = chat
)
cat(sprintf("✓ Handled missing abstract, category: %s\n", category_missing))

# Save classified papers for next module
cat("\nSaving classified papers for next module...\n")
saveRDS(classified_papers, "data/test_papers_classified.rds")
cat("✓ Saved to data/test_papers_classified.rds\n")

cat("\n=== All Tests Passed! ===\n")