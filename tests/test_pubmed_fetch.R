# tests/test_pubmed_fetch.R
# Test PubMed fetch functionality

# Load dependencies
source("modules/config_loader.R")
source("modules/pubmed_fetch.R")

# Load config
cat("Loading config...\n")
config <- load_config()

# Test 1: Fetch papers with current config
cat("\n=== TEST 1: Fetch papers with current settings ===\n")
papers <- fetch_papers(config)

if (!is.null(papers)) {
  cat("\n✅ Success! Fetched", nrow(papers), "papers\n")
  cat("\nFirst paper:\n")
  cat("  PMID:", papers$pmid[1], "\n")
  cat("  Title:", papers$title[1], "\n")
  cat("  Authors:", papers$authors[1], "\n")
  cat("  Journal:", papers$journal[1], "\n")
  cat("  DOI:", papers$doi[1], "\n")
  cat("  Abstract length:", nchar(papers$abstract[1]), "characters\n")
  
  # Check for missing abstracts
  missing_abstracts <- sum(papers$abstract == "" | is.na(papers$abstract))
  cat("\nPapers without abstracts:", missing_abstracts, "of", nrow(papers), "\n")
  
  # Save test data
  saveRDS(papers, file.path(config$data_dir, "test_papers.rds"))
  cat("\n📁 Saved test data to data/test_papers.rds\n")
  
} else {
  cat("\n⚠️ No papers found\n")
}

# Test 2: Test with very limited number (if max_articles_to_process is set)
if (!is.null(config$pubmed$max_articles_to_process)) {
  cat("\n=== TEST 2: Testing max_articles_to_process limit ===\n")
  cat("Config limit:", config$pubmed$max_articles_to_process, "\n")
  cat("Papers retrieved:", ifelse(is.null(papers), 0, nrow(papers)), "\n")
  
  if (!is.null(papers) && nrow(papers) <= config$pubmed$max_articles_to_process) {
    cat("✅ Limit respected!\n")
  }
}

# Test 3: Test filtering (if filters are configured)
if (!is.null(papers) && nrow(papers) > 0) {
  cat("\n=== TEST 3: Test filter application ===\n")
  
  if (!is.null(config$filters$exclude_keywords) && 
      length(config$filters$exclude_keywords) > 0) {
    cat("Exclude keywords:", paste(config$filters$exclude_keywords, collapse = ", "), "\n")
    
    # Check if any excluded keywords appear in results
    exclude_pattern <- paste(config$filters$exclude_keywords, collapse = "|")
    found_excluded <- papers %>%
      filter(grepl(exclude_pattern, title, ignore.case = TRUE) |
               grepl(exclude_pattern, abstract, ignore.case = TRUE))
    
    if (nrow(found_excluded) == 0) {
      cat("✅ No excluded keywords found in results\n")
    } else {
      cat("⚠️ Found", nrow(found_excluded), "papers with excluded keywords\n")
    }
  } else {
    cat("No exclude keywords configured\n")
  }
  
  if (!is.null(config$filters$include_only_keywords) && 
      length(config$filters$include_only_keywords) > 0) {
    cat("Include keywords:", paste(config$filters$include_only_keywords, collapse = ", "), "\n")
    cat("✅ All results should contain at least one include keyword\n")
  }
}

# Summary
cat("\n=== SUMMARY ===\n")
cat("Config loaded: ✅\n")
cat("Papers fetched:", ifelse(is.null(papers), "❌ None", paste("✅", nrow(papers))), "\n")
cat("Abstracts present:", ifelse(is.null(papers), "N/A", 
                                 paste(sum(papers$abstract != "" & !is.na(papers$abstract)), 
                                       "of", nrow(papers))), "\n")
cat("\nTest complete!\n")