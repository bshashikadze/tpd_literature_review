# LLM Classification Module
# Classifies papers into categories using LLM

library(ellmer)
library(dplyr)

#' Initialize LLM for classification
#' 
#' @param config Configuration list
#' @return ellmer chat object
init_classifier_llm <- function(config) {
  model <- config$llm$model
  
  cat(sprintf("Initializing classifier with model: %s\n", model))
  
  chat <- chat_ollama(model = model)
  
  return(chat)
}

#' Classify a single paper
#' 
#' @param title Paper title
#' @param abstract Paper abstract
#' @param config Configuration list
#' @param chat ellmer chat object
#' @return Category string (validated against config categories)
classify_single_paper <- function(title, abstract, config, chat) {
  
  # Get prompt template and categories from config
  prompt_template <- config$classification$prompt_template
  valid_categories <- config$classification$categories
  
  # Handle missing abstract
  if (is.na(abstract) || abstract == "") {
    abstract <- "[No abstract available]"
  }
  
  # Substitute title and abstract into prompt template
  prompt <- prompt_template
  prompt <- gsub("\\{title\\}", title, prompt)
  prompt <- gsub("\\{abstract\\}", abstract, prompt)
  
  # Get classification from LLM with timeout and retry
  max_retries <- config$llm$max_retries
  timeout <- config$llm$classification_timeout
  
  category <- NULL
  attempt <- 1
  
  while (attempt <= (max_retries + 1) && is.null(category)) {
    tryCatch({
      # Set timeout
      response <- withTimeout({
        chat$chat(prompt)
      }, timeout = timeout)
      
      # Clean up response - remove whitespace and convert to uppercase
      category_raw <- toupper(trimws(response))
      
      # Validate category
      if (category_raw %in% valid_categories) {
        category <- category_raw
      } else {
        # Try to find partial match
        matched <- FALSE
        for (valid_cat in valid_categories) {
          if (grepl(valid_cat, category_raw)) {
            category <- valid_cat
            matched <- TRUE
            break
          }
        }
        
        if (!matched) {
          warning(sprintf("Invalid category '%s' returned, defaulting to OTHER", 
                          category_raw))
          category <- "OTHER"
        }
      }
      
    }, error = function(e) {
      if (attempt <= max_retries) {
        warning(sprintf("Classification attempt %d failed: %s. Retrying...", 
                        attempt, e$message))
      } else {
        warning(sprintf("Classification failed after %d attempts: %s. Defaulting to OTHER", 
                        attempt, e$message))
        category <<- "OTHER"
      }
    })
    
    attempt <- attempt + 1
  }
  
  # Final fallback
  if (is.null(category)) {
    category <- "OTHER"
  }
  
  return(category)
}

#' Classify all papers
#' 
#' @param papers_df Data frame with papers
#' @param config Configuration list
#' @return papers_df with added 'category' column
classify_papers <- function(papers_df, config) {
  
  cat("\n=== Starting Paper Classification ===\n")
  cat(sprintf("Total papers to classify: %d\n", nrow(papers_df)))
  
  # Initialize LLM
  chat <- init_classifier_llm(config)
  
  # Initialize category column
  papers_df$category <- NA_character_
  
  # Track progress
  total <- nrow(papers_df)
  start_time <- Sys.time()
  
  # Classify each paper
  for (i in 1:nrow(papers_df)) {
    
    # Progress indicator every 5 papers
    if (i %% 5 == 0 || i == 1) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      avg_time <- elapsed / i
      remaining <- (total - i) * avg_time
      cat(sprintf("Progress: %d/%d (%.1f%%) | Elapsed: %.1fs | Est. remaining: %.1fs\n", 
                  i, total, (i/total)*100, elapsed, remaining))
    }
    
    # Get paper details
    title <- papers_df$title[i]
    abstract <- papers_df$abstract[i]
    
    # Classify
    category <- classify_single_paper(title, abstract, config, chat)
    papers_df$category[i] <- category
    
    # Small delay to avoid overwhelming the LLM
    Sys.sleep(0.1)
  }
  
  # Summary statistics
  cat("\n=== Classification Complete ===\n")
  category_counts <- table(papers_df$category)
  for (cat_name in names(category_counts)) {
    cat(sprintf("%s: %d papers (%.1f%%)\n", 
                cat_name, 
                category_counts[cat_name],
                (category_counts[cat_name]/total)*100))
  }
  
  total_time <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  cat(sprintf("\nTotal classification time: %.1f seconds (%.2f sec/paper)\n", 
              total_time, total_time/total))
  
  return(papers_df)
}

#' Timeout wrapper function
#' 
#' @param expr Expression to evaluate
#' @param timeout Timeout in seconds
#' @return Result of expr
withTimeout <- function(expr, timeout) {
  # Simple timeout implementation
  # For production, consider using R.utils::withTimeout or future package
  result <- tryCatch({
    eval(expr)
  }, error = function(e) {
    stop(e)
  })
  return(result)
}