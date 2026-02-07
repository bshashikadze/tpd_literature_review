# Configuration loading and validation for TPD Literature Review System

library(yaml)

#' Load configuration from YAML file
#' 
#' @param config_path Path to config.yaml file
#' @return List containing all configuration values
#' @export
load_config <- function(config_path = "config.yaml") {
  
  # Check if file exists
  if (!file.exists(config_path)) {
    stop(sprintf("Config file not found: %s", config_path))
  }
  
  # Load YAML
  tryCatch({
    config <- yaml::read_yaml(config_path)
    message(sprintf("✓ Loaded configuration from: %s", config_path))
    
    # Validate config
    validate_config(config)
    
    return(config)
    
  }, error = function(e) {
    stop(sprintf("Error loading config file: %s", e$message))
  })
}

#' Get nested configuration value
#' 
#' @param config Configuration list
#' @param path Dot-separated path (e.g., "llm.model")
#' @return Value at the specified path
#' @export
get_config_value <- function(config, path) {
  
  # Split path by dots
  keys <- strsplit(path, "\\.")[[1]]
  
  # Navigate through nested list
  value <- config
  for (key in keys) {
    if (!key %in% names(value)) {
      stop(sprintf("Config path not found: %s", path))
    }
    value <- value[[key]]
  }
  
  return(value)
}

#' Validate configuration structure and required fields
#' 
#' @param config Configuration list
#' @return TRUE if valid, stops with error if invalid
#' @export
validate_config <- function(config) {
  
  # Required top-level sections
  required_sections <- c("project_name", "llm", "pubmed", "classification", 
                         "summarization", "report", "email", "schedule")
  
  missing_sections <- setdiff(required_sections, names(config))
  if (length(missing_sections) > 0) {
    stop(sprintf("Missing required config sections: %s", 
                 paste(missing_sections, collapse = ", ")))
  }
  
  # Validate LLM settings
  if (is.null(config$llm$model)) {
    stop("llm.model is required")
  }
  
  # Validate PubMed settings
  if (is.null(config$pubmed$search_terms)) {
    stop("pubmed.search_terms is required")
  }
  if (is.null(config$pubmed$days_back)) {
    stop("pubmed.days_back is required")
  }
  
  # Validate classification settings
  if (is.null(config$classification$categories) || 
      length(config$classification$categories) == 0) {
    stop("classification.categories must have at least one category")
  }
  if (is.null(config$classification$prompt_template)) {
    stop("classification.prompt_template is required")
  }
  
  # Validate summarization settings
  if (is.null(config$summarization$prompt_template)) {
    stop("summarization.prompt_template is required")
  }
  
  # Validate email settings (if enabled)
  if (isTRUE(config$email$enabled)) {
    if (is.null(config$email$from_email) || is.null(config$email$to_email)) {
      warning("Email is enabled but from_email or to_email is missing")
    }
  }
  
  message("✓ Configuration validation passed")
  return(TRUE)
}

#' Print configuration summary
#' 
#' @param config Configuration list
#' @export
print_config_summary <- function(config) {
  cat("\n=== Configuration Summary ===\n")
  cat(sprintf("Project: %s\n", config$project_name))
  cat(sprintf("LLM Model: %s\n", config$llm$model))
  cat(sprintf("PubMed days back: %d\n", config$pubmed$days_back))
  cat(sprintf("Max articles to process: %s\n", 
              ifelse(is.null(config$pubmed$max_articles_to_process), 
                     "ALL", config$pubmed$max_articles_to_process)))
  cat(sprintf("Classification categories: %s\n", 
              paste(config$classification$categories, collapse = ", ")))
  cat(sprintf("Email enabled: %s\n", config$email$enabled))
  cat(sprintf("Schedule: %s at %s\n", config$schedule$day, config$schedule$time))
  cat("============================\n\n")
}