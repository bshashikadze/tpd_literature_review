# modules/pubmed_fetch.R
# PubMed paper fetching module

library(rentrez)
library(xml2)
library(dplyr)
library(lubridate)
library(purrr)

#' Fetch papers from PubMed based on config settings
#' @param config Config list from load_config()
#' @return data.frame with columns: pmid, title, authors, journal, pub_date, doi, abstract
#' @return NULL if no papers found
fetch_papers <- function(config) {
  message("========================================")
  message("Fetching papers from PubMed...")
  message("========================================")
  
  # Calculate date range
  end_date <- Sys.Date()
  start_date <- end_date - config$pubmed$days_back
  
  # Format dates for PubMed (YYYY/MM/DD)
  date_query <- sprintf("%s:%s[PDAT]", 
                        format(start_date, "%Y/%m/%d"),
                        format(end_date, "%Y/%m/%d"))
  
  # Combine search terms with date range
  full_query <- paste0("(", config$pubmed$search_terms, ") AND ", date_query)
  
  message(sprintf("Search period: %s to %s (%d days)", 
                  start_date, end_date, config$pubmed$days_back))
  message(sprintf("Search query: %s", full_query))
  
  # Search PubMed for PMIDs
  tryCatch({
    search_results <- entrez_search(
      db = "pubmed",
      term = full_query,
      retmax = config$pubmed$max_results,
      use_history = TRUE
    )
    
    if (search_results$count == 0) {
      message("No papers found matching search criteria")
      return(NULL)
    }
    
    message(sprintf("Found %d papers (retrieving up to %d)", 
                    search_results$count, 
                    min(search_results$count, config$pubmed$max_results)))
    
    pmids <- search_results$ids
    
    # Apply max_articles_to_process limit if specified
    if (!is.null(config$pubmed$max_articles_to_process)) {
      original_count <- length(pmids)
      pmids <- head(pmids, config$pubmed$max_articles_to_process)
      message(sprintf("Limiting to %d papers for processing (from %d total)", 
                      length(pmids), original_count))
    }
    
    # Fetch paper details in batches
    papers_df <- fetch_paper_batch(pmids, batch_size = 100)
    
    # Apply filters
    if (!is.null(papers_df) && nrow(papers_df) > 0) {
      papers_df <- apply_filters(papers_df, config)
    }
    
    message(sprintf("Final count after filtering: %d papers", 
                    ifelse(is.null(papers_df), 0, nrow(papers_df))))
    message("========================================")
    
    return(papers_df)
    
  }, error = function(e) {
    message(sprintf("ERROR fetching papers: %s", e$message))
    return(NULL)
  })
}

#' Apply keyword filters to papers
#' @param papers_df data.frame of papers
#' @param config Config list
#' @return Filtered data.frame
apply_filters <- function(papers_df, config) {
  if (is.null(papers_df) || nrow(papers_df) == 0) {
    return(papers_df)
  }
  
  original_count <- nrow(papers_df)
  
  # Exclude keywords
  if (!is.null(config$filters$exclude_keywords) && 
      length(config$filters$exclude_keywords) > 0) {
    
    exclude_pattern <- paste(config$filters$exclude_keywords, collapse = "|")
    
    papers_df <- papers_df %>%
      filter(!grepl(exclude_pattern, title, ignore.case = TRUE) &
               !grepl(exclude_pattern, abstract, ignore.case = TRUE))
    
    excluded_count <- original_count - nrow(papers_df)
    if (excluded_count > 0) {
      message(sprintf("Excluded %d papers based on keywords", excluded_count))
    }
  }
  
  # Include only keywords
  if (!is.null(config$filters$include_only_keywords) && 
      length(config$filters$include_only_keywords) > 0) {
    
    include_pattern <- paste(config$filters$include_only_keywords, collapse = "|")
    
    before_include <- nrow(papers_df)
    papers_df <- papers_df %>%
      filter(grepl(include_pattern, title, ignore.case = TRUE) |
               grepl(include_pattern, abstract, ignore.case = TRUE))
    
    filtered_count <- before_include - nrow(papers_df)
    if (filtered_count > 0) {
      message(sprintf("Filtered to %d papers based on include keywords", nrow(papers_df)))
    }
  }
  
  return(papers_df)
}

#' Fetch papers in batches to respect API limits
#' @param pmids Vector of PubMed IDs
#' @param batch_size Number of papers per batch (default 100)
#' @return data.frame with paper details
fetch_paper_batch <- function(pmids, batch_size = 100) {
  if (length(pmids) == 0) {
    return(NULL)
  }
  
  # Split PMIDs into batches
  n_batches <- ceiling(length(pmids) / batch_size)
  batches <- split(pmids, ceiling(seq_along(pmids) / batch_size))
  
  message(sprintf("Fetching %d papers in %d batches...", length(pmids), n_batches))
  
  all_papers <- list()
  
  for (i in seq_along(batches)) {
    batch_pmids <- batches[[i]]
    
    tryCatch({
      # Fetch summaries for batch
      summaries <- entrez_summary(db = "pubmed", id = batch_pmids)
      
      # Fetch full records (for abstracts)
      fetch_result <- entrez_fetch(
        db = "pubmed",
        id = batch_pmids,
        rettype = "xml"
      )
      
      # Parse XML
      xml_data <- read_xml(fetch_result)
      
      # Extract data for each paper
      batch_papers <- map_dfr(batch_pmids, function(pmid) {
        extract_paper_data(pmid, summaries, xml_data)
      })
      
      all_papers[[i]] <- batch_papers
      
      # Progress message
      if (i %% 5 == 0 || i == n_batches) {
        message(sprintf("  Processed %d/%d batches (%d papers)", 
                        i, n_batches, i * batch_size))
      }
      
      # Rate limiting - wait 0.5 seconds between batches
      if (i < n_batches) {
        Sys.sleep(0.5)
      }
      
    }, error = function(e) {
      message(sprintf("  ERROR in batch %d: %s", i, e$message))
    })
  }
  
  # Combine all batches
  if (length(all_papers) > 0) {
    papers_df <- bind_rows(all_papers)
    return(papers_df)
  } else {
    return(NULL)
  }
}

#' Extract paper data from PubMed XML and summary
#' @param pmid PubMed ID
#' @param summaries entrez_summary object
#' @param xml_data XML document
#' @return data.frame row with paper details
extract_paper_data <- function(pmid, summaries, xml_data) {
  tryCatch({
    # Get summary data
    summary <- summaries[[pmid]]
    
    # Extract from summary
    title <- ifelse(is.null(summary$title), NA, summary$title)
    journal <- ifelse(is.null(summary$source), NA, summary$source)
    pub_date <- ifelse(is.null(summary$pubdate), NA, summary$pubdate)
    
    # Extract authors
    authors <- NA
    if (!is.null(summary$authors) && length(summary$authors) > 0) {
      author_names <- sapply(summary$authors, function(a) {
        if (!is.null(a$name)) a$name else NA
      })
      author_names <- author_names[!is.na(author_names)]
      if (length(author_names) > 0) {
        authors <- paste(author_names, collapse = ", ")
      }
    }
    
    # Extract DOI from XML
    doi <- NA
    article_node <- xml_find_first(xml_data, sprintf("//PubmedArticle[MedlineCitation/PMID='%s']", pmid))
    if (!is.na(article_node) && !is.null(article_node)) {
      doi_node <- xml_find_first(article_node, ".//ArticleId[@IdType='doi']")
      if (!is.na(doi_node) && !is.null(doi_node)) {
        doi <- xml_text(doi_node)
      }
    }
    
    # Extract abstract from XML
    abstract <- NA
    if (!is.na(article_node) && !is.null(article_node)) {
      abstract_nodes <- xml_find_all(article_node, ".//AbstractText")
      if (length(abstract_nodes) > 0) {
        abstract_parts <- xml_text(abstract_nodes)
        abstract <- paste(abstract_parts, collapse = " ")
      }
    }
    
    # Create data frame row
    data.frame(
      pmid = pmid,
      title = title,
      authors = authors,
      journal = journal,
      pub_date = pub_date,
      doi = ifelse(is.na(doi), "", doi),
      abstract = ifelse(is.na(abstract), "", abstract),
      stringsAsFactors = FALSE
    )
    
  }, error = function(e) {
    # Return row with NA values if extraction fails
    data.frame(
      pmid = pmid,
      title = NA,
      authors = NA,
      journal = NA,
      pub_date = NA,
      doi = NA,
      abstract = NA,
      stringsAsFactors = FALSE
    )
  })
}