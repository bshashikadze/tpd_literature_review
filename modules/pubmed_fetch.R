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

#' Fetch papers in batches with details
#' 
#' @param pmids Vector of PubMed IDs
#' @param batch_size Number of papers per batch
#' @return Data frame with paper details
fetch_paper_batch <- function(pmids, batch_size = 100) {
  library(rentrez)
  library(XML)
  
  total <- length(pmids)
  papers_list <- list()
  
  # Process in batches
  num_batches <- ceiling(total / batch_size)
  
  for (batch_num in 1:num_batches) {
    start_idx <- (batch_num - 1) * batch_size + 1
    end_idx <- min(batch_num * batch_size, total)
    batch_pmids <- pmids[start_idx:end_idx]
    
    cat(sprintf("Fetching batch %d/%d (PMIDs %d-%d)...\n", 
                batch_num, num_batches, start_idx, end_idx))
    
    tryCatch({
      # Fetch summaries for batch
      summaries <- entrez_summary(db = "pubmed", id = batch_pmids)
      
      # Fetch XML for batch (to get abstracts and DOIs)
      xml_data <- entrez_fetch(db = "pubmed", 
                               id = batch_pmids, 
                               rettype = "xml", 
                               parsed = TRUE)
      
      # Extract data for each paper in batch
      for (i in seq_along(batch_pmids)) {
        pmid <- batch_pmids[i]
        
        # Get individual summary
        if (length(batch_pmids) == 1) {
          individual_summary <- summaries
        } else {
          individual_summary <- summaries[[i]]
        }
        
        # Extract paper data (pass the full XML, extract_paper_data will parse it)
        paper_data <- extract_paper_data(pmid, individual_summary, xml_data)
        papers_list[[length(papers_list) + 1]] <- paper_data
      }
      
      # Rate limiting - wait between batches
      if (batch_num < num_batches) {
        Sys.sleep(0.5)
      }
      
    }, error = function(e) {
      warning(sprintf("Error fetching batch %d: %s", batch_num, e$message))
      # Add placeholder data for failed batch
      for (pmid in batch_pmids) {
        papers_list[[length(papers_list) + 1]] <- list(
          pmid = pmid,
          title = NA_character_,
          authors = NA_character_,
          journal = NA_character_,
          pub_date = NA_character_,
          doi = NA_character_,
          abstract = NA_character_
        )
      }
    })
  }
  
  # Convert to data frame
  papers_df <- do.call(rbind.data.frame, papers_list)
  papers_df[] <- lapply(papers_df, as.character)
  
  return(papers_df)
}

#' Extract paper data from PubMed summary and XML
#' 
#' @param pmid PubMed ID
#' @param summaries entrez_summary object for this paper
#' @param xml_data Parsed XML document (may contain multiple papers)
#' @return Named list with paper data
extract_paper_data <- function(pmid, summaries, xml_data) {
  library(XML)
  
  tryCatch({
    # Extract from summary
    title <- summaries$title
    
    # Extract authors
    if (!is.null(summaries$authors) && length(summaries$authors) > 0) {
      authors <- paste(summaries$authors$name, collapse = ", ")
    } else {
      authors <- NA_character_
    }
    
    journal <- summaries$source
    pub_date <- summaries$pubdate
    
    # Extract DOI and abstract from XML for this specific PMID
    doi <- NA_character_
    abstract <- NA_character_
    
    if (!is.null(xml_data)) {
      # Find the PubmedArticle node for this specific PMID
      pmid_xpath <- sprintf("//PubmedArticle[MedlineCitation/PMID='%s']", pmid)
      article_node <- getNodeSet(xml_data, pmid_xpath)
      
      if (length(article_node) > 0) {
        article_node <- article_node[[1]]
        
        # Extract DOI
        doi_nodes <- xpathSApply(article_node, ".//ArticleId[@IdType='doi']", xmlValue)
        if (length(doi_nodes) > 0) {
          doi <- doi_nodes[1]
        }
        
        # Extract abstract
        abstract_nodes <- xpathSApply(article_node, ".//AbstractText", xmlValue)
        if (length(abstract_nodes) > 0) {
          abstract <- paste(abstract_nodes, collapse = " ")
        }
      }
    }
    
    return(list(
      pmid = pmid,
      title = title,
      authors = authors,
      journal = journal,
      pub_date = pub_date,
      doi = doi,
      abstract = abstract
    ))
    
  }, error = function(e) {
    warning(sprintf("Error extracting data for PMID %s: %s", pmid, e$message))
    return(list(
      pmid = pmid,
      title = NA_character_,
      authors = NA_character_,
      journal = NA_character_,
      pub_date = NA_character_,
      doi = NA_character_,
      abstract = NA_character_
    ))
  })
}