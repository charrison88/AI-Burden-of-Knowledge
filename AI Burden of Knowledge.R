library(httr)
library(retry)
library(dplyr)
library(ggplot2)
library(tidyr)

# Fetch the API key from environment variables. You need to get your own API key. 
api_key <- Sys.getenv("S2_API_KEY")
if (api_key == "") {
  stop("API key not found in environment variables.")
}

# Define the retry configuration
retry_config <- list(
  max_attempts = 5,
  retry_times = c(1, 2, 4, 8, 16),
  max_total_wait_time = Inf,
  terminate_on = 0L,
  pause_min = 1,
  pause_cap = 1
)

# Function to make an API request
make_request <- function(api_key, query, fields, limit, publication_types, s2FieldsOfStudy, year, min_citation_count, token) {
  response <- RETRY(
    "GET",
    "https://api.semanticscholar.org/graph/v1/paper/search/bulk",
    add_headers(`x-api-key` = api_key),
    query = list(
      query = query,
      fields = fields,
      limit = limit,
      publicationTypes = publication_types,
      s2FieldsOfStudy = s2FieldsOfStudy,
      year = year,
      minCitationCount = min_citation_count,
      token = token
    ),
    times = retry_config$max_attempts,
    pause_base = retry_config$pause_min,
    pause_cap = retry_config$pause_cap
  )
  stop_for_status(response)
  return(response)
}

# Function to process API response data and store in a list
process_data <- function(data) {
  papers <- data$data
  paper_list <- vector("list", length(papers))

  for (i in seq_along(papers)) {
    paper <- papers[[i]]
    new_row <- data.frame(
      Title = if (!is.null(paper$title)) paper$title else NA,
      Year = if (!is.null(paper$year)) paper$year else NA,
      Number_of_Authors = if (!is.null(length(paper$authors))) length(paper$authors) else NA,
      References = if (!is.null(paper$referenceCount)) paper$referenceCount else NA,
      Citations = if (!is.null(paper$citationCount)) paper$citationCount else NA,
      Publication_Venue_Name = if (!is.null(paper$publicationVenue$name)) paper$publicationVenue$name else NA,
      stringsAsFactors = FALSE
    )
    paper_list[[i]] <- new_row
  }

  return(paper_list)
}

# Define the search query and filters
query <- "Artificial Intelligence | AI | Neural Network* | Deep Learning | Reinforcement Learning | Machine Learning | Computer Vision | Natural Language Processing | Robotics)"
  
fields <- "title,year,authors,referenceCount,citationCount,publicationVenue"
limit <- 1000  # This is the limit for Bulk Search
publication_types <- "JournalArticle,ConferencePaper"
s2FieldsOfStudy <- "Computer Science,Mathematics,Artificial Intelligence,Machine Learning"
year <- "1980-2023"
min_citation_count <- 0

# Initialize a list to accumulate all results
all_papers_list <- list()
initial_response <- make_request(api_key, query, fields, limit, publication_types, s2FieldsOfStudy, year, min_citation_count, token = NULL)
initial_data <- content(initial_response, "parsed")
all_papers_list <- c(all_papers_list, process_data(initial_data))

print(initial_data$total)

# Initialize variables
all_papers_list <- list()
cycle_count <- 1
total_cycles <- ceiling(initial_data$total / limit)
start_time <- Sys.time()
token <- initial_data$token

while (!is.null(token)) {
  iteration_start <- Sys.time() 
  next_response <- make_request(api_key, query, fields, limit, publication_types, s2FieldsOfStudy, year, min_citation_count, token)
  next_data <- content(next_response, "parsed")
  all_papers_list <- c(all_papers_list, process_data(next_data))
  token <- next_data$token
  cycle_count <- cycle_count + 1
  iteration_end <- Sys.time()  
  iteration_time <- as.numeric(difftime(iteration_end, iteration_start, units = "secs"))
  
  # Calculate average time per iteration
  avg_time_per_iteration <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) / cycle_count
  
  # Estimate remaining time
  estimated_time_left <- avg_time_per_iteration * (total_cycles - cycle_count)
  
  # Print How much time remaining
  if (cycle_count %% 5 == 0) {
    cat(sprintf("Progress: %d cycles completed; %d cycles remaining\n", 
                cycle_count, total_cycles - cycle_count))
    cat(sprintf("Average time per iteration: %.2f seconds\n", avg_time_per_iteration))
    cat(sprintf("Estimated time left: %.2f seconds\n", estimated_time_left))
  }
}


# Combine all rows into a single data frame
all_papers <- bind_rows(all_papers_list)
View(all_papers)



## Looking for Journals
# Group by Publication_Venue_Name and summarize the count of papers
journal_summary <- all_papers %>%
  group_by(Publication_Venue_Name) %>%
  summarize(Number_of_Papers = n(), .groups = 'drop') %>%
  left_join(
    all_papers %>%
      filter(Year < 2000) %>%
      group_by(Publication_Venue_Name) %>%
      summarize(Number_of_Papers_Before_2000 = n(), .groups = 'drop'),
    by = c("Publication_Venue_Name")
  ) %>%
  replace_na(list(Number_of_Papers_Before_2000 = 0))

View(journal_summary)



target_journals = c("International Joint Conference on Artificial Intelligence",
                    "Robotica (Cambridge. Print)",
                    "International Conference on Industrial, Engineering and Other Applications of Applied Intelligent Systems",
                    "IEEE Transactions on Systems, Man and Cybernetics",
                    "AAAI Conference on Artificial Intelligence",
                    "International Conference on Scientific Computing",
                    "Applied Artificial Intelligence",
                    "Computer",
                    "Artificial Intelligence Review",
                    "Artificial Intelligence",
                    "Annals of Mathematics and Artificial Intelligence",
                    "International Journal of Intelligent Systems"
)



# Top 20 AI journals from Google Scholar 
target_journals2 <- c(
  "Neural Information Processing Systems",
  "International Conference on Learning Representations",
  "International Conference on Machine Learning",
  "AAAI Conference on Artificial Intelligence",
  "Expert systems with applications",
  "IEEE Transactions on Neural Networks and Learning Systems",
  "IEEE Transactions on Systems, Man and Cybernetics",
  "Neurocomputing",
  "International Joint Conference on Artificial Intelligence",
  "Applied Soft Computing",
  "Knowledge-Based Systems",
  "Neural Computing and Applications",
  "IEEE transactions on fuzzy systems",
  "Journal of machine learning research",
  "Artificial Intelligence Review",
  "International Conference on Artificial Intelligence and Statistics",
  "Neural Networks",
  "Engineering Applications of Artificial Intelligence",
  "Applied Intelligence",
  "Conference on Robot Learning"
  )

excluded_journals <- setdiff(target_journals2, target_journals)

excluded_papers <- all_papers %>%
  filter(Publication_Venue_Name %in% excluded_journals) %>%
  filter(Year > 1983)

excluded_summary <- excluded_papers %>%
  group_by(Publication_Venue_Name) %>%
  summarize(Number_of_Papers = n(), .groups = 'drop') %>%
  left_join(
    all_papers %>%
      filter(Year < 2000) %>%
      group_by(Publication_Venue_Name) %>%
      summarize(Number_of_Papers_Before_2000 = n(), .groups = 'drop'),
    by = c("Publication_Venue_Name")
  ) %>%
  replace_na(list(Number_of_Papers_Before_2000 = 0))

head(excluded_summary)





# Combine the data for technical journals and arXiv papers
technical_papers <- all_papers %>%
  filter(Publication_Venue_Name %in% target_journals) 

View(target_data)

# Average stats by year for technical journals
avg_stats_by_year_tech <- technical_papers %>%
  group_by(Year) %>%
  summarise(
    Average_References = mean(References, na.rm = TRUE),
    Average_Coauthors = mean(Number_of_Authors, na.rm = TRUE),
    Total_Papers = n()
  ) %>%
  mutate(Source = "Technical Journals")

View(avg_stats_by_year_tech)
sum(avg_stats_by_year_tech$Total_Papers)


arXiv_papers <- all_papers %>%
  filter(Publication_Venue_Name == "arXiv.org") %>%
  filter(Citations > 15)

# Average stats by year for arXiv papers
avg_stats_by_year_arxiv <- arXiv_papers %>%
  group_by(Year) %>%
  summarise(
    Average_References = mean(References, na.rm = TRUE),
    Average_Coauthors = mean(Number_of_Authors, na.rm = TRUE),
    Total_Papers = n()
  ) %>%
  mutate(Source = "arXiv papers (>15 citations)")

# Combine the two datasets
combined_stats_by_year1 <- bind_rows(avg_stats_by_year_tech, avg_stats_by_year_arxiv)

View(combined_stats_by_year1)


# Plot for Total Papers by Year
plot_total_papers <- ggplot(combined_stats_by_year1, aes(x = Year, color = Source)) +
  geom_line(aes(y = Total_Papers)) +
  geom_vline(xintercept = 2013, linetype = "dashed", color = "black", alpha = 0.5) +  # Add vertical line at Year 2012
  geom_vline(xintercept = 1983, linetype = "dashed", color = "black", alpha = 0.5) +  # Add vertical line at Year 2012
  labs(
    title = "Data Used",
    x = "Year",
    y = "Number of Papers",
    color = "Source"
  ) +
  theme_minimal()

print(plot_total_papers)

# Not many observations in 1980, 1981, 1982 for technical journals
avg_stats_by_year_tech <- avg_stats_by_year_tech %>%
  filter(Year > 1983)

sum(avg_stats_by_year_tech$Total_Papers)

# Not many observations before 2012 for ArXiv posts
avg_stats_by_year_arxiv_later <- avg_stats_by_year_arxiv %>%
  filter(Year > 2012)

sum(avg_stats_by_year_arxiv_later$Total_Papers)

# Combine the two datasets
combined_stats_by_year <- bind_rows(avg_stats_by_year_tech, avg_stats_by_year_arxiv_later)

# Plot for Average References by Year
plot_avg_references <- ggplot(combined_stats_by_year, aes(x = Year, color = Source)) +
  geom_line(aes(y = Average_References)) + 
  geom_smooth(aes(y = Average_References, group = Source), se = FALSE, color = "black", method = "lm", size=0.3) +  # This will set the color for the line
  labs(
    title = "Average Number of References by Year for AI Papers",
    x = "Year",
    y = "Average Number of References",
    color = "Source"
  ) +
  theme_minimal()

# Print the plot
print(plot_avg_references)


# Plot for Average Co-authors by Year
plot_avg_coauthors <- ggplot(combined_stats_by_year, aes(x = Year, color = Source)) +
  geom_line(aes(y = Average_Coauthors)) +
  geom_smooth(aes(y = Average_Coauthors, group = Source), se = FALSE, color = "black", method = "lm", size=0.3) +  # This will set the color for the line
  
  labs(
    title = "Average Number of Co-authors by Year for AI Papers",
    x = "Year",
    y = "Average Number of Co-authors",
    color = "Source"
  ) +
  theme_minimal()

# Display the plot
print(plot_avg_coauthors)