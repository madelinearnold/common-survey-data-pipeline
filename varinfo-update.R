# Source the boilerplate message silencer
source("R/silence-boilerplate-messages.R")


# Load necessary libraries
library(tidyverse)
library(fuzzyjoin)


# User-defined variables (adjust as needed)
survey_year <- "2025"
survey_name <- "experience" 
join_column_text <- "QuestionText.2023"  # question text column from existing varinfo used for join 
join_column_var <- "ITEM_NAME" # "variable column from existing varinfo used for join
max_string_distance <- 6 # fuzzy matching leniency

# Define file paths (adjust as needed)

# data files
response_file <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/temp/UCGSES- Berkeley 2025_May 8, 2025_13.53.csv" # Raw Qualtrics response file - question text should be in the first row
prior_varinfo_file <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/2021-2024_experience_cumulative_varinfo_20240919.csv"

# intermediate working files
needs_manual_update_file <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/temp/2021-2025_experience_cumulative_varinfo_manual-update-needed_20250512.csv"  # File to be manually updated
unmatched_vars_file <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/temp/2025_experience_unmatched_variables_20250512.csv"  # File to use as reference while updating
manually_updated_file <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/temp/2021-2025_experience_cumulative_varinfo_manually-updated_20250508.csv"  # Rename file to this after updating

# output files
path_to_varinfo_folder <- "/Volumes/Secure/surveys/graduate-experience/data/variable-info/" # update as needed for survey
output_file <- paste0(path_to_varinfo_folder, survey_name, "_cumulative-varinfo_updated-", survey_year, ".csv")
trimmed_output_file <- paste0(path_to_varinfo_folder, survey_name, "_cumulative-varinfo_updated-", survey_year, "_2-trimmed-for-dashboard.csv")



# Data processing functions

# Function creating column names based on survey year
generate_dynamic_columns <- function(survey_year) {
  return(list(
    qualtrics_col_name = paste0("QualtricsVariableName.", survey_year),
    question_text_col_name = paste0("QuestionText.", survey_year),
    survey_admin_year_col_name = paste0("SurveyAdminYear.", survey_year)
  ))
}

# Define dynamic column name objects
dynamic_columns <- generate_dynamic_columns(survey_year)
qualtrics_col_name <- dynamic_columns$qualtrics_col_name
question_text_col_name <- dynamic_columns$question_text_col_name
survey_admin_year_col_name <- dynamic_columns$survey_admin_year_col_name

# Create data frame with response file column info (variables and question text)
load_survey_column_info <- function(response_file, qualtrics_col_name, question_text_col_name, survey_admin_year_col_name) {
  responses <- read_csv(response_file)
  
  column_info <- tibble(
    !!qualtrics_col_name := colnames(responses),  # Dynamic column name for QualtricsVariableName
    !!question_text_col_name := as.character(responses |> slice(1)),  # Dynamic column name for QuestionText
    !!survey_admin_year_col_name := survey_year  # Dynamic column name for SurveyAdminYear
  ) |>
    bind_rows(tibble(!!question_text_col_name := "")) # add empty string to text matching column for fuzzy matching to use
  
  return(column_info)
}

# merge columns when joining data frames, adapted from https://alistaire.rbind.io/blog/coalescing-joins/
coalesce_left_join <- function(x, y, 
                               by = NULL, suffix = c(".x", ".y"), 
                               join = dplyr::left_join, ...) {
  joined <- join(x, y, by = by, suffix = suffix, ...)
  # names of desired output
  cols <- union(names(x), names(y))
  
  to_coalesce <- names(joined)[!names(joined) %in% cols]
  suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
  # remove suffixes and deduplicate
  to_coalesce <- unique(substr(
    to_coalesce, 
    1, 
    nchar(to_coalesce) - nchar(suffix_used)
  ))
  
  coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
    joined[[paste0(.x, suffix[1])]], 
    joined[[paste0(.x, suffix[2])]]
  ))
  names(coalesced) <- to_coalesce
  
  dplyr::bind_cols(joined, coalesced)[cols]
}

# Join latest survey vars with prior varinfo, first by fuzzy match on text then match by variable
join_varinfo <- function(prior_varinfo, column_info, join_column_text, join_column_var, qualtrics_col_name, question_text_col_name, max_string_distance = 3) {
  
  # join data sets using question text fuzzy matching
  fuzzy_joined <- prior_varinfo %>%
    rowid_to_column(var = "row_id") %>%
    mutate(!!join_column_text := replace_na(!!sym(join_column_text), "")) %>%
    stringdist_left_join(
      column_info,
      by = join_by(!!sym(join_column_text) == !!sym(question_text_col_name)),
      method       = "lv",
      max_dist     = max_string_distance,
      distance_col = "string_distance"
    ) %>%
    arrange(row_id, string_distance) %>%
    distinct(row_id, .keep_all = TRUE)
  
  # summarize fuzzy matches
  total_rows <- nrow(column_info)
  num_exact            <- sum(fuzzy_joined$string_distance == 0 & !is.na(fuzzy_joined[qualtrics_col_name]), na.rm = TRUE)
  num_fuzzy            <- sum(!is.na(fuzzy_joined$string_distance) & fuzzy_joined$string_distance > 0 & !is.na(fuzzy_joined[qualtrics_col_name]) , na.rm = TRUE)
  num_unmatched_text   <- total_rows - num_exact - num_fuzzy
  
  message(sprintf(
    "🔍 Fuzzy text join (max distance = %d): %d exact, %d fuzzy, %d unmatched (out of %d).",
    max_string_distance, num_exact, num_fuzzy, num_unmatched_text, total_rows
  ))
  
  # additional join using variable names
  joined <- fuzzy_joined %>%
    coalesce_left_join(
      column_info,
      join_by(!!sym(join_column_var) == !!sym(qualtrics_col_name)),
      keep = TRUE
    )
  
  # summarize final matches
  num_total_matched <- sum(!is.na(joined[[qualtrics_col_name]]))
  num_unmatched     <- total_rows - num_total_matched
  num_additional_matches <- num_unmatched_text - num_unmatched
  
  message(sprintf(
    "🔍 After variable-name join: %d additional matches, %d total matched, %d still unmatched (out of %d).",
    num_additional_matches, num_total_matched, num_unmatched, total_rows
  ))
  
  return(joined)
}

# Return the unmatched variables from the column info
get_unmatched <- function(joined, column_info, qualtrics_col_name) {
    unmatched <- anti_join(column_info, joined, by = qualtrics_col_name)
    return(unmatched)
  }

# Export the data for manual updates
export_manual_update_files <- function(joined_varinfo, unmatched_vars) {
  # CSV that requires manual updates
  write_csv(joined_varinfo, needs_manual_update_file)
  # CSV with unmatched variables
  write_csv(unmatched_vars, unmatched_vars_file)
  cat("✅ Exported joined data for manual updates. Please update the file and 💾 save to the *manually_updated_file* path you specified at the top of the script. Re-run the script once the updates are done to continue processing.\n Also exported unmatched variable data for reference.\n")
}

# sort the varinfo file according to digits in SurveyAdmin* columns or user supplied list of columns in recency order
sort_varinfo <- function(updated_varinfo, survey_admin_cols_in_recency_order = NULL) {
  # define recency columns
  if (!is.null(survey_admin_cols_in_recency_order)) {
    # if user supplies columns in order, use that
    recency_cols <- survey_admin_cols_in_recency_order
  } else {
    # find all SurveyAdmin* columns
    recency_cols <- grep("^SurveyAdmin", names(updated_varinfo), value = TRUE)
    # extract all digits, convert to integer (e.g. "2023-1" becomes 20231)
    digit_lists <- str_extract_all(recency_cols, "\\d+")
    digit_keys  <- sapply(digit_lists, paste0, collapse = "")
    col_nums    <- as.integer(digit_keys)
    # order columns descending so "newest" by numeric key comes first
    recency_cols <- recency_cols[order(col_nums, decreasing = TRUE)]}
  # pull the single value for each recency column
  recency_values <- sapply(recency_cols, function(col) {
    # grab that entire column vector
    vals <- updated_varinfo[[col]]
    # drop blanks and NAs
    uv   <- unique(vals[!is.na(vals) & vals != ""])
    if (length(uv) == 0) {
      NA_character_
    } else if (length(uv) > 1) {
      warning("Column ", col, " has >1 distinct non-NA values; using the first")
      uv[1]
    } else {
      uv
    }
  }, USE.NAMES = FALSE )
  # assign rank to values
  recency_rank <- setNames(seq_along(recency_values), recency_values)
  message("Recency ranking:\n",
          paste0(names(recency_rank), " → ", recency_rank, collapse = "\n"))
  # generate most recent column
  result <- updated_varinfo |>
    rowid_to_column("row_id") |>
    rowwise() |>
    mutate(
      most_recent = {
        # inline pull the row's SurveyAdmin* values
        v <- c_across(all_of(recency_cols))
        v <- v[!is.na(v) & v != ""] # drop NA/empty
        if (length(v) == 0) {
          NA_character_
        } else {
          valid <- intersect(v, names(recency_rank))
          if (length(valid) == 0) {
            NA_character_
          } else {
            list(valid)
            # pick the value earliest in your recency_order
            valid[which.min(recency_rank[valid])]
          }
        }
      },
      # create priority - admin/metadata first, otherwise ranked according to recency
      priority = if_else(
        ITEM_TYPE %in% c("administrative", "metadata"),-1L,
        recency_rank[most_recent]
      )
    ) |>
    ungroup() |>
    # Sort by priority
    arrange(priority, row_id) |>
    select(-priority, -row_id)
  return(result)
}

# Function to generate clean version for dashboard
generate_dashboard_data <- function(sorted_varinfo) {
    varinfo_clean <- 
    sorted_varinfo |>
    filter(!ITEM_TYPE %in% c("administrative", "metadata"),
           ITEM_NAME != "CONSENT") |>
    select(ITEM_NAME, ITEM_SECTION, ITEM_STEM, ITEM_MEMBER, SCALE_OPTIONS, ITEM_TYPE, most_recent, ITEM_PARENT_ID)
  write_csv(varinfo_clean, trimmed_output_file)
}

# Main script

# Step 1: Load & process data
column_info <- load_survey_column_info(response_file, qualtrics_col_name, question_text_col_name, survey_admin_year_col_name)
prior_varinfo <- read_csv(prior_varinfo_file)
joined_varinfo <- join_varinfo(prior_varinfo, column_info, join_column_text, join_column_var, qualtrics_col_name, question_text_col_name, max_string_distance)
unmatched_vars <- get_unmatched(joined_varinfo, column_info, qualtrics_col_name)

# Step 2: Export joined data for manual updates. Export unmatched variables for reference.
export_manual_update_files(joined_varinfo, unmatched_vars)

# Step 3: After the manual updates, read the updated file
updated_varinfo <- read_csv(manually_updated_file)

# Step 4: Sort the updated varinfo by most recent questions
new_cumulative_varinfo <- sort_varinfo(updated_varinfo)

# Step 5: Save the new cumulative varinfo as csv
write_csv(new_cumulative_varinfo, output_file)

# Step 6: Generate dashboard data and save to csv
generate_dashboard_data(new_cumulative_varinfo)

# Print completion message
cat("✅ Varinfo update complete. File saved to:", output_file, "\n")
