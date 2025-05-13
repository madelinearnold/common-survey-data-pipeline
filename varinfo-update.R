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

# Join latest survey vars with prior varinfo, first by fuzzy match on text then match by variable
join_varinfo <- function(prior_varinfo, column_info, join_column_text, join_column_var, qualtrics_col_name, question_text_col_name, max_string_distance = 3) {

  joined <- 
    prior_varinfo |>
    # Create a row_id for filtering after the join
    rowid_to_column(var = "row_id")  |> 
    # Replace NAs in the varinfo data frame with an empty string placeholder
    mutate(!!join_column_text := replace_na(!!sym(join_column_text), "")) |>
    # perform join with fuzzy matching of text
    stringdist_left_join(column_info, by = join_by(!!join_column_text == !!question_text_col_name), method = "lv", max_dist = max_string_distance, distance_col = "string_distance") |>
    # keep only the rows with the best match/lowest distance (e.g. keep perfect over fuzzy matches)
    arrange(row_id, string_distance) |>
    distinct(row_id, .keep_all = TRUE) |>
    # coalesce join using variable names
    coalesce_left_join(column_info, join_by(!!join_column_var == !!qualtrics_col_name), keep = TRUE) 
  
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
  cat("Exported joined data for manual updates. Please update the file and save to the *manually_updated_file* path you specified at the top of the script. Re-run the script once the updates are done to continue processing.\n Also exported unmatched variable data for reference.\n")
}

# Read and process the manually updated varinfo
sort_varinfo <- function(updated_varinfo) {
  sorted_varinfo <-
  updated_varinfo |>
    rowid_to_column(var = "row_id")  |> # Create a row_id for sorting
    # calculate most recent year
    mutate(across(starts_with("SurveyAdminYear"), ~ as.numeric(.x))) |>
    rowwise() %>%
    mutate(
      # Calculate the maximum, handling case where all columns are NA
      most_recent_year = {
        # Get the values for the current row
        vals <- c_across(starts_with("SurveyAdminYear"))
        # Check if all values are NA
        if(all(is.na(vals))) {
          NA_real_ # Assign NA (use NA_real_ for numeric context)
        } else {
          max(vals, na.rm = TRUE) # Calculate max if there's at least one non-NA
        }
      }
    ) |>
    ungroup() |>
    mutate(priority = case_when(
      ITEM_TYPE %in% c("administrative", "metadata") ~ -1, # metadata variables at the top of the list
      .default = 2025 - most_recent_year)) |> # otherwise priority to most recently asked variables
    arrange(priority, row_id) |> # sort
    select(-any_of(c("priority", "row_id", "string_distance"))) # remove unnecessary columns
  return(sorted_varinfo)
}

# Function to generate clean version for dashboard
generate_dashboard_data <- function(sorted_varinfo) {
    varinfo_clean <- 
    sorted_varinfo |>
    filter(!ITEM_TYPE %in% c("administrative", "metadata"),
           ITEM_NAME != "CONSENT") |>
    select(ITEM_NAME, ITEM_SECTION, ITEM_STEM, ITEM_MEMBER, SCALE_OPTIONS, ITEM_TYPE, most_recent_year, ITEM_PARENT_ID)
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
cat("Varinfo update complete. File saved to:", output_file, "\n")
