# Common survey data pipeline

Code for processing survey data, written by the UC Berkeley Graduate Division Institutional Research Team.

## A reusable pipeline for updating a survey variable code book ("varinfo" file) 

This script joins the variable and text information from the Qualtrics response data onto the existing variable information file, first using fuzzy match on question text, then using variable names. There will likely be data that isn't matched, so the script creates an output file of the joined data for manual checking and updating, along with a file of unmatched variables from the Qualtrics data set. The user then manually reviews and updates, and the script further processes the data to calculate most recent year and sort by most recently used variables.

### Overview

Use the step-by-step `varinfo-update.R` script to:

-   Load variable information from a raw Qualtrics CSV (with question text in row 1)

-   Load a previous variable information code book

-   Fuzzy match on question text, then exact match on variable name to join the two data sets

-   Output CSVs to assist in manually updating any unmatched variables

-   Sort your updated codebook by recency (more recently asked questions come first)

-   Save complete and trimmed-down versions of the code book

### Prerequisites

-   R (≥ 4.0)

-   R packages: `tidyverse`, `fuzzyjoin`

-   Git & GitHub (if you’re cloning the repo)

### Using this code

Clone this Github repository, or download the `varinfo-update.R` script.

Open and edit the top of the script. Edit your survey's name, year, the columns you want to use for matching, and file paths for reading and saving CSVs.

Then run `Rscript varinfo-update.R` from the command line.

-   The script will load your files and export a file for you to manually check and update

-   After updating, re-run the script to generate the final sorted varinfo file

### Example

See the `example/` folder for example input files. You can use these as templates, or update the file paths to point to these files to test out the script.

### Input files:

1.  existing varinfo file - with columns for variable name, question text, and optionally SurveyAdminYear

2.  raw Qualtrics response file - with variable names as column names and question text in the first row.

### Output files:

1.  First pass CSV requiring manual update — review and edit question texts or mappings.

    -   Use the unmatched variables CSV to see which variables from the raw Qualtrics file didn't match, so you can update manually or adjust the max_string_distance for more lenient text fuzzy matching.

2.  Final updated varinfo file

3.  Trimmed down version of varinfo file for sharing (doesn't include all the columns with question text from different years)

### Troubleshooting

-   Fuzzy matches too few questions → increase max_string_distance

-   All NA in most_recent →

    -   If you have columns beginning with SurveyAdmin\*, ensure they contain digits to indicate the administration (e.g. SurveyAdminYear2025, SurveyAdminTerm25-2) and matching the regex \\\\d+.

    -   If you don't have SurveyAdmin\* columns, specify the columns you want to use to generate the most_recent column with most recent first (e.g. `survey_admin_cols_in_recency_order = c("SurveyAdminTermSp2025","SurveyAdminTermSp2024", "SurveyAdminTermFa2023", "SurveyAdminTermSu2023", "SurveyAdminTermSp2023")`
