# common-survey-data-pipeline

Code for processing survey data, written by the UC Berkeley Graduate Division Institutional Research Team.

### Update survey variable info using `varinfo-update.R` script 

Update a survey variable information (varinfo) file with new variable information from a survey administration.

Input files include:

1.  existing varinfo file - with columns for variable name, question text, and optionally SurveyAdminYear

2.  raw Qualtrics response file - with variable names as column names and question text in the first row

Output files include:

1.  Updated varinfo file

2.  Trimmed down version of varinfo file for sharing (doesn't include all the columns with question text from different years)

The script joins the variable and text information from the Qualtrics response data onto the existing variable information file, first using fuzzy match on question text, then using variable names. There will likely be data that isn't matched, so the script creates an output file of the joined data for manual checking and updating, along with a file of unmatched variables from the Qualtrics data set. The user then manually reviews and updates, and the script further processes the data to calculate most recent year and sort by most recently used variables.
