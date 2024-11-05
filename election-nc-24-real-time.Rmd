# Election Results Analysis Script Documentation

## Overview
This script automates the download, processing, and uploading of election results data to Google Sheets. It highlights competitive and unopposed races in the North Carolina House and Senate.

## Libraries Used
- **`tidyverse`**: Data manipulation.
- **`vroom`**: Quick reading of large files.
- **`janitor`**: Data cleaning.
- **`googlesheets4`**: Google Sheets integration.
- **`lubridate`**: Date-time handling.
- **`beepr`**: Notification sounds.

## Code Explanation

### Initial Setup
#Load libraries required for data processing and Google Sheets interaction.
```{r}
library(tidyverse)
library(vroom)
library(janitor)
library(googlesheets4)
library(lubridate)
library(beepr)
```

# Authenticate for the First Time: If this is your first time using googlesheets4, you need to allow R to access your Google account.
```{r}
gs4_auth()
```

### Data Download and Temporary Storage
#Define a temporary file path and download the election results data from the specified URL.
```{r}
temp <- tempfile()
download.file('https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip', temp)
```


### Define Competitive Races
#Specify which NC House and Senate districts are considered competitive.
```{r}
house_comp <- c('NC HOUSE-005', 'NC HOUSE-007', 'NC HOUSE-009', 'NC HOUSE-012', 'NC HOUSE-020', 'NC HOUSE-024', 
                'NC HOUSE-025', 'NC HOUSE-032', 'NC HOUSE-035', 'NC HOUSE-036', 'NC HOUSE-037', 'NC HOUSE-047', 
                'NC HOUSE-048', 'NC HOUSE-054', 'NC HOUSE-062', 'NC HOUSE-063', 'NC HOUSE-064', 'NC HOUSE-073', 
                'NC HOUSE-098', 'NC HOUSE-105', 'NC HOUSE-115')

senate_comp <- c('NC SENATE-001', 'NC SENATE-002', 'NC SENATE-004', 'NC SENATE-007', 'NC SENATE-011', 'NC SENATE-013', 
                 'NC SENATE-018', 'NC SENATE-024', 'NC SENATE-042')
```

### Data Processing
#Read and clean the data, filter relevant contests, calculate vote totals and percentages, and identify competitive and unopposed races.
# Detailed Code Explanation

### Step-by-Step Breakdown

1. **Reading Data**: 
   The `vroom()` function reads the file into a data frame using tab delimiters, enabling fast processing of large data sets.

2. **Cleaning Column Names**:
   The `clean_names()` function standardizes column names to snake_case format, making them easier to work with.

3. **Filtering Contests**:
   The `filter()` function retains only rows where `contest_name` contains either "NC HOUSE" or "NC STATE SENATE", focusing on relevant election data.

4. **Adding Reporting Status**:
   `mutate()` is used to create a `reporting` column that flags precincts that have reported votes. This column contains 'Y' if `total_votes` is greater than 0 and 'N' otherwise.

5. **Summarizing Data**:
   `group_by()` and `summarize()` functions are applied to aggregate total votes and count how many precincts have reported for each contest and party.

6. **Pivoting Data**:
   The `pivot_wider()` function reshapes the data so that different party names become column headers, creating a cleaner view of the data.

7. **Calculating Totals and Percentages**:
   `mutate()` is used to compute the total votes, percentages for Republican (`rep_pct`), and Democrat (`dem_pct`) votes. It also calculates the percentage of precincts reporting (`precincts_pct`).

8. **Renaming Columns**:
   `rename_with()` changes all column names to lowercase for consistency, and `rename()` is used to replace the column for other votes (`NA`) with `other`.

9. **Standardizing Names**:
   `str_replace()` standardizes the `contest_name` format by replacing full contest names (e.g., "NC HOUSE OF REPRESENTATIVES DISTRICT") with shorter, more readable forms (e.g., "NC HOUSE-").

10. **Identifying Status**:
    The `mutate()` function adds a `status` column to classify each race as either `competitive` or `unopposed`. 
    - The code first checks if the `contest_name` is in the `house_comp` or `senate_comp` lists, marking these races as `competitive`.
    - If the race does not meet the previous condition, the next check determines if any party's vote count (e.g., `rep` or `dem`) is `NA` (missing), indicating that the race is `unopposed`. 
    - This step ensures that races with only one major party running are identified clearly.

11. **Selecting Columns**:
    The `select()` function removes unnecessary columns such as `lib`, `gre`, and `other`, focusing only on major party results and essential data.

12. **Filtering Empty Rows**:
    The final `filter()` step ensures that only rows with valid data (i.e., those that have non-`NA` values for `rep` or `dem`) remain in the final dataset.

```{r}
leg_breakdown <- vroom(temp, delim = '\t') %>%
  clean_names() %>%
  filter(str_detect(contest_name, 'NC HOUSE|NC STATE SENATE')) %>%
  mutate(reporting = if_else(total_votes > 0, 'Y', 'N')) %>%
  group_by(contest_name, choice_party) %>%
  summarize(votes = sum(total_votes),
            precincts_reporting = sum(reporting == 'Y'),
            precincts_total = sum(real_precinct == 'Y'), .groups = 'keep') %>%
  pivot_wider(names_from = choice_party, values_from = votes) %>%
  mutate(total = sum(across(REP:`NA`), na.rm = TRUE)) %>%
  rename_with(tolower) %>%
  rename(other = 'na') %>%
  mutate(precincts_pct = round(precincts_reporting / precincts_total * 100, 1), .after = precincts_total) %>%
  mutate(rep_pct = round(rep / total * 100, 1), dem_pct = round(dem / total * 100, 1), .after = total) %>%
  mutate(contest_name = str_replace(contest_name, 'NC HOUSE OF REPRESENTATIVES DISTRICT ', 'NC HOUSE-')) %>%
  mutate(contest_name = str_replace(contest_name, 'NC STATE SENATE DISTRICT ', 'NC SENATE-')) %>%
  mutate(status = ifelse(contest_name %in% house_comp | contest_name %in% senate_comp, 'competitive', NA), .after = contest_name) %>%
  mutate(status = ifelse(is.na(rep) | is.na(dem), 'unopposed', status)) %>%
  select(-lib, -gre, -other) %>%
  filter(!(is.na(rep) & is.na(dem)))
```

### View and Write Results
#View the processed data and write it to Google Sheets.
```{r}
#view(leg_breakdown)

write_sheet(
  data = leg_breakdown,
  ss = '1qDpAXTvQEodZsaxEBM5O0_ktXQovhEXZkp-tbS1z3R0',
  sheet = "main_results"
)
```

### Function for Repeated Execution
#Create a function to automate the process with error handling and time intervals.
```{r}
get_results <- function(house_comp, senate_comp, sleep_time) {
  start <- Sys.time()
  temp <- tempfile()

  tryCatch({
    download.file('https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip', temp, quiet = TRUE)
    cat('>>> FILE DOWNLOADED AT', as.character(start), '\n')
  }, error = function(e) {
    beep('wilhelm')
    message('Error downloading the data: ', e)
    cat('>>> SLEEPING FOR 10 SECONDS AT', as.character(Sys.time()), '\n')
    Sys.sleep(10)
  })

  tryCatch({
    results <- vroom(temp, delim = '\t', show_col_types = FALSE)
    results_clean <- results %>%
      clean_names('snake')
      
    leg_breakdown <- results_clean %>%
      filter(str_detect(contest_name, 'NC HOUSE|NC STATE SENATE')) %>%
      mutate(reporting = if_else(election_day > 0, 'Y', 'N')) %>%
      group_by(contest_name, choice_party) %>%
      summarize(votes = sum(total_votes),
                precincts_reporting = sum(reporting == 'Y' & real_precinct == 'Y'),
                precincts_total = sum(real_precinct == 'Y'), .groups = 'keep') %>%
      pivot_wider(names_from = choice_party, values_from = votes) %>%
      mutate(total = sum(across(REP:`NA`), na.rm = TRUE)) %>%
      rename_with(tolower) %>%
      rename(other = 'na') %>%
      mutate(precincts_pct = round(precincts_reporting / precincts_total * 100, 1), .after = precincts_total) %>%
      mutate(rep_pct = round(rep / total * 100, 1), dem_pct = round(dem / total * 100, 1), .after = total) %>%
      mutate(contest_name = str_replace(contest_name, 'NC HOUSE OF REPRESENTATIVES DISTRICT ', 'NC HOUSE-')) %>%
      mutate(contest_name = str_replace(contest_name, 'NC STATE SENATE DISTRICT ', 'NC SENATE-')) %>%
      mutate(status = ifelse(contest_name %in% house_comp | contest_name %in% senate_comp, 'competitive', NA), .after = contest_name) %>%
      mutate(status = ifelse(is.na(rep) | is.na(dem), 'unopposed', status)) %>%
      select(-lib, -gre, -other) %>%
      filter(!(is.na(rep) & is.na(dem)))
    
    write_sheet(
      data = leg_breakdown,
      ss = '1qDpAXTvQEodZsaxEBM5O0_ktXQovhEXZkp-tbS1z3R0',
      sheet = "main_results"
    )
    
    total_time <- lubridate::seconds_to_period(as.numeric(Sys.time()) - as.numeric(start))
    cat('>>> WRITE FINISHED AT', as.character(Sys.time()), 'AFTER', as.character(total_time), '\n')
    cat('>>> SLEEPING FOR', sleep_time, 'SECONDS AT', as.character(Sys.time()), '\n')
    Sys.sleep(sleep_time)
  }, error = function(e) {
    beep()
    message('Error processing the data: ', e)
    cat('>>> SLEEPING FOR 10 SECONDS AT', as.character(Sys.time()), '\n')
    Sys.sleep(10)
  })
}
```

### Continuous Execution
#Run the function in an infinite loop to continuously process data at defined intervals.
```{r}
while(TRUE) {
  get_results(house_comp, senate_comp, 120)
}
```

## How to Run
1. Ensure all required libraries are installed.
2. Replace the Google Sheets ID in `write_sheet()` with your own.
3. Run the script in an R environment.
4. The script will automatically download, process, and write results every 120 seconds.

## Troubleshooting
- Ensure Google Sheets authentication (`gs4_auth()`) is set up correctly if running for the first time.
- Adjust `house_comp` and `senate_comp` as needed for different competitive districts.
- Check for any changes in the data format from the source URL to update processing steps accordingly.
