# Setting up --------------------------------------------------------------

library(tidyverse)
library(vroom)
library(janitor)
library(googlesheets4)
library(lubridate)
library(beepr)

# Download the file -------------------------------------------------------


# Authenticate for the First Time: If this is your first time using googlesheets4, you need to allow R to access your Google account.
gs4_auth()

# Set a temporary file to store our zip
temp <- tempfile()

download.file('https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip',
              temp)

house_comp <- c('NC HOUSE-005', 'NC HOUSE-007', 'NC HOUSE-009', 'NC HOUSE-012', 'NC HOUSE-020', 'NC HOUSE-024', 'NC HOUSE-025', 'NC HOUSE-032', 
                'NC HOUSE-035', 'NC HOUSE-036', 'NC HOUSE-037', 'NC HOUSE-047', 'NC HOUSE-048', 'NC HOUSE-054', 'NC HOUSE-062', 'NC HOUSE-063', 
                'NC HOUSE-064', 'NC HOUSE-073', 'NC HOUSE-098', 'NC HOUSE-105', 'NC HOUSE-115')

senate_comp <- c('NC SENATE-001', 'NC SENATE-002', 'NC SENATE-004', 'NC SENATE-007', 'NC SENATE-011', 'NC SENATE-013', 
                 'NC SENATE-018', 'NC SENATE-024', 'NC SENATE-042')

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
  mutate(precincts_pct = round(precincts_reporting/precincts_total * 100, 1), .after = precincts_total) %>% 
  mutate(rep_pct = round(rep/total * 100, 1), dem_pct = round(dem/total * 100, 1), .after = total) %>%
  mutate(contest_name = str_replace(contest_name, 'NC HOUSE OF REPRESENTATIVES DISTRICT ', 'NC HOUSE-')) %>%
  mutate(contest_name = str_replace(contest_name, 'NC STATE SENATE DISTRICT ', 'NC SENATE-')) %>% 
  mutate(status = ifelse(contest_name %in% house_comp | contest_name %in% senate_comp, 'competitive', NA), .after = contest_name) %>%
  mutate(status = ifelse(is.na(rep) | is.na(dem), 'unopposed', status)) %>% 
  select(-lib, -gre, -other) %>%
  filter(!(is.na(rep) & is.na(dem))) 

#view(leg_breakdown)

# Escribir a Google Sheets
write_sheet(
  data = leg_breakdown,  # El objeto data.frame correcto
  ss = '1qDpAXTvQEodZsaxEBM5O0_ktXQovhEXZkp-tbS1z3R0',  # ID de la hoja de cálculo
  sheet = "main_results"
)

# Write into a function ---------------------------------------------------

get_results <- function(house_comp, senate_comp, sleep_time){
  # Start counting
  start <- Sys.time()
  
  # Set a temporary file to store our zip
  temp <- tempfile()
  
  # Download the file
  tryCatch(
    {
      download.file('https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/2024_11_05/results_pct_20241105.zip', temp, quiet = TRUE)
      cat('>>> FILE DOWNLOADED AT', as.character(start), '\n')
    },
    error = function(e){
      beep('wilhelm')
      message('Error downloading the data: ', e)
      cat('>>> SLEEPING FOR 10 SECONDS AT', as.character(Sys.time()), '\n')
      Sys.sleep(10)
    }
  )
  
  tryCatch(
    {
      # Separate the steps
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
        mutate(precincts_pct = round(precincts_reporting/precincts_total * 100, 1), .after = precincts_total) %>% 
        mutate(rep_pct = round(rep/total * 100, 1), dem_pct = round(dem/total * 100, 1), .after = total) %>%
        mutate(contest_name = str_replace(contest_name, 'NC HOUSE OF REPRESENTATIVES DISTRICT ', 'NC HOUSE-')) %>%
        mutate(contest_name = str_replace(contest_name, 'NC STATE SENATE DISTRICT ', 'NC SENATE-')) %>% 
        mutate(status = ifelse(contest_name %in% house_comp | contest_name %in% senate_comp, 'competitive', NA), .after = contest_name) %>%
        mutate(status = ifelse(is.na(rep) | is.na(dem), 'unopposed', status)) %>% 
        select(-lib, -gre, -other) %>%
        filter(!(is.na(rep) & is.na(dem)))
      
      #view(leg_breakdown)
      
      write_sheet(
        data = leg_breakdown,  # El objeto data.frame correcto
        ss = '1qDpAXTvQEodZsaxEBM5O0_ktXQovhEXZkp-tbS1z3R0',  # ID de la hoja de cálculo
        sheet = "main_results"
      )
      
      total_time <- lubridate::seconds_to_period(as.numeric(Sys.time()) - as.numeric(start))
      cat('>>> WRITE FINISHED AT', as.character(Sys.time()), 'AFTER', as.character(total_time), '\n')
      
      cat('>>> SLEEPING FOR', sleep_time, 'SECONDS AT', as.character(Sys.time()), '\n')
      Sys.sleep(sleep_time)
    },
    error = function(e){
      beep()
      message('Error processing the data: ', e)
      cat('>>> SLEEPING FOR 10 SECONDS AT', as.character(Sys.time()), '\n')
      Sys.sleep(10)
    }
  )
}

# Execute
while(TRUE){
  get_results(house_comp, senate_comp, 120)
}
