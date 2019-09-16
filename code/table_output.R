source("code/table_function.R")
# source("code/preliminary_weighted_analysis.R")
# Run the first 28 lines from the file above to get hte data, questionnaire, etc.
library(tidyverse)

# Correcting blank cells to NA

data <- mutate_if(data, is.character, na_if, "")

# filtering data for what we want to analyze
# leave select multiple text columns in

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)

data_to_analyze <- data %>%
  select(-one_of(text)) %>%
  select(-start, -end, -today, -audit, -device_id, -mantika, -enumerator_id, -organisation_name, -enumerator_name,
         -baladiya, -baladiya_label, -consent, -hoh, -`_index`, -`_uuid`, -`_submission_time`, -`_id`,
         -`_validation_status`, -`_geolocation_longitude`, -`_geolocation_latitude`, -`_geolocation_altitude`,
         -`_geolocation_precision`, -geolocation) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))

strata_output <- table_maker(data_to_analyze, 
                            questions, 
                            choices,
                            weighting_function = NULL, 
                            labels = T, 
                            language = "english", 
                            "Libya", 
                            "region_of_origin")

saveRDS(strata_output, "output/overall_region_origin.RDS")
write_csv(strata_output, "output/overall_region_origin.csv", na = "")
