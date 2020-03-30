source("code/table_function.R")
# source("code/preliminary_weighted_analysis.R")
# Run the first 28 lines from the file above to get hte data, questionnaire, etc.
library(tidyverse)

# Correcting blank cells to NA

# filtering data for what we want to analyze
# leave select multiple text columns in

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name

data_to_analyze <- data %>%
  select(-one_of(text)) %>%
  select(-start, -end, -today, -audit, -device_id, -mantika, -enumerator_id, -organisation_name, -enumerator_name,
         -baladiya, -baladiya_label, -consent, -hoh, -`_index`, -`_uuid`, -`_submission_time`, -`_id`,
         -`_validation_status`, -`_geolocation_longitude`, -`_geolocation_latitude`, -`_geolocation_altitude`,
         -`_geolocation_precision`, -geolocation) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))




data_to_analyze <- data_to_analyze %>% mutate(
  working_status= ifelse(type_jobs.didnt_work==1, "did_not_work","is_working")
)
strata_output <- table_maker(data_to_analyze, 
                            questions, 
                            choices,
                            weighting_function = NULL, 
                            labels = T, 
                            language = "english", 
                            "Libya",
                            "region_type_jobs")

saveRDS(strata_output, "output/time_arrival_categories.RDS")
write_csv(strata_output, "../final_analysis/region_type_jobs.csv", na = "")
