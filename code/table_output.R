source("code/table_function.R")
# source("code/preliminary_weighted_analysis.R")
# Run the first 28 lines from the file above to get hte data, questionnaire, etc.
library(tidyverse)

# Correcting blank cells to NA

# filtering data for what we want to analyze
# leave select multiple text columns in

text <- filter(questions, str_detect(type, "(\\btext\\b)|(\\bnote\\b)"))$name

data_to_analyze <- mydata %>%
  select(-one_of(text)) %>%
  select(-start, -end, -today, -audit, -device_id, -mantika, -enumerator_id, -organisation_name, -enumerator_name,
         -baladiya, -baladiya_label, -consent, -hoh, -`_index`, -`_uuid`, -`_submission_time`, -`_id`,
         -`_validation_status`, -`_geolocation_longitude`, -`_geolocation_latitude`, -`_geolocation_altitude`,
         -`_geolocation_precision`, -geolocation) %>%
  select_if(~ !(all(is.na(.x)) | all(. == "")))


data_to_analyze <- data_to_analyze %>% mutate(
  arabic_speaking= ifelse(languages.arabic==1, "yes","no")
)

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
                            "live_with_family")

saveRDS(strata_output, "output/live_with_family.RDS")
write_csv(strata_output, "output/live_with_family.csv", na = "")
