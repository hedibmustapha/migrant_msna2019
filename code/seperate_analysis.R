mydata %>% select(total_income_excluding_0,
                  total_expenditure_excluding_0,
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  mantika_label) %>% group_by(mantika_label) %>%
  summarise_all(list(~median(., na.rm = T))) %>% 
  write.csv("../final_analysis/income_expenditure_without0_mantika.csv")


mydata %>% select(total_income_excluding_0,
                  total_expenditure_excluding_0,
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  region_of_origin) %>% group_by(region_of_origin) %>%
  summarise_all(list(~median(., na.rm = T))) %>% 
  write.csv("../final_analysis/income_expenditure_without0_region_of_origin.csv")  


mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  mantika_label) %>% group_by(mantika_label) %>%
  summarise_all(list(~median(., na.rm = T))) %>% 
  write.csv("../final_analysis/type_income_without0_mantika.csv")

mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  region_of_origin) %>% group_by(region_of_origin) %>%
  summarise_all(list(~median(., na.rm = T))) %>% 
  write.csv("../final_analysis/type_income_without0_region_of_origin.csv")


mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  
                  food_expenditure_without_0,
                  rent_expenditure_without_0,
                  shelter_maintenance_expenditure_without_0,
                  water_expenditure_without_0,
                  nfi_expenditure_without_0,
                  utilities_expenditure_without_0,
                  fuel_expenditure_without_0,
                  health_related_expenditure_without_0,
                  education_related_expenditure_without_0,
                  transportation_expenditure_without_0,
                  mobile_phone_credit_expenditure_without_0,
                  productive_assets_expenditure_without_0,
                  debt_repayment_expenditure_without_0,
                  other_expenditure_without_0,
                  
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  total_income_excluding_0,
                  total_expenditure_excluding_0) %>% summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/overall_medians_excluding_0.csv")


mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  
                  food_expenditure_without_0,
                  rent_expenditure_without_0,
                  shelter_maintenance_expenditure_without_0,
                  water_expenditure_without_0,
                  nfi_expenditure_without_0,
                  utilities_expenditure_without_0,
                  fuel_expenditure_without_0,
                  health_related_expenditure_without_0,
                  education_related_expenditure_without_0,
                  transportation_expenditure_without_0,
                  mobile_phone_credit_expenditure_without_0,
                  productive_assets_expenditure_without_0,
                  debt_repayment_expenditure_without_0,
                  other_expenditure_without_0,
                  
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  total_income_excluding_0,
                  total_expenditure_excluding_0,
                  region_of_origin) %>% group_by(region_of_origin) %>%  summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/region_origin_medians_excluding_0.csv")

mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  
                  food_expenditure_without_0,
                  rent_expenditure_without_0,
                  shelter_maintenance_expenditure_without_0,
                  water_expenditure_without_0,
                  nfi_expenditure_without_0,
                  utilities_expenditure_without_0,
                  fuel_expenditure_without_0,
                  health_related_expenditure_without_0,
                  education_related_expenditure_without_0,
                  transportation_expenditure_without_0,
                  mobile_phone_credit_expenditure_without_0,
                  productive_assets_expenditure_without_0,
                  debt_repayment_expenditure_without_0,
                  other_expenditure_without_0,
                  
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  total_income_excluding_0,
                  total_expenditure_excluding_0,
                  time_of_arrival) %>% group_by(time_of_arrival) %>%  summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/time_arrival_medians_excluding_0.csv")

mydata %>% select(salaried_work_without_0,
                  casual_daily_labour_without_0,
                  income_own_business_without_0,
                  support_from_family_inlibya_without_0,
                  support_from_family_home_without_0,
                  support_from_family_europe_without_0,
                  humanitarian_assistance_without_0,
                  zakat_without_0,
                  income_other_without_0,
                  
                  food_expenditure_without_0,
                  rent_expenditure_without_0,
                  shelter_maintenance_expenditure_without_0,
                  water_expenditure_without_0,
                  nfi_expenditure_without_0,
                  utilities_expenditure_without_0,
                  fuel_expenditure_without_0,
                  health_related_expenditure_without_0,
                  education_related_expenditure_without_0,
                  transportation_expenditure_without_0,
                  mobile_phone_credit_expenditure_without_0,
                  productive_assets_expenditure_without_0,
                  debt_repayment_expenditure_without_0,
                  other_expenditure_without_0,
                  
                  hh_total_debt_without_0,
                  rental_cost_without_0,
                  total_income_excluding_0,
                  total_expenditure_excluding_0,
                  respondent_gender) %>% group_by(respondent_gender) %>%  summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/gender_medians_excluding_0.csv")

mydata %>% select(salaried_work,
                  casual_daily_labour,
                  income_own_business,
                  support_from_family_inlibya,
                  support_from_family_home,
                  support_from_family_europe,
                  humanitarian_assistance,
                  zakat,
                  income_other,
                  
                  food_expenditure,
                  rent_expenditure,
                  shelter_maintenance_expenditure,
                  water_expenditure,
                  nfi_expenditure,
                  utilities_expenditure,
                  fuel_expenditure,
                  health_related_expenditure,
                  education_related_expenditure,
                  transportation_expenditure,
                  mobile_phone_credit_expenditure,
                  productive_assets_expenditure,
                  debt_repayment_expenditure,
                  other_expenditure,
                  
                  hh_total_debt,
                  rental_cost,
                  total_income,
                  total_expenditure) %>% summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/overall_medians.csv")


mydata %>% select(salaried_work,
                  casual_daily_labour,
                  income_own_business,
                  support_from_family_inlibya,
                  support_from_family_home,
                  support_from_family_europe,
                  humanitarian_assistance,
                  zakat,
                  income_other,
                  
                  food_expenditure,
                  rent_expenditure,
                  shelter_maintenance_expenditure,
                  water_expenditure,
                  nfi_expenditure,
                  utilities_expenditure,
                  fuel_expenditure,
                  health_related_expenditure,
                  education_related_expenditure,
                  transportation_expenditure,
                  mobile_phone_credit_expenditure,
                  productive_assets_expenditure,
                  debt_repayment_expenditure,
                  other_expenditure,
                  
                  hh_total_debt,
                  rental_cost,
                  total_income,
                  total_expenditure,
                  mantika_label) %>% group_by(mantika_label) %>%  summarise_all(list(~median(.,na.rm = T))) %>%
  write.csv("../final_analysis/mantika_medians.csv")

select_one_names <- filter(questions, str_detect(type,"(\\bselect_one\\b)"))$name
select_one_names<- select_one_names[select_one_names %in% names(data)]
select_multiple_names <- names(data)[str_detect(names(data),"(\\b[.]\\b)")]
select_one_multiple_data <- data[,c(select_one_names,select_multiple_names)]

select_one_multiple_data <- select_one_multiple_data %>% mutate(
  rcsi_category = mydata$rcsi_category,
  cash_coping = mydata$cash_coping,
  time_arrival_category = mydata$time_arrival_category,
  type_jobs_categories = mydata$type_job_categories,
  mantika_label = mydata$mantika_label
)

select_one_multiple_data <- select_one_multiple_data %>%
  select(-enumerator_id, -organisation_name, -pulled_name,
       -baladiya, -consent)

reachR::aggregate_count(select_one_multiple_data,split.by ="time_arrival_category",ignore.missing.data = T,
                        write.to.file = "../final_analysis/counts_by_time_arrival_category.csv")

table(mydata$time_arrival_category,mydata$mantika_label) %>% 
  write.csv("../final_analysis/counts_mantika_time_arrival_category.csv")

table(mydata$type_job_categories,mydata$mantika_label) %>% 
  write.csv("../final_analysis/counts_mantika_type_jobs.csv")
