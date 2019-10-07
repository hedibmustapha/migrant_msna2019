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

