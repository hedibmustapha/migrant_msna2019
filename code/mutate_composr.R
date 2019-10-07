library(tidyverse)
library(composr)

mydata <- merge(unique(region), mydata, by='mantika_label')

mydata <- data %>% mutate(
  cash_coping_stress= rowSums(select(.,
                                     sold_nonproductive_hh_assets,
                                     spent_savings,
                                     borrowed_money,
                                     borrowed_purchased_oncredit_food)%>%
                                mutate_all(~.x %in% c("yes", "no_already_exhausted")),
                              na.rm = T),
  cash_coping_severe= rowSums(select(.,
                                     sold_productive_hh_assets,
                                     reduced_expenditures_essential_nfi,
                                     reduced_expenditures_health_education,
                                     child_dropped_school,
                                     delayed_skipped_rent,
                                     took_additional_job)%>%
                                mutate_all(~.x %in% c("yes", "no_already_exhausted")),
                              na.rm = T),
  cash_coping_extreme= rowSums(select(.,
                                      begging,
                                      adult_accepting_degrading_illegal_work,
                                      minor_accepting_degrading_illegal_work,
                                      child_marriage)%>%
                                mutate_all(~.x %in% c("yes", "no_already_exhausted")),
                              na.rm = T),
  cash_coping = case_when(
    cash_coping_stress > 0 ~ "stress",
    cash_coping_severe > 0 ~ "severe",
    cash_coping_extreme > 0 ~ "extreme",
    cash_coping_stress + cash_coping_severe + cash_coping_extreme == 0 ~ "minimal"
  ),
  
  salaried_work_without_0 = ifelse(salaried_work==0, NA, salaried_work),
  casual_daily_labour_without_0 = ifelse(casual_daily_labour==0, NA, casual_daily_labour),
  income_own_business_without_0 = ifelse(income_own_business==0, NA, income_own_business),
  support_from_family_inlibya_without_0 = ifelse(support_from_family_inlibya==0, NA, support_from_family_inlibya),
  support_from_family_home_without_0 = ifelse(support_from_family_home==0, NA, support_from_family_home),
  support_from_family_europe_without_0 = ifelse(support_from_family_europe==0, NA, support_from_family_europe),
  humanitarian_assistance_without_0 = ifelse(humanitarian_assistance==0, NA, humanitarian_assistance),
  zakat_without_0 = ifelse(zakat==0, NA, zakat),
  income_other_without_0 = ifelse(income_other==0, NA, income_other),
  
  food_expenditure_without_0 = ifelse(food_expenditure==0, NA, food_expenditure),
  rent_expenditure_without_0 = ifelse(rent_expenditure==0, NA, rent_expenditure),
  shelter_maintenance_expenditure_without_0 = ifelse(shelter_maintenance_expenditure==0, NA, shelter_maintenance_expenditure),
  water_expenditure_without_0 = ifelse(water_expenditure==0, NA, water_expenditure),
  nfi_expenditure_without_0 = ifelse(nfi_expenditure==0, NA, nfi_expenditure),
  utilities_expenditure_without_0 = ifelse(utilities_expenditure==0, NA, utilities_expenditure),
  fuel_expenditure_without_0 = ifelse(fuel_expenditure==0, NA, fuel_expenditure),
  health_related_expenditure_without_0 = ifelse(health_related_expenditure==0, NA, health_related_expenditure),
  education_related_expenditure_without_0 = ifelse(education_related_expenditure==0, NA, education_related_expenditure),
  transportation_expenditure_without_0 = ifelse(transportation_expenditure==0, NA, transportation_expenditure),
  mobile_phone_credit_expenditure_without_0 = ifelse(mobile_phone_credit_expenditure==0, NA, mobile_phone_credit_expenditure),
  productive_assets_expenditure_without_0 = ifelse(productive_assets_expenditure==0, NA, productive_assets_expenditure),
  debt_repayment_expenditure_without_0 = ifelse(debt_repayment_expenditure==0, NA, debt_repayment_expenditure),
  other_expenditure_without_0 = ifelse(other_expenditure==0, NA, other_expenditure),
  
  hh_total_debt_without_0 = ifelse(hh_total_debt==0, NA, hh_total_debt),
  rental_cost_without_0 = ifelse(rental_cost ==0, NA, rental_cost)
  
) %>% mutate(
  total_income = rowSums(select(.,salaried_work,
                                          casual_daily_labour,
                                          support_from_family_inlibya,
                                          support_from_family_home,
                                          support_from_family_europe,
                                          humanitarian_assistance,
                                          zakat,
                                          income_other,
                                income_own_business), na.rm = T),
  total_expenditure = rowSums(select(.,food_expenditure,
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
                                               other_expenditure), na.rm = T)) %>%
  mutate(total_income_excluding_0= ifelse(total_income==0, NA, total_income),
         total_expenditure_excluding_0= ifelse(total_expenditure==0, NA, total_income))

mydata <- mydata %>% new_recoding(school_issue_faced) %>%
  recode_to(to = "minimal",
            where = sm_selected(issues_faced_attending_school, 
                                exactly = c("no_issue_faced_attending_school"))) %>%
  recode_to(to = "stress",
            where = sm_selected(issues_faced_attending_school, 
                                any = c("lack_seperate_safe_toilets", "lack_functioning_latrines",
                                        "lack_clean_water", "lack_accessibility_disabled",
                                        "overcrowding_class", "poor_quality_teachers"))) %>%
  recode_to(to = "severe",
            where = sm_selected(issues_faced_attending_school,
                                any = c("violence_from_teacher", "violence_from_students"))) %>%
  recode_to(to = "extreme",
            where = sm_selected(issues_faced_attending_school,
                        any = c("sexual_violence_harassment", "discrimination_school",
                                "attempted_recruitment_armed_actors", "school_building_conversion_other_purpose")))

mydata <- mydata %>% new_recoding(challenges_accessing_healthcare) %>%
  recode_to(to = "minimal",
            where= access_healthcare_challenges %in% c("no")) %>%
recode_to( to = "stress",
           where = sm_selected(access_healthcare_challenges_reasons, any = c("distance_healthfacilities_toofar",
                                                                             "no_lack_money_topay_care",
                                                                             "lack_transportations_to_healthfacilities",
                                                                             "lack_medical_staff","lack_private_rooms",
                                                                             "lack_medicines",
                                                                             "lack_medical_supplies",
                                                                             "lack_documentation_health",
                                                                             "price_barriers_access_medical_aid",
                                                                             "linguistic_barriers","didnot_know_where_togo"))) %>%
recode_to(to = "severe",
          where = sm_selected(access_healthcare_challenges_reasons, any = c("lack_female_medical_staff",
                                                                           "unavailable_healthcare_accept_migrants",
                                                                           "female_denial_permission","female_absence_male","afraid_become_known_migrant"))) %>%
recode_to(to = "extreme",
          where = sm_selected(access_healthcare_challenges_reasons, any = c("healthfacilities_destroyed",
                                                                            "route_healthfacilities_isunsafe",
                                                                            "presence_explosive_hazard_healthfacilities",
                                                                            "no_available_healthfacilities")))

mydata <- mydata %>% new_recoding(psychologica_distress_signs) %>%
  recode_to(to = "yes",
            where = sm_selected(psycho_distress, none = c("none_psycho_distress", "dwta"))) %>%
  recode_to(to = "no",
            where = sm_selected(psycho_distress, any = c("none_psycho_distress"))) %>% end_recoding()


mydata <- mydata %>% new_recoding(psychologica_distress_signs_under18) %>%
  recode_to(to = "yes",
            where = sm_selected(psycho_distress, none = c("none_psycho_distress", "dwta")) & nb_under18>0) %>%
  recode_to(to = "no",
            where = sm_selected(psycho_distress, any = c("none_psycho_distress"))| nb_under18==0) %>% end_recoding()


mydata <- mydata %>% new_recoding(age_category)%>%
  recode_to(to = "more_than_64",
            where = respondent_age > 64)%>%
  recode_to(to = "between_25_64",
            where = respondent_age < 65) %>%
  recode_to(to = "between_18_24",
            where = respondent_age <25) %>% end_recoding()


mydata <- data %>%
  separate(arrive_libya ,c("day", "month", "year"), sep = "/") %>% 
  unite(date,year,month,day,sep = "")

mydata <- mydata %>% mutate(
  time_arrival_category = ifelse(date >= "20170701", "within_2_years", "over_2_years")
)


mydata <- mydata %>% new_recoding(target = intention_leave_lby, source = migration_intention) %>%
  recode_to(to = "want_to_leave",
            where.selected.exactly = c("leave_lby")) %>%
  recode_to(to = "does_not_want_to_leave_lby",
            where.selected.any = c("stay_current_location_lby","move_another_location_lby")) %>% end_recoding()


mydata <- data %>% mutate(
  with_children_uasc= ifelse(rowSums(select(.,nb_under18,uasc_male,uasc_female))>0,"yes","no"))

mydata <- data %>% new_recoding(target = buy_only_myself, source = domestic_consumption_purchase_forwhom)%>%
  recode_to(to = "yes",
            where.selected.exactly = c("buy_myself")) %>%
  recode_to(to = "no",
            where.selected.any = c("buy_for_myhh","share_certain_expenses_other_people","share_expenses_other_people","other")) %>%
  end_recoding()

mydata <- data %>% new_recoding(target = cash_challenges, source = obtaining_cash_challenges)%>%
  recode_to(to = "yes",
            where.selected.exactly = c("yes")) %>%
  recode_to(to = "no",
            where.selected.exactly = c("no")) %>%
  end_recoding()


mydata <- data %>% new_recoding(target = refugee_producung_countries, source = country)%>%
  recode_to(to = "yes",
            where.selected.any = c("eritrea","sudan","south_sudan","somalia","ethiopia","iraq","palestine","syria","yemen")) %>%
  recode_to(to = "no",
            where.selected.none = c("eritrea","sudan","south_sudan","somalia","ethiopia","iraq","palestine","syria","yemen")) %>%
  end_recoding()


mydata <- data %>% new_recoding(target = live_with_family) %>%
  recode_to(to = "yes",
            where =  sm_selected(live_with_who, any = c("live_with_family"))) %>%
  recode_to(to = "no",
            where =  sm_selected(live_with_who, none = c("live_with_family", "dk","dwta"))) %>% end_recoding()
