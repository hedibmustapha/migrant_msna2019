many_records <- read.csv("./input/unmet_needs_records_v2.csv")

mydata <- mydata %>% recode_batch(tos = many_records$to_values,
             wheres = many_records$conditions,
             targets = many_records$target_variables,
             questionnaire = questionnaire) %>%
  end_recoding()

mydata <- mydata %>% mutate(
  wash_sector = case_when(
    rowSums(select(.,wash_index_1:wash_index_2) %>% mutate_all(~.x %in% NA)) == 2 ~ NA_real_,
    rowSums(select(.,wash_index_1:wash_index_2),na.rm = T) > 0 ~ 1,
    rowSums(select(.,wash_index_1:wash_index_2),na.rm = T) == 0 ~ 0
),
 shelter_sector = case_when(
   rowSums(select(.,shelter_index_1:shelter_index_3) %>% mutate_all(~.x %in% NA)) == 3 ~ NA_real_,
   rowSums(select(.,shelter_index_1:shelter_index_3),na.rm = T) > 0 ~ 1,
   rowSums(select(.,shelter_index_1:shelter_index_3),na.rm = T) == 0 ~ 0
 ),
 #cash_sector = case_when(
  # rowSums(select(.,cash_index_1:cash_index_2) %>% mutate_all(~.x %in% NA)) == 2 ~ NA_real_,
  #rowSums(select(.,cash_index_1:cash_index_2),na.rm = T) > 0 ~ 1,
  #rowSums(select(.,cash_index_1:cash_index_2),na.rm = T) == 0 ~ 0
#),
 protection_sector = case_when(
   rowSums(select(.,protection_index_1:protection_index_8) %>% mutate_all(~.x %in% NA)) ==  8 ~ NA_real_,
  rowSums(select(.,protection_index_1:protection_index_8),na.rm = T) > 0 ~ 1,
  rowSums(select(.,protection_index_1:protection_index_8),na.rm = T) == 0 ~ 0
),
nb_sectors_inneed = ifelse(rowSums(select(.,wash_sector,shelter_sector,food_sector,education_sector,health_sector,protection_sector) %>% 
                                     mutate_all(~.x %in% NA)) == 6, NA,
                           rowSums(select(.,wash_sector,shelter_sector,food_sector,education_sector,health_sector,protection_sector),na.rm = T)),
inneed_one_sectors = ifelse(nb_sectors_inneed == 1, 1,0),
inneed_two_sectors = ifelse(nb_sectors_inneed == 2, 1,0),
inneed_three_sectors = ifelse(nb_sectors_inneed == 3, 1,0),
inneed_four_sectors = ifelse(nb_sectors_inneed == 4, 1,0),
inneed_five_sectors = ifelse(nb_sectors_inneed == 5, 1,0),
inneed_six_sectors = ifelse(nb_sectors_inneed == 6, 1,0))

mydata %>% summarise(
  wash_sector = mean(wash_sector,na.rm = T) *100,
  shelter_sector = mean(shelter_sector,na.rm = T) *100,
  food_sector = mean(food_sector,na.rm = T) *100,
  education_sector = mean(education_sector,na.rm = T) *100,
  health_sector = mean(health_sector,na.rm = T) *100,
  protection_sector = mean(protection_sector,na.rm = T) *100,
  inneed_one_sectors = mean(inneed_one_sectors,na.rm = T) *100,
  inneed_two_sectors = mean(inneed_two_sectors,na.rm = T) *100,
  inneed_three_sectors = mean(inneed_three_sectors,na.rm = T) *100,
  inneed_four_sectors = mean(inneed_four_sectors,na.rm = T) *100,
  inneed_five_sectors = mean(inneed_five_sectors,na.rm = T) *100,
  inneed_six_sectors = mean(inneed_six_sectors,na.rm = T) *100
) %>% write.csv("../final_analysis/overall_all_sectors_unmet_needs_v2.csv")

mydata %>% group_by(mantika_label) %>% summarise(
    wash_sector = mean(wash_sector,na.rm = T) *100,
    shelter_sector = mean(shelter_sector,na.rm = T) *100,
    food_sector = mean(food_sector,na.rm = T) *100,
    education_sector = mean(education_sector,na.rm = T) *100,
    health_sector = mean(health_sector,na.rm = T) *100,
    protection_sector = mean(protection_sector,na.rm = T) *100,
    inneed_one_sectors = mean(inneed_one_sectors,na.rm = T) *100,
    inneed_two_sectors = mean(inneed_two_sectors,na.rm = T) *100,
    inneed_three_sectors = mean(inneed_three_sectors,na.rm = T) *100,
    inneed_four_sectors = mean(inneed_four_sectors,na.rm = T) *100,
    inneed_five_sectors = mean(inneed_five_sectors,na.rm = T) *100,
    inneed_six_sectors = mean(inneed_six_sectors,na.rm = T) *100) %>%
  write.csv("../final_analysis/mantika_all_sectors_unmet_needs_v2.csv")

