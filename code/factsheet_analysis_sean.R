region <- read.csv("./input/region.csv")
Setviz_data <- merge(unique(region), Setviz_data, by='mantika_label')

#% of PIN with vulnerability score of at least 2
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_atlease_two",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/1_pct_pin_vul_atleast2.csv")

#Number of PIN with a vulnerability score of at least 2
table(filter(Setviz_data,vul_atlease_one =="yes")[["vul_atlease_two"]]) %>% 
  write.csv("../final_analysis/factsheet_analysis_sean/2_nb_pin_vul_atleast2.csv")

#% of PIN with a vulnerability score of 1
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_one",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/3_pct_pin_vul_onesector.csv")

#% of PIN with a vulnerability score of 2
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_two",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/4_pct_pin_vul_twosector.csv")

#% of PIN with a vulnerability score of 3
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_three",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/5_pct_pin_vul_threesector.csv")

#% of PIN with a vulnerability score of 4
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_four",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/6_pct_pin_vul_foursector.csv")

#% of PIN with a vulnerability score of at least 2, per region in Libya
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_atlease_two",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/7_pct_pin_vul_atleast2_region.csv")

#% of PIN with a vulnerability score of at least 2, per region of origin 
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_atlease_two",
           by = "region_of_origin",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/8_pct_pin_vul_atleast2_region_origin.csv")

# 8 Corrected
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_one",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/pct_vulnerability_score_1.csv")

proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_two",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/pct_vulnerability_score_2.csv")

proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_three",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/pct_vulnerability_score_3.csv")

proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_score_four",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/pct_vulnerability_score_4.csv")
#% of PIN with a vulnerability score of at least 2, per assessed location 
proportion(data = filter(Setviz_data,vul_atlease_one =="yes"),
           variable = "vul_atlease_two",
           by = "mantika_label",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/9_pct_pin_vul_atleast2_mantika.csv")

#% of total sample with a capacity gap score of 4
Setviz_data %>% summarise(
  pct_sample_CG_4 = mean(capacity_gap) * 100
) %>%
  write.csv("../final_analysis/factsheet_analysis_sean/10_pct_sample_CG_4.csv")

#number of total sample with a capacity gap score of 4
Setviz_data %>% group_by(capacity_gap) %>% count %>%
  write.csv("../final_analysis/factsheet_analysis_sean/11_nbr_sample_CG_4.csv")

#% of R&M with a capacity gap score of 4, per region of origin 
Setviz_data %>% group_by(region_of_origin) %>% summarise(
  pct_sample_CG_4 = mean(capacity_gap) * 100
) %>%
  write.csv("../final_analysis/factsheet_analysis_sean/12_pct_sample_CG_4_region_origin.csv")

#% of R&M with a capacity gap score of 4, per region in Libya 
Setviz_data %>% group_by(region) %>% summarise(
  pct_sample_CG_4 = mean(capacity_gap) * 100
) %>%
  write.csv("../final_analysis/factsheet_analysis_sean/13_pct_sample_CG_4_region.csv")

#Q17  corrected 
proportion(
  data = Setviz_data,
  variable = "capacity_gap_score",
  by = "region",
  path = "../final_analysis/factsheet_analysis_sean/13_pct_sample_CG_region.csv",
  digits = 1
)

#% of R&M with a capacity gap score of 4, per location
Setviz_data %>% group_by(mantika_label) %>% summarise(
  pct_sample_CG_4 = mean(capacity_gap) * 100
) %>%
  write.csv("../final_analysis/factsheet_analysis_sean/14_pct_sample_CG_4_mantika.csv")

#% of respondents who had at least one LSG severity score of at least 3 (in 1 or more sectors), AND/OR a capacity gap severity score of 4 (PIN)
proportion(data = Setviz_data,
           variable = "vul_atlease_one",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/15_pct_sample_atleast_one.csv")

#% of respondents who had at least one LSG severity score of at least 3, and/or a capacity gap severity score LOWER than 4
proportion(data = Setviz_data,
           variable = "vul_lsg_one_cg_lower4",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/16_pct_vul_lsg_one_andor_cg_lower4.csv")

#% of respondents who have at least one LSG severity score AND a capacity gap score of 4
proportion(data = Setviz_data,
           variable = "vul_lsg_one_and_cg4",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/17_pct_vul_lsg_one_and_cg4.csv")

#% of respondents who had an LSG severity score LOWER than 3, but had a capacity hap score of 4
proportion(data = Setviz_data,
           variable = "lsg_severity_score_lower3_cg4",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/18_pct_lsg_severity_score_lower3_cg4.csv")

#respondents who are PIN
table(Setviz_data$vul_atlease_one) %>%
  write.csv("../final_analysis/factsheet_analysis_sean/19_nb_respondents_whoare_pin.csv")

#% respondents who are PIN per region of origin 
proportion(data = Setviz_data,
           variable = "vul_atlease_one",
           by = "region_of_origin",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/20_pct_pin_origin.csv")

#% respondents who are PIN per region of inLibya 
proportion(data = Setviz_data,
           variable = "vul_atlease_one",
           by = "region",
           digits = 0,
           path = "../final_analysis/factsheet_analysis_sean/21_pct_pin_region.csv")
w