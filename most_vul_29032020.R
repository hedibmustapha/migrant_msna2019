

library(rio)
a <- data %>% select(.,salaried_work,
                     casual_daily_labour,
                     support_from_family_inlibya,
                     support_from_family_home,
                     support_from_family_europe,
                     humanitarian_assistance,
                     zakat,
                     income_other,
                     income_own_business)


df5 <-  as.data.frame(apply(df4, 2, as.numeric))
df5 <-  df4[,2:3]

b <-  rowSums(x = as.data.frame(apply(df4, 2, as.numeric)),na.rm = T)
c <-  colSums(x = as.data.frame(apply(df4, 1, as.numeric)),na.rm = T)

str( as.data.frame(apply(df4, 1, as.numeric)))
psych::describe(df4)

summarize( as.data.frame(apply(df4, 1, as.numeric)))

aa <- df4 %>% mutate(
  sum = rowSums(x = as.data.frame(apply(df4, 2, as.numeric))[,-1],na.rm = T),
                )




my_data <- data %>% mutate(
  total_income = rowSums(select(.,salaried_work,
                                casual_daily_labour,
                                support_from_family_inlibya,
                                support_from_family_home,
                                support_from_family_europe,
                                humanitarian_assistance,
                                zakat,
                                income_other,
                                income_own_business), na.rm = T))
  

df2 <- filter(my_data, obtaining_cash_challenges=="yes", 
              obtaining_cash_challenge_reasons.salary_too_low==1 ,
              obtaining_cash_challenge_reasons.lack_work_opportunity==1,
              shelter_condition == "light_damage" | shelter_condition == "medium_damage"|
              shelter_condition == "heavy_damage" 
) %>%
  group_by(.,hh_size) %>% 
  summarise(.,
                median.income= median(total_income,na.rm = TRUE)) 

# filter(df2,hh_size<7)
# new.el <- c("7+",sum(rowSums(df2[7:11,2])))

output <- as.data.frame(rbind(filter(df2,hh_size<7),c("7+",median(rowSums(df2[7:11,2]),na.rm = TRUE))))

export(output,"./output/median_incomev2.csv")



df3 <- data %>% group_by(.,hh_size) %>% 
  summarise(.,
            rent_expenditure = format(round(median(rent_expenditure,na.rm = TRUE), 2), nsmall = 2),
            health_related_expenditure = format(round(median(health_related_expenditure,na.rm = TRUE), 2), nsmall = 2),
            education_related_expenditure = format(round(median(education_related_expenditure,na.rm = TRUE), 2), nsmall = 2),
            transportation_expenditure = format(round(median(transportation_expenditure,na.rm = TRUE), 2), nsmall = 2),
            mobile_phone_credit_expenditure = format(round(median(mobile_phone_credit_expenditure,na.rm = TRUE), 2), nsmall = 2),
  )

df4 <- data %>% group_by(.,hh_size) %>% 
  summarise(.,
            rent_expenditure = format(round(mean(rent_expenditure,na.rm = TRUE), 2), nsmall = 2),
            health_related_expenditure = format(round(mean(health_related_expenditure,na.rm = TRUE), 2), nsmall = 2),
            education_related_expenditure = format(round(mean(education_related_expenditure,na.rm = TRUE), 2), nsmall = 2),
            transportation_expenditure = format(round(mean(transportation_expenditure,na.rm = TRUE), 2), nsmall = 2),
            mobile_phone_credit_expenditure = format(round(mean(mobile_phone_credit_expenditure,na.rm = TRUE), 2), nsmall = 2),
)

export(df4,"./output/expenditure_overall.csv")

export(output,"./output/median_income.csv")
