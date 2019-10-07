rm(list = ls())

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
library(hypegrammaR)
library(parallel)
library(dplyr)


questions <- read.csv("./input/questionnaire_edited.csv", stringsAsFactors = F)
choices <- read.csv("./input/choices_edited.csv", stringsAsFactors = F)
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)
data <- load_data(file = "./input/data.csv")
data <- mutate_if(data, is.character, na_if, "")
#sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
questionnaire <- load_questionnaire(data = mydata,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")
#analysisplan <- load_analysisplan(file = "./input/analysisplan_medians.csv")
#analysisplan <- make_analysisplan_all_vars(df = data,questionnaire = questionnaire,repeat.for.variable = "mantika_label")

kobostandards::check_input(data = data, questions = questions, choices = choices,
                           analysisplan = analysisplan) %>% write.csv("./output/check_input.csv")


results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = analysisplan,
                                           questionnaire = questionnaire
                                           )

map_to_master_table(results_object = labeled_results, filename = "./output/overall_repeat.csv")

#hypegrammaR:::map_to_generic_hierarchical_html(resultlist = results,
                                               #render_result_with = hypegrammaR:::from_result_map_to_md_table,
                                               #by_analysisplan_columns = c("dependent.var", "repeat.var.value"),
                                               #by_prefix = c("indicator: ","Mantika:"),
                                               #level = 2,
                                               #questionnaire = questionnaire,
                                               #dir = "./output",
                                               #filename = "overall_Mantika_output.html")


group_diff_case <- map_to_case(hypothesis.type = "group_difference",
                    dependent.var.type = "categorical",
                    independent.var.type = "categorical")

case_result<-map_to_result(data = mydata,
                      dependent.var = "psychologica_distress_signs_under18",
                      independent.var = "mantika_label",
                      case = group_diff_case)

median_direct_case <- map_to_case(hypothesis.type = "direct_reporting_median",
                    dependent.var.type = "numerical")

direct_case <- map_to_case(hypothesis.type = "direct_reporting",
                           dependent.var.type = "categorical")

case_result<- map_to_result(data = mydata %>% filter(mantika_label=="Tripoli"),
                           dependent.var = "psychologica_distress_signs_under18",
                           case = direct_case
                           )

map_to_visualisation(case_result)

case_result %>% map_to_labeled(questionnaire) -> result_labeled
map_to_file(result_labeled$summary.statistic,"./output/psychologica_distress_signs_under18_mantika.csv")
