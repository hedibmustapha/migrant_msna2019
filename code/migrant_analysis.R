rm(list = ls())

Sys.setenv("R_REMOTES_NO_ERRORS_FROM_WARNINGS"=TRUE)
remove.packages("hypegrammaR", c(.libPaths(),"C:/Users/REACH/Documents/R/win-library/3.4"))
devtools::install_github("ellieallien/hypegrammaR", build_opts = c())
library(hypegrammaR)
library(parallel)
library(dplyr)
Sys.setlocale("LC_ALL","C")


questions <- read.csv("./input/questionnaire_edited.csv", stringsAsFactors = F)
choices <- read.csv("./input/choices_edited.csv", stringsAsFactors = F)
choices$label..English..en. <- gsub("^\\d+[.]\\s*","", choices$label..English..en.)
data <- load_data(file = "./input/data.csv")
data <- mutate_if(data, is.character, na_if, "")
#sampling_frame <- load_samplingframe(file = "./input/sampling_frame.csv")
questionnaire <- load_questionnaire(data = data,
                                    questions = questions,
                                    choices = choices,
                                    choices.label.column.to.use = "label::English (en)")
analysisplan <- load_analysisplan(file = "./input/analysisplan.csv")
#analysisplan <- make_analysisplan_all_vars(df = data,questionnaire = questionnaire,repeat.for.variable = "mantika_label")

kobostandards::check_input(data = data, questions = questions, choices = choices,
                           analysisplan = analysisplan) %>% write.csv("./output/check_input.csv")


results <- from_analysisplan_map_to_output(data = data,
                                           analysisplan = analysisplan,
                                           questionnaire = questionnaire)

labeled_result <- map_to_labeled(results,questionnaire = questionnaire)

map_to_master_table(results_object = results$results, filename = "./output/lcsi_rcsi_fcs_regionorigin_mantika_gender.csv",
                    questionnaire = questionnaire)

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
                      dependent.var = "cash_coping",
                      independent.var = "time_arrival_category",
                      case = group_diff_case)

case_result %>% map_to_labeled(questionnaire) -> result_labeled
map_to_file(result_labeled$summary.statistic,"./output/cash_coping_by_arrival_time.csv")

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
map_to_file(result_labeled$summary.statistic,"./output/rcsi_by_job_categories.csv")
