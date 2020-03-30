library(Setviz)
library("UpSetR")
library(data.table)
library(purrr)

Setviz_data <- composite_data %>% mutate(
  protection = ifelse(protection_score >=3,1,0),
  shelter_nfi = ifelse(shelter_nfi_score>=3,1,0),
  fs = ifelse(fs_score>=3,1,0),
  health = ifelse(health_score>=3,1,0),
  wash = ifelse(wash_score>=3,1,0),
  education = ifelse(education_score>=3,1,0),
  capacity_gap = ifelse(capacity_gap_score ==4,1,0),
  capacity_gap_lowerthan_4 = ifelse(capacity_gap_score < 4,1,0),
  vul_lsg_one_cg_lower4 = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap_lowerthan_4>0,"yes","no"),
  vul_atlease_one = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap>0,"yes","no"),
  vul_atlease_two = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap>1,"yes","no"),
  vul_score_one = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap == 1,"yes","no"),
  vul_score_two = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap == 2,"yes","no"),
  vul_score_three = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap == 3,"yes","no"),
  vul_score_four = ifelse(protection+shelter_nfi+fs+health+wash+education+capacity_gap == 4,"yes","no"),
  vul_lsg_one_and_cg4 = ifelse((protection+shelter_nfi+fs+health+wash+education > 0) &
                                 capacity_gap > 0, "yes",
                               "no"),
  lsg_severity_score_lower3_cg4 = ifelse((protection+shelter_nfi+fs+health+wash+education == 0) &
                                           capacity_gap > 0, "yes",
                                         "no"),
  lsg_3 = ifelse(protection+shelter_nfi+fs+health+wash+education >0,"yes","no")
)

setnames(x = Setviz_data,
         old = c("protection", "shelter_nfi", "fs", "health", "wash", "education", "capacity_gap"),
         new = c("Protection", "Shelter_NFI", "FSL", "Health", "WASH", "Education", "Capacity_gap"))

plot_set_percentages(data = filter(Setviz_data,lsg_3 =="yes"),
                     varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education"),
                     mutually_exclusive_sets = T,
                     exclude_unique = F,
                     round_to_1_percent = T,
                     nintersects = 12)

plot_set_percentages(data = Setviz_data,
                     varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education"),
                     mutually_exclusive_sets = T,
                     exclude_unique = F,
                     round_to_1_percent = T,
                     nintersects = 12)


plots <- Setviz_data %>% filter(vul_atlease_one =="yes") %>%
              split(.$mantika_label) %>% purrr::map(plot_set_percentages,
              varnames = c("Protection","Shelter_NFI","FSL","Health","WASH","Education","Capacity_gap"),
              mutually_exclusive_sets = T,
              exclude_unique = F,
              round_to_1_percent = T,
              nintersects = 12)

filenames<- unique(Setviz_data$mantika_label)
filenames <- filenames[order(filenames)]
purrr::map2(plots,filenames,function(plot, fn){
  on.exit({dev.off()})
  pdf(paste0("./intersection_",fn,".pdf"))
  print(plot)
})


######J
library(extrafont)
font_import(pattern="ARIALN")
fonts()
fonttable()
loadfonts(device="win")

filenames <- str_c(filenames, ".png")

map2(filenames,plots, function(fn,plot){
  png(filename = fn, width = 784, height = 512, family = "arial", res = 100)
  print(plot)
  dev.off()
})


msni19::venn_msni(df = Setviz_data,
                  lsg = c("protection_score",
                          "shelter_nfi_score",
                          "fs_score",
                          "health_score",
                          "wash_score",
                          "education_score"),
                  capacity_gaps = "capacity_gap_score",
                  weighting_function = NULL,
                  print_plot = T,
                  plot_name = "graphs")



msni19::venn_msni(df = filter(Setviz_data, vul_atlease_one =="yes"),
                  lsg = c("Protection",
                          "Shelter_NFI",
                          "FSL",
                          "Health",
                          "WASH",
                          "Education"),
                  capacity_gaps = "Capacity_gap",
                  weighting_function = NULL,
                  print_plot = T,
                  plot_name = "graphs")
