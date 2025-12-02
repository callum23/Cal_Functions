# Cals quick functions

# most used functions
library(tidyverse)
#library(purrr)
library(lubridate)
library(janitor)
#library(tidyr)
library(haven)
library(foreign)
library(dplyr)
library(stringr)
library(readr)
library(beepr)

# used in sgss_infections
pfunc <- function(x){
  x <- x$processed
  x <- x[!duplicated(paste(x$nhsno, x$hosno, x$forename, x$surname, x$dob, x$epids, x$organism, x$spec_dt, x$specno, x$specimen_type_code, sep="_")),]
  x
}




# left and right function 
right = function (string, char) {substr(string,nchar(string)-(char-1),nchar(string))}
left = function (string,char) {substr(string,1,char)}
# cal_auto_dedupe
# this function is an auto dedupe that will just dedupe leaving the first duplicate in so there are no duplicates
cal_auto_dedupe <- function(df) {
  df <- df %>%
    mutate(dupe = duplicated(.)) %>%
    filter(dupe == F) %>%
    select(-dupe)
}


#_______________________________________________________________________________
# duplicated function (this will tell you if it has a duplicate within it not ignoring the first iteration of a duplicate)
cal_is_duplicated <- function(x) {
  duplicated(x) | duplicated(x, fromLast = T)
}

# # Example
# manual <- df %>%
#   mutate(dupe = cal_is_duplicated(key)) %>%
#   filter(dupe == TRUE)
  
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# this is used to show TRUE if it is a duplicate disregarding the first iteration of the duplication.
cal_dupe_to_remove <- function(x) {
  duplicated(x)
}

# # Example: 
# df_clean <- df %>%
#   arrange(desc(date)) %>%
#   mutate(rm = cal_dupe_to_remove(key)) %>%
#   filter(rm == FALSE)

#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# sort your tables
# setup for charts
produce_chart <- function() {
  
  chart <- summary_year_sex %>%
        mutate(organism_species_name = toupper(organism_species_name)) %>%
        filter(organism_species_name == name)
  
}
#_______________________________________________________________________________

gender_for_text <- function() {
  
gender_for_text <- chart %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
  filter(sex %in% c("male","female")) %>%
  arrange(desc(total))
}
#_______________________________________________________________________________

table_for_text <- function() {
  
table_for_text <- test_method_by_species %>%
  mutate(organism_species_name = toupper(organism_species_name)) %>%
  filter(organism_species_name == name)  %>%
  mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
  arrange(desc(total))

}
#_______________________________________________________________________________

total_trend <- function() {
  
  total_trend <- total_summary %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name)
  
}
#_______________________________________________________________________________

age_group_trends <- function() {
  
  age_group_trends <- total_summary %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    filter(trend_overview != total_trend[[1,18]]) %>%
    mutate(non_matched = paste0(age_group, " with a ", trend_overview, " trend"))
  
}
#_______________________________________________________________________________
#
# table <- function() {
#   
# table <- test_method_by_species %>%
#   mutate(organism_species_name = toupper(organism_species_name)) %>%
#   filter(organism_species_name == name)  %>%
#   mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
#   arrange(desc(total)) %>%
#   select(-total)
# 
# }
#_______________________________________________________________________________

age_perc <- function() {
  
  age_perc <- round(gender_for_text$total[1]/sum(gender_for_text$total)*100,1)
  
}
#_______________________________________________________________________________

age_for_text <- function() {
  
  age_for_text <- age_by_species %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    arrange(desc(total)) %>%
    select(-total)
  
}
#_______________________________________________________________________________

module_for_text <- function() {
  
  module_for_text <- table %>%   mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) 
  
  module_tot <- round(module_for_text$total[1]/sum(module_for_text$total)*100,1)
  module_tot <- sum(module_for_text$total)
  
  module_for_text <- module_for_text %>%
    mutate(summ = module_tot,
           perc = round((total/module_tot)*100,2)) %>%
    mutate(text = paste0(test_method_description, " , which accounts for ", perc,"% of the total"))
  
}
#_______________________________________________________________________________

trend_for_module <- function() {
  
  trend_for_module <-  flags_function(table) %>%
    mutate(match_trend = trend_overview ==  trend,
           missing_module = case_when(`2015` == 0 | `2016` == 0 | `2017` == 0 | `2018` == 0 | `2019` == 0 | `2020` == 0 | `2021` == 0 ~ TRUE,
                                      TRUE ~ FALSE))

}
#_______________________________________________________________________________

non_trend_for_text_1 <- function() {
  
  non_trend <- trend_for_module %>% filter(trend_overview != trend) %>% ungroup() %>% select(test_method_description,trend_overview)
  non_trend_for_text_1 <- non_trend[[1]]
  
}
#_______________________________________________________________________________

non_trend_for_text_2 <- function() {
  
  non_trend <- trend_for_module %>% filter(trend_overview != trend) %>% ungroup() %>% select(test_method_description,trend_overview)
  non_trend_for_text_2 <- non_trend[[2]]
  
}
#_______________________________________________________________________________
missing_data_module <- function() {
  
  missing_data_module <- trend_for_module %>%
    filter(missing_module == TRUE)
  
  missing_data_module <- missing_data_module[[2]]
}
#_______________________________________________________________________________

diff <- function() {
  age_for_text_1 <- age_by_species %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) 
  
  summ_tot <- sum(age_for_text_1$total)
  
  age_for_text_1 <- age_for_text_1 %>%
    mutate(summ = summ_tot) %>%
    filter(age_group %in% c("28_days","29-264_days","1-4_years"))
  
  summ_low <- sum(age_for_text_1$total)
  
  diff <- round((summ_low/summ_tot)*100,1)
}
#_______________________________________________________________________________

diff_high <- function() {
  
  age_for_text_1 <- age_by_species %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) 
  
  summ_tot <- sum(age_for_text_1$total)
  
  age_for_text_1 <- age_for_text %>%
    filter(age_group %in% c(age_for_text[[1,2]],age_for_text[[2,2]],age_for_text[[3,2]])) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
  
  summ_high <- sum(age_for_text_1$total)
  
  diff_high <- round((summ_high/summ_tot)*100,1)
  
}

#_______________________________________________________________________________
age_for_text_low <- function() {
  
  age_for_text_low <- age_for_text %>% 
    filter(age_group %in% c("28_days","29-264_days","1-4_years"))
  
  age_for_text_low <- age_for_text_low[[2]]
  
}
#_______________________________________________________________________________
module_for_text_2_3 <- function() {
   
  key <- module_for_text[[1,2]]
  
  module_for_text_2_3 <- module_for_text %>%
     filter(test_method_description != key)

   module_for_text_2_3 <- module_for_text_2_3[[13]]
   
}
#_______________________________________________________________________________
# if you have something created in the function it will be forgotten directly after
# if you want something remembred then you have to cast it.
# chart <- produce_chart()
# gender_for_text <- gender_for_text()
# table_for_text <- table_for_text()
# age_group_trends <- age_group_trends()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 1 for big doc
sex_overall_trend_chart <- function() {

  total_trend <- total_summary %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name)
  
  ggplot(chart,aes(x=year,y=value,group=sex)) +
    geom_line(aes(color=sex)) +
    labs(title = paste0("Overall trend of ",name)) +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    #scale_color_manual(values = c("darkred","steelblue")) +
    geom_vline(xintercept = "2020", linetype="dotted", 
               color = "red", size=1.5)
  
  #missing_dob <- as.character(age_group_trends %>% filter(age_group == "No dob") %>% select(total))
}
#_______________________________________________________________________________
# sex_overall_trend_chart()
#_______________________________________________________________________________
#_______________________________________________________________________________



# chart 1 for big doc
sex_overall_rate_chart <- function() {

  chart <- summary_year_sex %>%
    mutate(organism_species_name = toupper(organism_species_name),
           year = as.numeric(year)) %>%
    filter(organism_species_name == name,
           sex != "unknown") %>%
    rename(calendar_year = year) %>%
    left_join(annual_population_2015_2021_sex, by = c("calendar_year","sex")) %>%
    left_join(annual_population_2015_2021, by = "calendar_year") %>%
    mutate(pop = pop.x,
           pop = ifelse(is.na(pop) & sex == "total", pop.y, pop)) %>%
    select(-pop.x,-pop.y) %>%
    mutate(rate_per_100000 = (value/pop*100000))
  
  ggplot(chart,aes(x=calendar_year,y=rate_per_100000,group=sex)) +
    geom_line(aes(color=sex)) +
    labs(title = paste0("Rate of ",name, 
                        " per 100,000 population"), y = "Rate per 100,000 population", x = "Calendar year") +
    theme(plot.title = element_text(hjust = 0.5, size = 10)) +
    scale_x_continuous(breaks=seq(2015,2021,1)) + 
    geom_vline(xintercept = 2020, linetype="dotted", 
                                                          color = "red", size=1.5)
}


#_______________________________________________________________________________
# sex_overall_rate_chart()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 2 
# makes a table that shows age_group by spp
age_group_by_spp <- function() {
age_by_species %>%
  mutate(organism_species_name = toupper(organism_species_name)) %>%
  filter(organism_species_name == name) %>%
  arrange(desc(total)) %>%
  select(-total) %>%
  flextable() %>% 
  #align(part = "all") %>% # left align
  font(fontname = "Calibri (Body)", part = "all") %>% 
  fontsize(size = 12, part = "body") %>% 
  theme_booktabs() %>% # default theme
  autofit() %>%
  fit_to_width(8)
}
#_______________________________________________________________________________
# age_group_by_spp()
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 3
# produces a table showing all the age groups that do not match to the overall trend
non_matching_age_groups_to_trend <- function() {
  chart_non_matched <- age_group_trends %>%
    select(age_group,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`) %>%
    pivot_longer(`2015`:`2021`,names_to = "year", values_to = "count") 
  
  fig <- chart_non_matched %>%
    ggplot(aes(x=year,y=count,group = 1)) +
    geom_line() +
    facet_wrap(vars(age_group),ncol = 3) +
    geom_vline(xintercept = "2019", linetype="dotted", color = "red", size=1.5) +
    scale_color_manual(values = c("darkred","steelblue")) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  
  if(nrow(chart_non_matched)!=0){
    print(fig)
    }
}

#_______________________________________________________________________________
# non_matching_age_groups_to_trend()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 4
# makes your test method description chart
test_method_description_table <- function() {
  
  age_for_text_1 <- age_by_species %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) 
  
  summ_tot <- sum(age_for_text_1$total)
  
  age_for_text_1 <- age_for_text_1 %>%
    mutate(summ = summ_tot) %>%
    filter(age_group %in% c("28_days","29-264_days","1-4_years"))
  
  summ_low <- sum(age_for_text_1$total)
  
  diff <- round((summ_low/summ_tot)*100,1)
  
  table <- test_method_by_species %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name)  %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) %>%
    select(-total)
  
  table_for_doc <- table %>%
    ungroup() %>%
    select(-organism_species_name) %>%
    flextable() %>% 
    #align(part = "all") %>% # left align
    font(fontname = "Calibri (Body)", part = "all") %>% 
    fontsize(size = 10, part = "body") %>% 
    theme_booktabs() %>% # default theme
    autofit() %>%
    fit_to_width(8)
  
  gender_for_text <- chart %>%
    pivot_wider(names_from = year, values_from = value) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    filter(sex %in% c("male","female")) %>%
    arrange(desc(total))
  
  age_perc <- round(gender_for_text$total[1]/sum(gender_for_text$total)*100,1)
  
  table_for_text <- table %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
  
  age_for_text <- age_by_species %>%
    mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    arrange(desc(total)) %>%
    select(-total)
  
  age_for_text_low <- age_for_text %>% 
    filter(age_group %in% c("28_days","29-264_days","1-4_years"))
  
  age_for_text_low <- age_for_text_low[[2]]

  age_for_text_1 <- age_for_text %>%
    filter(age_group %in% c(age_for_text[[1,2]],age_for_text[[2,2]],age_for_text[[3,2]])) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`)
  
  summ_high <- sum(age_for_text_1$total)
  
  diff_high <- round((summ_high/summ_tot)*100,1)
  
  module_for_text <- table %>%   mutate(across(everything(), .fns = ~replace_na(.,0))) %>%
    mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>%
    arrange(desc(total)) 
  
  module_tot <- round(module_for_text$total[1]/sum(module_for_text$total)*100,1)
  module_tot <- sum(module_for_text$total)
  
  module_for_text <- module_for_text %>%
    mutate(summ = module_tot,
           perc = round((total/module_tot)*100,2)) %>%
    mutate(text = paste0(test_method_description, " , which accounts for ", perc,"% of the total"))
  
  module_for_text_1 <- module_for_text[[1,13]]
  
  key <- module_for_text[[1,2]]
  
  module_for_text_2_3 <- module_for_text %>%
    filter(test_method_description != key)
  
  module_for_text_2_3 <- module_for_text_2_3[[13]]
  
  trend_for_module <-  flags_function(table) %>%
    mutate(match_trend = trend_overview ==  trend,
           missing_module = case_when(`2015` == 0 | `2016` == 0 | `2017` == 0 | `2018` == 0 | `2019` == 0 | `2020` == 0 | `2021` == 0 ~ TRUE,
                                      TRUE ~ FALSE))
  
  non_trend <- trend_for_module %>% filter(trend_overview != trend) %>% ungroup() %>% select(test_method_description,trend_overview)
  non_trend_for_text_1 <- non_trend[[1]]
  non_trend_for_text_2 <- non_trend[[2]]
  
  
  missing_data_module <- trend_for_module %>%
    filter(missing_module == TRUE)
  
  missing_data_module <- missing_data_module[[2]]
  
  table_for_doc
}

#_______________________________________________________________________________
#test_method_description_table()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 5
# this produces a total chart by test method description
total_by_test_method_description <- function() {
table %>%
  ungroup() %>%
  select(-organism_species_name) %>%
  pivot_longer(!test_method_description, names_to = "year", values_to = "value") %>%
  mutate(value = as.numeric(value), year = as.character(year)) %>%
  ggplot(aes(x=year,y=value,group=test_method_description)) +
  geom_line(aes(color=test_method_description)) +
  theme(plot.title = element_text(hjust = 0.5, size = 10),
        legend.position = "bottom") +
  #scale_color_manual(values = c("darkred","steelblue")) +
  geom_vline(xintercept = "2019", linetype="dotted", 
             color = "red", size=1.5) +
  labs(title = paste0("Trend by test method description for ",name))
}

#_______________________________________________________________________________
#total_by_test_method_description()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# chart 6
# covid_apportionment_chart
covid_apportionment_chart <- function() {
  summary_cosecco %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    flextable() %>% 
    #align(part = "all") %>% # left align
    font(fontname = "Calibri (Body)", part = "all") %>% 
    fontsize(size = 12, part = "body") %>% 
    theme_booktabs() %>% # default theme
    autofit() %>%
    fit_to_width(8)
}

#_______________________________________________________________________________
#covid_apportionment_chart()
#_______________________________________________________________________________
#_______________________________________________________________________________
#
#
#
#
#
#
#
#_______________________________________________________________________________
# 
# 
amr_group <- function(drug,long_name) {
  
  amr_table <- amr_table1 %>%
    mutate(organism_species_name = toupper(organism_species_name)) %>%
    filter(organism_species_name == name) %>%
    select(-organism_species_name) %>%
    t(.) %>%
    as.data.frame(.) %>%
    row_to_names(row_number = 1) %>%
    tibble::rownames_to_column("drug_group") %>%
    filter(str_detect(drug_group,drug))
  
  # flextable the document
  flextable(amr_table) %>% 
    #align(part = "all") %>% # left align
    font(fontname = "Calibri (Body)", part = "all") %>% 
    fontsize(size = 8, part = "body") %>% 
    theme_booktabs() %>% # default theme
    autofit() %>%
    fit_to_width(8) %>%
    set_caption(caption = paste0("Numbers of ",long_name," by year for ",name), style = "Table Caption", autonum = autonum)
  
}

#_______________________________________________________________________________
#covid_apportionment_chart()
#_______________________________________________________________________________
#_______________________________________________________________________________







#_______________________________________________________________________________
#linkage_function() 
#_______________________________________________________________________________
# to be used after name changes etc
linkage_function <- function(fin,amr) {
  set_1 <- fin %>%
    select(organism,organism_genus_name ,nhsno, spec_dt, dob, forename, surname, hosno, sex, postcode,
           #lab_nm = laboratorywherespecimenprocessed,
           specno, lab_cd, wave, CovidWeek, days, coinfection, secondary28d, secondary60d, preceding28d,
           preceding60d, beforeCOVID, afterCOVID, concurrent,coviddate, Ethnicity, test_method_description
    )
  
  
  # make your sgss snip
  set_2 <- amr %>%
    select(organism = organism_species_name, nhsno, spec_dt, dob, forename, surname, hosno, sex, pastcode=patient_postcode,
           #lab_nm = laboratorywherespecimenprocessed,
           specno, lab_cd, drug_cd, drug_nm, amr
    )
  
  #_______________________________________________________________________________
  
  set_1 <- set_1 %>%
    mutate(
      spec_dt = as.Date(spec_dt),
      dob = as.Date(dob),
      lab_cd = as.integer(lab_cd),
      soundex = phonics::soundex(surname),
      #organism = ifelse(grepl("^kleb", specie), "kleb", specie),
      #extract= "",
      FI = toupper(substr(forename,1,1)),
      sex = case_when(toupper(sex) == "MALE" ~ "M",
                      toupper(sex) == "FEMALE" ~ "F",
                      toupper(sex) == "UNKNOWN" ~ "U"))
  
  set_2 <- set_2 %>%
    mutate(
      spec_dt = as.Date(spec_dt),
      dob = as.Date(dob),
      lab_cd = as.integer(lab_cd),
      soundex = phonics::soundex(surname),
      #organism = ifelse(grepl("^kleb", specie), "kleb", specie),
      #extract= "",
      FI = toupper(substr(forename,1,1)),
      sex = case_when(toupper(sex) == "MALE" ~ "M",
                      toupper(sex) == "FEMALE" ~ "F",
                      toupper(sex) == "UNKNOWN" ~ "U"))
  
  #_______________________________________________________________________________
  
  set_1 <- set_1 %>%
    mutate(data_type = "fin") %>%
    mutate(drug_cd = NA,
           drug_nm = NA,
           amr = NA)
  
  set_2 <- set_2 %>%
    mutate(data_type = "amr",
           extract = "SGSS") %>%
    mutate(wave = NA,
           CovidWeek = NA,
           days = NA,
           coinfection = NA,
           secondary28d = NA,
           secondary60d = NA,
           preceding28d = NA,
           preceding60d = NA,
           beforeCOVID = NA,
           afterCOVID = NA,
           concurrent = NA,
           coviddate = NA,
           Ethnicity = NA)
  
  #_______________________________________________________________________________
  
  linked_data <- bind_rows(set_1, set_2) %>%
    mutate_if(is.character, toupper)
  
  # clean environment
  rm(set_1,set_2)
  
  linked_data <- linked_data %>% 
    mutate(hosno_dcs = hosno,
           temp_f = as.integer(hosno),
           hosno = ifelse(is.na(temp_f), hosno, as.character(temp_f)))
  
  # make the epids NULL
  linked_data$pids <- linked_data$epids <- NULL
  
  #_______________________________________________________________________________
  
  linked_data <- linked_data %>%
    mutate(
      cri_1 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA) | dob %in% c(dmy("01/01/1900"),NA), NA, paste(nhsno, dob, sep="")),
      cri_2 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA),NA,nhsno),
      cri_3 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA) | dob %in% c(dmy("01/01/1900"),NA) | soundex %in% c("", NA), NA,paste(hosno,dob,soundex, sep="")),
      cri_4 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,hosno),
      cri_5 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
                       lab_cd %in% c(NA) |
                       soundex %in% c("", NA) |
                       FI %in% c("", NA) |
                       toupper(forename) %in% c("", "UNKNOWN", NA) |
                       sex %in% c("", "U", NA) |
                       dob %in% c(dmy("01/01/1900"),NA),
                     NA,
                     paste(specno,lab_cd,sex,FI,dob, soundex, sep="")
      ),
      cri_6 = ifelse(
        specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
          lab_cd %in% c(NA) |
          FI %in% c("", NA) |
          toupper(forename) %in% c("", "UNKNOWN", NA) |
          sex %in% c("", "U", NA),
        NA,
        paste(specno,lab_cd,sex,FI, sep="")
      ),
      cri_7 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_8 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_9 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,specno),
      # fuzzy dob
      pt1=paste(year(dob), str_pad(month(dob), width = 2, pad = 0), sep=""),
      pt2=paste(year(dob), str_pad(day(dob), width = 2, pad = 0), sep=""),
      pt3=paste(str_pad(day(dob), width = 2, pad = 0), str_pad(month(dob), width = 2, pad = 0), sep="")
    )
  
  
  linked_data$lk_sn <- 1:nrow(linked_data)
  linked_data$FI_2 <- linked_data$FI
  linked_data$soundex_2 <- linked_data$soundex
  
  
  # develop your patient_id
  linked_data$patient_id <- record_group(df = linked_data, # put in the data
                                         sn = lk_sn, # the unique id you created above "linked_data$lk_sn <- 1:nrow(linked_data)"
                                         criteria = c("cri_1", "cri_2", "cri_3", "cri_4", "cri_5", "cri_6", "cri_7", "cri_8","cri_9"), # list out the chriteria
                                         sub_criteria = list(s6 = c("FI","soundex"), s9.1 = c("pt1","pt2","pt3"), s9.2=c("FI_2","soundex_2")), to_s4 = T) # you are also identifying the sub_chriteria
  
  # # add in your episode_id
  # linked_data$episode_id <- diyar::episodes(sn = linked_data$lk_sn, # select the data
  #                                           date = linked_data$spec_dt, # isolate the date
  #                                           case_length = 13, # this is the 13 days that make it a group
  #                                           #data_source = linked_data$extract, # SGSS or DCS
  #                                           strata = diyar::combi(linked_data$patient_id, linked_data$organism),
  #                                           episode_type = "rolling")
  
  #_______________________________________________________________________________
  
  # make the episode_id as a character, then add in all the columns stuck together and then find out how many characters there are so we can get the max 
  linked_data <- linked_data %>%
    mutate(episode_chr_id = as.character(episode_id),
           no_chr = paste0(organism,nhsno,spec_dt,dob,forename,surname,hosno,sex,postcode,specno,lab_cd,wave,CovidWeek,days,coinfection,secondary28d,secondary60d,preceding28d,preceding60d,beforeCOVID,afterCOVID,concurrent,coviddate,Ethnicity),
           n_chr = str_length(no_chr)) %>%
    arrange(episode_chr_id)
  
  
  linked_data <- linked_data %>%
    mutate(dob_backup = as.Date(dob),
           spec_dt_backup = as.Date(spec_dt))
  
  #_______________________________________________________________________________
  
  # we will have n_chr arranged with the highest first
  # then group by the episode chr id
  # then make a rank variable 1 to end of each group
  # then you arrange it by episode chr id so that one is at the top
  # if there is no duplication rank should be 1
  
  # take out all the AMR data and find out how many are duplicated
  AMR <- linked_data %>%
    filter(data_type == "AMR") %>%
    mutate(dup_epids = cal_is_duplicated(episode_id)) %>%
    arrange(desc(n_chr)) %>%
    group_by(episode_chr_id) %>%
    mutate(rank = 1:n()) %>%
    arrange(episode_chr_id) # %>%
  #filter(dup_epids == T)
  
  # take out all the FIN data and find out how many are duplicated
  FIN <- linked_data %>%
    filter(data_type == "FIN") %>%
    mutate(dup_epids = cal_is_duplicated(episode_id)) %>%
    arrange(desc(n_chr)) %>%
    group_by(episode_chr_id) %>%
    mutate(rank = 1:n()) %>%
    arrange(episode_chr_id) # %>%
  
  #_______________________________________________________________________________
  
  # logic
  # now i have had a look at the duplications i will want to take out all the ones where rank does not == 1
  # this means i have selected all the largest character number and put at the top rank of 1 and kept all those without duplicate episode id and made rank == 1
  # de-duplicated FIN
  FIN_dedupe <- FIN %>%
    filter(rank == 1)
  
  # de-duplicated AMR
  AMR_dedupe <- AMR %>%
    filter(rank == 1)
  
  #_______________________________________________________________________________
  
  # bind them both together
  bound <- left_join(FIN_dedupe,AMR_dedupe,by="episode_id")
  
  # see if there are any duplications == 0
  nrow(FIN_dedupe)-nrow(bound)
  
  #_______________________________________________________________________________
  
  
  bound_clean <- bound %>%
    mutate(organism = ifelse(is.na(organism.x),organism.y,organism.x),
           nhsno = ifelse(is.na(nhsno.x),nhsno.y,nhsno.x),
           spec_dt = ifelse(is.na(spec_dt.x),spec_dt.y,spec_dt.x),
           dob = ifelse(is.na(dob.x),dob.y,dob.x),
           forename = ifelse(is.na(forename.x),forename.y,forename.x),
           surname = ifelse(is.na(surname.x),surname.y,surname.x),
           hosno = ifelse(is.na(hosno.x),hosno.y,hosno.x),
           sex = ifelse(is.na(sex.x),sex.y,sex.x),
           postcode = ifelse(is.na(postcode.x),postcode.y,postcode.x),
           specno = ifelse(is.na(specno.x),specno.y,specno.x),
           lab_cd = ifelse(is.na(lab_cd.x),lab_cd.y,lab_cd.x),
           wave = ifelse(is.na(wave.x),wave.y,wave.x),
           CovidWeek = ifelse(is.na(CovidWeek.x),CovidWeek.y,CovidWeek.x),
           days = ifelse(is.na(days.x),days.y,days.x),
           coinfection = ifelse(is.na(coinfection.x),coinfection.y,coinfection.x),
           secondary28d = ifelse(is.na(secondary28d.x),secondary28d.y,secondary28d.x),
           secondary60d = ifelse(is.na(secondary60d.x),secondary60d.y,secondary60d.x),
           preceding28d = ifelse(is.na(preceding28d.x),preceding28d.y,preceding28d.x),
           preceding60d = ifelse(is.na(preceding60d.x),preceding60d.y,preceding60d.x),
           beforeCOVID = ifelse(is.na(beforeCOVID.x),beforeCOVID.y,beforeCOVID.x),
           afterCOVID = ifelse(is.na(afterCOVID.x),afterCOVID.y,afterCOVID.x),
           concurrent = ifelse(is.na(concurrent.x),concurrent.y,concurrent.x),
           coviddate = ifelse(is.na(coviddate.x),coviddate.y,coviddate.x),
           Ethnicity = ifelse(is.na(Ethnicity.x),Ethnicity.y,Ethnicity.x),
           soundex = ifelse(is.na(soundex.x),soundex.y,soundex.x),
           FI = ifelse(is.na(FI.x),FI.y,FI.x),
           data_type = ifelse(is.na(data_type.x),data_type.y,data_type.x),
           drug_cd = ifelse(is.na(drug_cd.x),drug_cd.y,drug_cd.x),
           drug_nm = ifelse(is.na(drug_nm.x),drug_nm.y,drug_nm.x),
           amr = ifelse(is.na(amr.x),amr.y,amr.x),
           pastcode = ifelse(is.na(pastcode.x),pastcode.y,pastcode.x),
           extract = ifelse(is.na(extract.x),extract.y,extract.x),
           hosno_dcs = ifelse(is.na(hosno_dcs.x),hosno_dcs.y,hosno_dcs.x),
           temp_f = ifelse(is.na(temp_f.x),temp_f.y,temp_f.x),
           cri_1 = ifelse(is.na(cri_1.x),cri_1.y,cri_1.x),
           cri_2 = ifelse(is.na(cri_2.x),cri_2.y,cri_2.x),
           cri_3 = ifelse(is.na(cri_3.x),cri_3.y,cri_3.x),
           cri_4 = ifelse(is.na(cri_4.x),cri_4.y,cri_4.x),
           cri_5 = ifelse(is.na(cri_5.x),cri_5.y,cri_5.x),
           cri_6 = ifelse(is.na(cri_6.x),cri_6.y,cri_6.x),
           cri_7 = ifelse(is.na(cri_7.x),cri_7.y,cri_7.x),
           cri_8 = ifelse(is.na(cri_8.x),cri_8.y,cri_8.x),
           cri_9 = ifelse(is.na(cri_9.x),cri_9.y,cri_9.x),
           pt1 = ifelse(is.na(pt1.x),pt1.y,pt1.x),
           pt2 = ifelse(is.na(pt2.x),pt2.y,pt2.x),
           pt3 = ifelse(is.na(pt3.x),pt3.y,pt3.x),
           lk_sn = ifelse(is.na(lk_sn.x),lk_sn.y,lk_sn.x),
           FI_2 = ifelse(is.na(FI_2.x),FI_2.y,FI_2.x),
           soundex_2 = ifelse(is.na(soundex_2.x),soundex_2.y,soundex_2.x),
           #patient_id = ifelse(is.na(patient_id.x),patient_id.y,patient_id.x),
           #episode_id = ifelse(is.na(episode_id.x),episode_id.y,episode_id.x),
           episode_chr_id = ifelse(is.na(episode_chr_id.x),episode_chr_id.y,episode_chr_id.x),
           no_chr = ifelse(is.na(no_chr.x),no_chr.y,no_chr.x),
           n_chr = ifelse(is.na(n_chr.x),n_chr.y,n_chr.x),
           dup_epids = ifelse(is.na(dup_epids.x),dup_epids.y,dup_epids.x),
           rank = ifelse(is.na(rank.x),rank.y,rank.x),
           test_method_description = ifelse(is.na(test_method_description.x),test_method_description.y,test_method_description.x),
           organism_genus_name = ifelse(is.na(organism_genus_name.x),organism_genus_name.y,organism_genus_name.x))
  
  bound_clean <- bound_clean %>%
    select(-organism.x,-nhsno.x,-spec_dt.x,-dob.x,-forename.x,-surname.x,-hosno.x,-sex.x,-postcode.x,-specno.x,-lab_cd.x,
           -wave.x,-CovidWeek.x,-days.x,-coinfection.x,-secondary28d.x,-secondary60d.x,-preceding28d.x,-preceding60d.x,
           -beforeCOVID.x,-concurrent.x,-coviddate.x,-Ethnicity.x,-soundex.x,-FI.x,-data_type.x,-drug_cd.x,
           -drug_nm.x,-amr.x,-pastcode.x,-extract.x,-hosno_dcs.x,-temp_f.x,-cri_1.x,-cri_2.x,-cri_3.x,-cri_4.x,-cri_5.x,-cri_6.x,
           -cri_7.x,-cri_8.x,-cri_9.x,-pt1.x,-pt2.x,-pt3.x,-lk_sn.x,-FI_2.x,-soundex_2.x,-episode_chr_id.x,
           -no_chr.x,-n_chr.x,-dup_epids.x,-rank.x,-organism.y,-nhsno.y,-spec_dt.y,-dob.y,-forename.y,-surname.y,-hosno.y,-sex.y,
           -postcode.y,-specno.y,-lab_cd.y,-wave.y,-CovidWeek.y,-days.y,-coinfection.y,-secondary28d.y,-secondary60d.y,
           -preceding28d.y,-preceding60d.y,-beforeCOVID.y,-afterCOVID.y,-concurrent.y,-coviddate.y,-Ethnicity.y,-soundex.y,
           -FI.y,-data_type.y,-drug_cd.y,-drug_nm.y,-amr.y,-pastcode.y,-extract.y,-hosno_dcs.y,-temp_f.y,-cri_1.y,-cri_2.y,
           -cri_3.y,-cri_4.y,-cri_5.y,-cri_6.y,-cri_7.y,-cri_8.y,-cri_9.y,-pt1.y,-pt2.y,-pt3.y,-lk_sn.y,-FI_2.y,-soundex_2.y,    
           -episode_chr_id.y,-no_chr.y,-n_chr.y,-dup_epids.y,-rank.y,-afterCOVID.x,-test_method_description.x,-test_method_description.y,
           -organism_genus_name.x,-organism_genus_name.y)
  
  bound_clean <- bound_clean %>%
    mutate(patient_id = ifelse(is.na(patient_id.y), patient_id.x, patient_id.y),
           spec_dt_backup = spec_dt_backup.x,
           dob_backup = dob_backup.x) %>%
    select(-patient_id.x,-patient_id.y, -spec_dt_backup.x, -spec_dt_backup.y,-dob_backup.x,-dob_backup.y)
  
  # error check
  # make sure that you have no NA in patient_id
  error <- bound_clean %>%
    filter(is.na(patient_id))
  nrow(error)
  rm(error)
  
  bound_clean
}



#_______________________________________________________________________________
# grouped age sex pyramids

cal_pyramid <- function(df) {
  a <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2015) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2015") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  b <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2016) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2016") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  c <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2017) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2017") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  d <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2018) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2018") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  e <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2019) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2019") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  
  f <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2020) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2020") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  g <- df %>%
    filter(organism_species_name == tolower(name), calendar_year == 2021) %>%
    mutate(n = ifelse(sex=="male", n*(-1),
                      n*1)) %>%
    ggplot(aes(x = age_group,y = n, fill=sex)) + 
    geom_bar(stat = "identity", colour="black") +
    coord_flip() +
    labs(title = "2021") +
    theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal() +
    scale_fill_manual(values=c('#FF9999', '#009999'))
  
  
  plot_grid(a, b, c, d, e, f, g, 
            #labels = c('2015', '2016', '2017', '2018', '2019', '2020', '2021'), 
            ncol = 1)
}

#_______________________________________________________________________________
#_______________________________________________________________________________








#_______________________________________________________________________________
# graph of percentage resistant
#_______________________________________________________________________________

drug_chart <- function(bug){
    x <- amr_table1 %>%
    filter(organism_species_name == tolower(bug)) %>%
    select(calendar_year,
           ceph_resistant=ceph.percentage.resistant.no,
           carbs_resistant=carbs.percentage.resistant.no,
           macs_resistant=macs.percentage.resistant.no,
           cip_resistant=cip.percentage.resistant.no,
           gen_resistant=gen.percentage.resistant.no,
           ptz_resistant=ptz.percentage.resistant.no,
           ctz_resistant=ctz.percentage.resistant.no,
           pen_resistant=pen.percentage.resistant.no,
           van_resistant=van.percentage.resistant.no,
           tet_resistant=tet.percentage.resistant.no) %>%
    pivot_longer(!calendar_year, names_to = "class", values_to = "count")
  
ggplot(x,aes(x=calendar_year,y = count, color = class)) +
  geom_point() +
  geom_line(aes(group = class)) +
  theme(legend.position = "bottom") +
  labs(x = "calendar year", y = "% resistant from total tested", title = paste0(name, " % resistant from total tested"))
}

#_______________________________________________________________________________




#_______________________________________________________________________________
# amr raw download collapsed by specimen date
#_______________________________________________________________________________
# 1) generate your patient and episode id
# 2) collapse by worst case for amr susceptibility (drug / patient_id / specimen_id)

sgss_amr_worst_case <- function(amr){
  # set your criteria
  amr <- amr %>%
    mutate(spec_dt = as.Date(spec_dt),
           dob = as.Date(dob),
           lab_cd = as.integer(lab_cd),
           soundex = phonics::soundex(surname),
           FI = toupper(substr(forename,1,1)),
           sex = case_when(toupper(sex) == "MALE" ~ "M",
                           toupper(sex) == "FEMALE" ~ "F",
                           toupper(sex) == "UNKNOWN" ~ "U")) %>%
    mutate(
      cri_1 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA) | dob %in% c(dmy("01/01/1900"),NA), NA, paste(nhsno, dob, sep="")),
      cri_2 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA),NA,nhsno),
      cri_3 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA) | dob %in% c(dmy("01/01/1900"),NA) | soundex %in% c("", NA), NA,paste(hosno,dob,soundex, sep="")),
      cri_4 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,hosno),
      cri_5 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
                       lab_cd %in% c(NA) |
                       soundex %in% c("", NA) |
                       FI %in% c("", NA) |
                       toupper(forename) %in% c("", "UNKNOWN", NA) |
                       sex %in% c("", "U", NA) |
                       dob %in% c(dmy("01/01/1900"),NA),
                     NA,
                     paste(specno,lab_cd,sex,FI,dob, soundex, sep="")
      ),
      cri_6 = ifelse(
        specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
          lab_cd %in% c(NA) |
          FI %in% c("", NA) |
          toupper(forename) %in% c("", "UNKNOWN", NA) |
          sex %in% c("", "U", NA),
        NA,
        paste(specno,lab_cd,sex,FI, sep="")
      ),
      cri_7 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_8 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_9 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,specno),
      # fuzzy dob
      pt1=paste(year(dob), str_pad(month(dob), width = 2, pad = 0), sep=""),
      pt2=paste(year(dob), str_pad(day(dob), width = 2, pad = 0), sep=""),
      pt3=paste(str_pad(day(dob), width = 2, pad = 0), str_pad(month(dob), width = 2, pad = 0), sep="")
    )
  
  
  # develop your id
  amr$lk_sn <- 1:nrow(amr)
  amr$FI_2 <- amr$FI
  amr$soundex_2 <- amr$soundex
  
  # # develop your patient_id
  # amr$patient_id <- diyar::record_group(df = amr, # put in the data
  #                                       sn = lk_sn, # the unique id you created above "amr$lk_sn <- 1:nrow(amr)"
  #                                       criteria = c("cri_1", "cri_2", "cri_3", "cri_4", "cri_5", "cri_6", "cri_7", "cri_8","cri_9"), # list out the chriteria
  #                                       sub_criteria = list(s6 = c("FI","soundex"), s9.1 = c("pt1","pt2","pt3"), s9.2=c("FI_2","soundex_2")), to_s4 = T) # you are also identifying the sub_chriteria
  # 
  # # add in your episode_id
  # amr$episode_id <- diyar::episodes(sn = amr$lk_sn, # select the data
  #                                   date = amr$spec_dt, # isolate the date
  #                                   case_length = 13, # this is the 13 days that make it a group
  #                                   #data_source = amr$extract, # SGSS or DCS
  #                                   strata = diyar::combi(amr$patient_id, amr$organism_species_name),
  #                                   episode_type = "rolling")
  
  
  # what i will be doing is if this occurs keep the one that has a r for resistant
  amr <- amr %>%
    mutate_all(.funs = tolower) %>% # lowercase all your data if it is character class
    mutate(episode_chr_id = as.character(episode_id),
           patient_chr_id = as.character(patient_id)) %>% # make sure your episode id is in character format to add to a key
    mutate(key = paste0(patient_chr_id,"_",episode_chr_id)) %>% # cal addition 11-11-2022
    mutate(key_drug_name = paste0(key,"_",drug_nm)) %>%
    # sort odd amr codes
    mutate(amr = case_when(amr == "" ~ "n",
                           amr == "u" ~ "n",
                           amr == "n/a" ~ "n",
                           amr == "qqq" ~ "n",
                           amr == "xxx" ~ "n",
                           amr == "zzz" ~ "n",
                           TRUE~amr)) %>%
    
    mutate(rank_amr = case_when(amr == "r" ~ 1,
                                amr == "i" ~ 2,
                                amr == "s" ~ 3,
                                amr == "q" ~ 4,
                                amr == "n" ~ 5)) %>%
    arrange(rank_amr) %>% # this will make all the R at the top
    mutate(dup = cal_dupe_to_remove(key_drug_name)) %>% 
    filter(dup == F)
  
  amr <- amr %>%
    # the episode key is unlikely to be needed as each episode takes into consideration the person chriteria along with the episodes. so would stand to reason that it has all the above in it already
    arrange(key,drug_cd) %>% # arrange as follows
    mutate(lab_cd = as.character(lab_cd), # make sure that all labcodes are character so you can get rid of the bad ones
           remove = lab_cd %in% c("254336","254326","254280")) %>% # remove bad lab codes
    filter(remove == F) 
}


#_______________________________________________________________________________
# collapse amr data by the organism to create cal drug code and drug name and amr or not.

sgss_amr_collapse <- function(amr){
  
  amr <- amr %>%
    mutate(cal_drug_cd = paste0(drug_cd,"-",amr),
           cal_drug_nm = paste0(drug_nm,"-",amr)) %>%
    group_by(episode_chr_id) %>% # group by your episode_chr_id
    summarise(cal_drug_cd = paste0(cal_drug_cd, collapse = "_"),
              cal_drug_nm = paste0(cal_drug_nm, collapse = "_"),
              cal_amr = paste0(amr, collapse = "_")) %>% # concatenate together
    ungroup() 
}

#_______________________________________________________________________________


sgss_amr_groupings <- function(amr) {
  
    amr_group <- amr %>%
    mutate(cal_drug_nm = tolower(cal_drug_nm)) %>%
    mutate(ceph.tested = ifelse(str_detect(cal_drug_nm,"cefotaxime"),1,0),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftazidime"),1,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftriaxone"),1,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"cefotaxime-n"),0,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftazidime-n"),0,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftriaxone-n"),0,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"cefotaxime-_"),0,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftazidime-_"),0,ceph.tested),
           ceph.tested = ifelse(str_detect(cal_drug_nm,"ceftriaxone-_"),0,ceph.tested),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"cefotaxime-r"),1,0),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"ceftazidime-r"),1,ceph.resistant),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"ceftriaxone-r"),1,ceph.resistant),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"cefotaxime-i"),1,ceph.resistant),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"ceftazidime-i"),1,ceph.resistant),
           ceph.resistant = ifelse(str_detect(cal_drug_nm,"ceftriaxone-i"),1,ceph.resistant),
           ceph.tested = ifelse(is.na(ceph.tested),0,ceph.tested),
           ceph.resistant = ifelse(is.na(ceph.resistant),0,ceph.resistant)) %>%
    
    mutate(carbs.tested = ifelse(str_detect(cal_drug_nm,"meropenem"),1,0),
           carbs.tested = ifelse(str_detect(cal_drug_nm,"imipenem"),1,carbs.tested),
           carbs.tested = ifelse(str_detect(cal_drug_nm,"meropenem-n"),0,carbs.tested),
           carbs.tested = ifelse(str_detect(cal_drug_nm,"imipenem-n"),0,carbs.tested),
           carbs.tested = ifelse(str_detect(cal_drug_nm,"meropenem-_"),0,carbs.tested),
           carbs.tested = ifelse(str_detect(cal_drug_nm,"imipenem-_"),0,carbs.tested),
           carbs.resistant = ifelse(str_detect(cal_drug_nm,"meropenem-r"),1,0),
           carbs.resistant = ifelse(str_detect(cal_drug_nm,"imipenem-r"),1,carbs.resistant),
           carbs.resistant = ifelse(str_detect(cal_drug_nm,"meropenem-i"),1,carbs.resistant),
           carbs.resistant = ifelse(str_detect(cal_drug_nm,"imipenem-i"),1,carbs.resistant),
           carbs.tested = ifelse(is.na(carbs.tested),0,carbs.tested),
           carbs.resistant = ifelse(is.na(carbs.resistant),0,carbs.resistant)) %>%
    
    mutate(macs.tested = ifelse(str_detect(cal_drug_nm,"clarithromycin"),1,0),
           macs.tested = ifelse(str_detect(cal_drug_nm,"erythromycin"),1,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"azithromycin"),1,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"clarithromycin-n"),0,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"erythromycin-n"),0,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"azithromycin-n"),0,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"clarithromycin-_"),0,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"erythromycin-_"),0,macs.tested),
           macs.tested = ifelse(str_detect(cal_drug_nm,"azithromycin-_"),0,macs.tested),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"clarithromycin-r"),1,0),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"erythromycin-r"),1,macs.resistant),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"azithromycin-r"),1,macs.resistant),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"clarithromycin-i"),1,macs.resistant),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"erythromycin-i"),1,macs.resistant),
           macs.resistant = ifelse(str_detect(cal_drug_nm,"azithromycin-i"),1,macs.resistant),
           macs.tested = ifelse(is.na(macs.tested),0,macs.tested),
           macs.resistant = ifelse(is.na(macs.resistant),0,macs.resistant)) %>%
    
    mutate(cip.tested = ifelse(str_detect(cal_drug_nm, "ciprofloxacin"),1,0),
           cip.tested = ifelse(str_detect(cal_drug_nm,"ciprofloxacin-n"),0,cip.tested),
           cip.tested = ifelse(str_detect(cal_drug_nm,"ciprofloxacin-_"),0,cip.tested),
           cip.resistant = ifelse(str_detect(cal_drug_nm,"ciprofloxacin-r"),1,0),
           cip.resistant = ifelse(str_detect(cal_drug_nm,"ciprofloxacin-i"),1,cip.resistant),
           cip.tested = ifelse(is.na(cip.tested),0,cip.tested),
           cip.resistant = ifelse(is.na(cip.resistant),0,cip.resistant)) %>%
    
    mutate(gen.tested = ifelse(str_detect(cal_drug_nm, "gentamicin"),1,0),
           gen.tested = ifelse(str_detect(cal_drug_nm,"gentamicin-n"),0,gen.tested),
           gen.tested = ifelse(str_detect(cal_drug_nm,"gentamicin-_"),0,gen.tested),
           gen.resistant = ifelse(str_detect(cal_drug_nm,"gentamicin-r"),1,0),
           gen.resistant = ifelse(str_detect(cal_drug_nm,"gentamicin-i"),1,gen.resistant),
           gen.tested = ifelse(is.na(gen.tested),0,gen.tested),
           gen.resistant = ifelse(is.na(gen.resistant),0,gen.resistant)) %>%
    
    mutate(ptz.tested = ifelse(str_detect(cal_drug_nm, "piperacillin/tazobactam"),1,0),
           ptz.tested = ifelse(str_detect(cal_drug_nm,"piperacillin/tazobactam-n"),0,ptz.tested),
           ptz.tested = ifelse(str_detect(cal_drug_nm,"piperacillin/tazobactam-_"),0,ptz.tested),
           ptz.resistant = ifelse(str_detect(cal_drug_nm,"piperacillin/tazobactam-r"),1,0),
           ptz.resistant = ifelse(str_detect(cal_drug_nm,"piperacillin/tazobactam-i"),1,ptz.resistant),
           ptz.tested = ifelse(is.na(ptz.tested),0,ptz.tested),
           ptz.resistant = ifelse(is.na(ptz.resistant),0,ptz.resistant)) %>%
    
    mutate(ctz.tested = ifelse(str_detect(cal_drug_nm, "ceftazidime"),1,0),
           ctz.tested = ifelse(str_detect(cal_drug_nm,"ceftazidime-n"),0,ctz.tested),
           ctz.tested = ifelse(str_detect(cal_drug_nm,"ceftazidime-_"),0,ctz.tested),
           ctz.resistant = ifelse(str_detect(cal_drug_nm,"ceftazidime-r"),1,0),
           ctz.resistant = ifelse(str_detect(cal_drug_nm,"ceftazidime-i"),1,ctz.resistant),
           ctz.tested = ifelse(is.na(ctz.tested),0,ctz.tested),
           ctz.resistant = ifelse(is.na(ctz.resistant),0,ctz.resistant)) %>%
    
    mutate(pen.tested = ifelse(str_detect(cal_drug_nm, "penicillin"),1,0),
           pen.tested = ifelse(str_detect(cal_drug_nm,"penicillin-n"),0,pen.tested),
           pen.tested = ifelse(str_detect(cal_drug_nm,"penicillin-_"),0,pen.tested),
           pen.resistant = ifelse(str_detect(cal_drug_nm,"penicillin-r"),1,0),
           pen.resistant = ifelse(str_detect(cal_drug_nm,"penicillin-i"),1,pen.resistant),
           pen.tested = ifelse(is.na(pen.tested),0,pen.tested),
           pen.resistant = ifelse(is.na(pen.resistant),0,pen.resistant)) %>%
    
    mutate(van.tested = ifelse(str_detect(cal_drug_nm, "vancomycin"),1,0),
           van.tested = ifelse(str_detect(cal_drug_nm,"vancomycin-n"),0,van.tested),
           van.tested = ifelse(str_detect(cal_drug_nm,"vancomycin-_"),0,van.tested),
           van.resistant = ifelse(str_detect(cal_drug_nm,"vancomycin-r"),1,0),
           van.resistant = ifelse(str_detect(cal_drug_nm,"vancomycin-i"),1,van.resistant),
           van.tested = ifelse(is.na(van.tested),0,van.tested),
           van.resistant = ifelse(is.na(van.resistant),0,van.resistant)) %>%
    
    mutate(tet.tested = ifelse(str_detect(cal_drug_nm, "tetracycline"),1,0),
           tet.tested = ifelse(str_detect(cal_drug_nm,"tetracycline-n"),0,tet.tested),
           tet.tested = ifelse(str_detect(cal_drug_nm,"tetracycline-_"),0,tet.tested),
           tet.resistant = ifelse(str_detect(cal_drug_nm,"tetracycline-r"),1,0),
           tet.resistant = ifelse(str_detect(cal_drug_nm,"tetracycline-i"),1,tet.resistant),
           tet.tested = ifelse(is.na(tet.tested),0,tet.tested),
           tet.resistant = ifelse(is.na(tet.resistant),0,tet.resistant))
}

#________________________________________________________________________________________________________

strep_groups <- function(){

organism_species_name <- c("abiotrophia defectiva", "abiotrophia other named","abiotrophia sp","aerococcus christensenii","aerococcus other named","aerococcus sanguinicola","aerococcus sp","aerococcus urinae","aerococcus viridans","anaerococcus (peptostreptococcus) prevotii","gemella bergeri","gemella haemolysans",                                     
"gemella morbillorum","gemella other named","gemella sanguinis","gemella sp","globicatella sanguis","globicatella sp","globicatella sulfidifaciens","leuconostoc citreum","leuconostoc lactis","leuconostoc mesenteroides","leuconostoc pseudomesenteroides","leuconostoc sp",
"nutritionally variant streptococci","pediococcus acidilactici","pediococcus other named","pediococcus pentosaceus","pediococcus sp","peptoniphilus harei (peptostreptococcus harei)","peptostreptococcus anaerobius","peptostreptococcus asaccharolyticus",
"peptostreptococcus canis","peptostreptococcus magnus","peptostreptococcus other named","peptostreptococcus productus","peptostreptococcus sp","peptostreptococcus stomatis","streptococcus australis","streptococcus constellatus subsp constellatus",
"streptococcus constellatus subsp pharyngis","streptococcus devriesei","streptococcus halichoeri","streptococcus hyointestinalis","streptococcus infantarius","streptococcus infantarius subsp. infantarius","streptococcus iniae","streptococcus massiliensis",
"streptococcus minor","streptococcus minutus","streptococcus parauberis","streptococcus pleomorphus","streptococcus pseudopneumoniae","streptococcus pseudoporcinus","streptococcus sinensis","streptococcus tigurinus","streptococcus urinalis","streptococcus acidominimus","streptococcus agalactiae","streptococcus alactolyticus",
"streptococcus alpha haemolytic (streptococcus viridans)","streptococcus viridans","streptococcus anaerobic","streptococcus anginosus (group a)","streptococcus anginosus (group c)","streptococcus anginosus (group f)","streptococcus anginosus (group g)","streptococcus anginosus (ungroupable)",
"streptococcus anginosus (ungrouped)","streptococcus beta haemolytic not further specified","streptococcus bovis biotype ii","streptococcus bovis untyped","streptococcus canis","streptococcus constellatus (group a)","streptococcus constellatus (group c)","streptococcus constellatus (group f)",
"streptococcus constellatus (ungroupable)","streptococcus constellatus (ungrouped)","streptococcus cristatus","streptococcus dysgalactiae","streptococcus dysgalactiae subsp dysgalactiae","streptococcus dysgalactiae var equisimilis","streptococcus equi","streptococcus equinus",
"streptococcus equisimilis","streptococcus gallolyticus (streptococcus bovis biotype i)","streptococcus gallolyticus subsp. gallolyticus","streptococcus gallolyticus subsp gallolyticus",
"streptococcus gordonii","streptococcus group a","streptococcus group b","streptococcus group c","streptococcus group d","streptococcus group f","streptococcus group g","streptococcus infantarius subsp coli","streptococcus infantarius subsp nov","streptococcus infantis",
"streptococcus intermedius","streptococcus lutetiensis","streptococcus mitior","streptococcus mitis 1 or i","streptococcus mitis 2 or ii","streptococcus mitis group","streptococcus mutans","streptococcus non-haemolytic",
"streptococcus oralis" ,"streptococcus other group (not a-d,f,g)","streptococcus other named","streptococcus ovis","streptococcus parasanguinis","streptococcus pasteurianus","streptococcus peroris","streptococcus pluranimalium",
"streptococcus pneumoniae","streptococcus porcinus","streptococcus pyogenes","streptococcus saccharolyticus","streptococcus salivarius","streptococcus sanguinis 1 or i","streptococcus sanguinis 2 or ii","streptococcus sanguinis group",
"streptococcus sobrinus","streptococcus sp","streptococcus suis type 1 (group s)","streptococcus suis type 2 (group r)","streptococcus suis untyped","streptococcus thermophilus","streptococcus thoraltensis","streptococcus uberis",
"streptococcus vestibularis","streptococcus zooepidemicus","streptococcus milleri")

strep_group <- c("closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","mitis group","anginosus group","anginosus group","mutans group",
"other streptococci","other streptococci","bovis group","bovis group","other streptococci","sanguinis group",
"other streptococci","other streptococci","other streptococci","other streptococci","mitis group","other streptococci",
"other streptococci","mitis group","other streptococci","other streptococci","streptococcus group b","bovis group",
"other streptococci","other streptococci","other streptococci","anginosus group","anginosus group","anginosus group",
"anginosus group","anginosus group","anginosus group","other streptococci","bovis group","bovis group",
"streptococcus group g","anginosus group","anginosus group","anginosus group","anginosus group","anginosus group",
"mitis group","streptococcus group c","streptococcus group c","streptococcus group c","streptococcus group c","bovis group",
"streptococcus group c","bovis group","bovis group","bovis group","sanguinis group","streptococcus group a",
"streptococcus group b","streptococcus group c","excluded from report","anginosus group","streptococcus group g","bovis group",
"bovis group","mitis group","anginosus group","bovis group","mitis group","mitis group","mitis group","mitis group","mutans group",
"other streptococci","mitis group","other streptococci","other streptococci","other streptococci","sanguinis group",
"bovis group","other streptococci","other streptococci","excluded from report (has it's own report)","other streptococci","streptococcus group a",
"excluded from report","salivarius group","sanguinis group","sanguinis group","sanguinis group","mutans group",
"other streptococci","other streptococci","other streptococci","other streptococci","other streptococci","other streptococci",
"other streptococci","salivarius group","streptococcus group c","anginosus group")

df <- data.frame(organism_species_name,strep_group)

}


#________________________________________________________________________________________________________


strep_groups_cosecco <- function(){
  
organism_species_name <- c("abiotrophia spp.","aerococcus spp.","peptostreptococcus spp.","gemella spp.","globicatella sanguinis","globicatella spp.","globicatella sulfidifaciens","leuconostoc spp.",
"closely related genera","pediococcus spp.","streptococcus constellatus","streptococcus infantarius","streptococcus infantarius sp infantarius","streptococcus massiliensis","streptococcus pseudopneumoniae","streptococcus sinensis",
"streptococcus acidominimus","group b","streptococcus alactolyticus","streptococci not fully identified","anaerobic streptococcus'","streptococcus anginosus","streptococcus gallolyticus","streptococcus bovis untyped",
"group g","streptococcus cristatus","group c","streptococcus equinus","streptococcus bovis biotype i","streptococcus gordonii","group a","group d","streptococcus group f","streptococcus lutetiensis","streptococcus infantarius sp nov","streptococcus intermedius",
"streptococcus mitior","streptococcus mitis","streptococcus mutans","streptococcus oralis","streptococcus parasanguinis","streptococcus pasteurianus","streptococcus pneumoniae","streptococcus group d",
"streptococcus salivarius","streptococcus sanguinis","streptococcus sobrinus","streptococcus suis","streptococcus uberis","streptococcus vestibularis","streptococcus milleri group")

strep_group <- c("closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera","closely related genera",
"closely related genera","closely related genera","anginosus group","bovis group","bovis group","sanguinis group","mitis group","other streptococci","other streptococci","streptococcus group b","bovis group","other streptococci",
"other streptococci","anginosus group","bovis group","bovis group","streptococcus group g","mitis group","streptococcus group c","bovis group","bovis group","sanguinis group","streptococcus group a","excluded from report",
"anginosus group","bovis group","bovis group","anginosus group","mitis group","mitis group","mutans group","mitis group","sanguinis group","bovis group","excluded from report (has it's own report)","excluded from report",
"salivarius group","sanguinis group","mutans group","other streptococci","other streptococci","salivarius group","anginosus group")
  
df <- data.frame(organism_species_name,strep_group)

}


#________________________________________________________________________________________________________

sgss_species_update <- function() {
  
  organism_species_name <- c("alcaligenes piechaudii","alcaligenes xylosoxidans","alcaligenes denitrificans","chryseomonas luteola","flavimonas oryzihabitans","comamonas acidovorans",
                             "chryseobacterium meningosepticum","chryseobacterium miricola","aggregatibacter (haemophilus) segnis","anaerococcus (peptostreptococcus) prevotii","calymmatobacterium granulomatis","calymmatobacterium sp",
                             "chryseomonas sp","eubacterium aerofaciens","flavobacterium odoratum","gordonia bronchialis (rhodococcus bronchialis)","leptospira hardjo","leptospira icterohaemorrhagiae",
                             "psychrobacter phenylpyruvicus (moraxella phenylpyruvica)","rhodococcus equi (corynebacterium equi)","hewanella putrefaciens (pseudomonas putrefaciens)",
                             "stomatococcus mucilaginosus","stomatococcus sp","vibrio hollisae","wautersia sp","streptococcus group d","enterobacter agglomerans (pantoea agglomerans)",
                             "eubacterium lentum","eubacterium lenta","propionibacterium acnes","actinotingnum sanguinis","enterobacter aerogenes","eubacterium moniliforme",
                             "raoutella ssp","chryseomonas other named","achromobacter dentrificans","anaerobiospirllium thomasii","herbasprillum huttiense","murdochiella assaccharolytica",
                             "neisseria polysacchareae","ochrobactrium tritici","pseudomonas monteilli","rothia dentocariosia","bordetella holmeslii","mixta mixta calida",
                             "acinetobacter baumannii","acinetobacter calcoaceticus (anitratus)","acinetobacter pittii","acinetobacter nosocomialis","acinetobacter calcoaceticus","acinetobacter dijkshoorniae",
                             "citrobacter diversus","enterobacter cloacae","enterobacter hormaechei","enterobacter kobei","enterobacter cloacae complex","enterobacter cloacae dissolvens",
                             "enterobacter xiangfangensis","enterobacter asburiae","enterobacter bugandensis","enterobacter intermedius","enterobacter cloacae subsp cloacae","enterobacter ludwigii",
                             "enterobacter gergoviae","enterobacter sakazakii","enterobacter cancerogenus","klebsiella ornithinolytica","klebsiella pneumoniae subsp pneumoniae","klebsiella pneumoniae subsp ozenae",
                             "pseudomonas azotoformans","pseudomonas chlororaphis","pseudomonas corrugata","pseudomonas extremorientalis","pseudomonas fluorescens","pseudomonas fragi",
                             "pseudomonas fulva","pseudomonas libanensis","pseudomonas lundensis","pseudomonas luteola","pseudomonas mandelii","pseudomonas mosselii",
                             "pseudomonas oryzihabitans","pseudomonas paucimobilis","pseudomonas paucimobilis (sphingomonas paucimobilis)","pseudomonas plecoglossicida","pseudomonas putida","pseudomonas rhodesiae",
                             "pseudomonas stutzeri","pseudomonas synxantha","pseudomonas taetrolens","pseudomonas veronii","rhizopus arrhizus (rhizopus oryzae)","serratia marcescens",
                             "serratia liquefaciens","serratia ureilytica","serratia grimesii","serratia ficaria","alcaligenes xylosoxidans xylosoxidans","agrobacterium tumefaciens",
                             "psychrobacter phenylpyruvicus","other streptococci","peptococcus sp","pseudomonas monteilii","citrobacter koseri","enterococcus faecalis",
                             "providencia rettgeri","pseudomonas oryzihabitans (flavimonas oryzihabitans)","granulicatella adiacens (abiotrophia adjacens)(strep adjacens)",
                             "actinotignum schaalii","actinobaculum schaalii" )
  
  new_name <- c()
  
}








#________________________________________________________________________________________________________

# renaming function for sgss data

standard_names_sgss <- function(df) {
  df <- df %>%
    select(organism = organism_species_name,
           nhsno = patient_nhs_number, 
           spec_dt = date, 
           forename = patient_forename, 
           surname = patient_surname, 
           postcode = patient_postcode,
           lab_nm = laboratorywherespecimenprocessed, 
           specno = specimen_number, 
           lab_cd = lab_geography_code)
}

#________________________________________________________________________________________________________

# class variables in sgss

class_sgss <- function(df) {
  df <- df %>%
    mutate(date = as.Date(date),
           dob = as.Date(dob),
           lab_geography_code = as.integer(lab_geography_code),
           soundex = phonics::soundex(surname),
           FI = toupper(substr(forename,1,1)),
           sex = case_when(toupper(sex) == "MALE" ~ "M",
                           toupper(sex) == "FEMALE" ~ "F",
                           toupper(sex) == "UNKNOWN" ~ "U"))
  }

#________________________________________________________________________________________________________





    
#________________________________________________________________________________________________________

# Backfill function ----
# i have removed pid sections at the bottom

# this can just be done with the fill function in R 
# x <- x %>%
#   group_by(id) %>%
#   fill(backfill_element, .direction = "downup")

cal_backfill_string <- function(df, id, backfill_element) {
  
  # first thing we have too do is turn all "NA" or "" into true NA within the whole dataset
  #df[df == "" | df == "NA"] <- NA
  
  # generate your backup that you will be wanting to left join into
  df_backup <- df %>%
    mutate(id_var = {{id}})
  
  
  filtered <- df %>%
    # this will generate only those where the backfill_element and have id and the element
    filter(
      {{backfill_element}} != "NA" |
            {{backfill_element}} != "" | 
             !is.na({{backfill_element}})) %>%
    mutate(new_var = {{backfill_element}}) %>%
    select({{id}}, new_var) %>%
    mutate(id_var = {{id}}) %>%
    filter(new_var != "") %>%
    unique() %>%
    # there may be slightly differing backfill_elements so there will be duplicate id_var, here we will select the larger of the two and unique it again. 
    mutate(n = nchar(new_var),
           dupe = cal_is_duplicated(id_var)) %>%
    arrange(desc(n)) %>%
    mutate(to_remove = cal_dupe_to_remove(id_var)) %>%
    filter(to_remove != TRUE) %>%
    select(-dupe,-to_remove) %>%
    unique()
  
  # finally you will left_join to the backup by id_var creating a new variable called id_var.
  join <- left_join(df_backup, filtered, by = "id_var") %>%
    # now we replace the name of the backfill_element i.e. forename with whatever is in the new_var and that is that.
    mutate({{backfill_element}} := new_var) %>%
    #select(-n,-new_var,-{{id_var}},-pid.y) %>%
    #rename(pid = pid.x) %>%
    unique()
  
 
  # renaming section
  .y <- grep("\\.y$", names(join), value = TRUE)
  .x <- grep("\\.x$", names(join), value = TRUE)
  new_col_names <- gsub("\\.x", "", .x)
  
  # remove and rename
  join <- join %>%
    select(-.y, -new_var)
  
  join <- join %>%
    rename_with(~new_col_names, .cols = all_of(.x))
  
  return(join)
  #return(cols_to_select)
}





# # EXAMPLE 
#  x <- df %>%
#    cal_backfill(id = id, backfill_element = Patient_Forename) 




#________________________________________________________________________________________________________

  
#_______________________________________________________________________________________________________________________________
# Backfill function for dates ----
#_______________________________________________________________________________________________________________________________
# i have removed pid sections at the bottom
cal_backfill_date <- function(df, id, backfill_element) {
  
  # generate your backup that you will be wanting to left join into
  df_backup <- df %>%
    mutate(id_var = {{id}})
  
  
  filtered <- df %>%
    # this will generate only those where the backfill_element and have id and the element
    filter(
      #{{backfill_element}} != "NA" |
       # {{backfill_element}} != "" | 
        !is.na({{backfill_element}})) %>%
    mutate(new_var = {{backfill_element}}) %>%
    select({{id}}, new_var) %>%
    mutate(id_var = {{id}}) %>%
    filter(new_var != "") %>%
    unique() %>%
    # there may be slightly differing backfill_elements so there will be duplicate id_var, here we will select the larger of the two and unique it again. 
    mutate(n = nchar(new_var),
           dupe = cal_is_duplicated(id_var)) %>%
    arrange(desc(n)) %>%
    mutate(to_remove = cal_dupe_to_remove(id_var)) %>%
    filter(to_remove != TRUE) %>%
    select(-dupe,-to_remove) %>%
    unique()
  
  # finally you will left_join to the backup by id_var creating a new variable called id_var.
  join <- left_join(df_backup, filtered, by = "id_var") %>%
    # now we replace the name of the backfill_element i.e. forename with whatever is in the new_var and that is that.
    mutate({{backfill_element}} := new_var) %>%
    #select(-n,-new_var,-{{id_var}},-pid.y) %>%
    #rename(pid = pid.x) %>%
    unique()
  
  return(join)
}
#_______________________________________________________________________________________________________________________________
 
 






#_______________________________________________________________________________________________________________________________
# convert df "" and "NA" too a true NA
#_______________________________________________________________________________________________________________________________

# cal_convert_to_na <- function(data) {
#   data[data == "" | data == "NA" | data == "n/a" | data == "na"] <- NA
#   return(data)
# }
cal_convert_to_na <- function(data) {
  for (col in names(data)) {
    if (is.character(data[[col]])) {
      data[[col]][data[[col]] == "" | data[[col]] == "NA"] <- NA
    }
  }
  return(data)
}

# example of its usage
#df <- cal_convert_to_na(df)
#_______________________________________________________________________________________________________________________________














 
#.............................................
# Generate stata odds ratio with chi2 ----
#.............................................


# # Example to show you how it works
# df <- read_dta("F:/Projects & programmes/COVID-19/HPRU impact of COVID/8) total_script/XOXO - Final Logic/14) Analysis/1.1) annual CFR/data output/6) odds chi/pathogens_of_interest_by_episode_simple_filtered_cov.dta")
# df <- df %>%
#   mutate(period = ifelse(calendar_year <= 2019,0,1),
#          period = ifelse(calendar_year_month %in% c("2020_01","2020_02"),0,period))
# 
# # try with the bovis
# bovis <- df %>% filter(organism_species_name == "bovis group")


generate_two_by_two_table <- function(line_list_data, exposure_var, outcome_var) {
  # Calculate the two by two table
  my_table <- table(bovis[["period"]], bovis[["outcome"]])
  my_table <- table(line_list_data[[exposure_var]], line_list_data[[outcome_var]])
  
  # Calculate the total count
  total_count <- sum(my_table)
  
  # Calculate the risk for cases and non-cases in each exposure group
  risk_unexposed_cases <- my_table[1, 2] / sum(my_table[1, ])
  risk_unexposed_noncases <- my_table[1, 1] / sum(my_table[1, ])
  risk_exposed_cases <- my_table[2, 2] / sum(my_table[2, ])
  risk_exposed_noncases <- my_table[1, 2] / sum(my_table[1, ])
  
  # Create the dataframe with the 2x2 table and risk
  result_table <- data.frame(
    "Cases" = c(my_table[2, 2], my_table[1, 2], my_table[2, 2]+my_table[1, 2]),
    "Noncases" = c(my_table[2, 1], my_table[1, 1], my_table[2, 1]+my_table[1, 1]),
    "Total" = c(sum(my_table[2, ]), sum(my_table[1, ]), total_count),
    "Risk" = c(risk_exposed_cases, risk_unexposed_cases, sum(my_table[2, ]) / total_count),
    row.names = c("Exposed", "Unexposed", "Total")
  )
  
  # Calculate additional statistics
  risk_diff <- risk_exposed_cases - risk_unexposed_cases
  risk_ratio <- risk_exposed_cases / risk_unexposed_cases
  attr_frac_exposed <- (risk_exposed_cases - risk_unexposed_cases) / risk_exposed_cases
  attr_frac_pop <- (risk_exposed_cases - risk_unexposed_cases) / (risk_exposed_cases * total_count)
  odds_ratio <- (risk_exposed_cases / (1 - risk_exposed_cases)) / (risk_unexposed_cases / (1 - risk_unexposed_cases))
  
  # Add additional statistics to the result_table
  result_table <- cbind(result_table, 
                        "Risk difference" = risk_diff,
                        "Risk ratio" = risk_ratio,
                        "Attr. frac. ex." = attr_frac_exposed,
                        "Attr. frac. pop" = attr_frac_pop,
                        "Odds ratio" = odds_ratio)
  
  # Calculate the chi-squared statistic and its p-value using Fisher's exact test
  chi_sq <- chisq.test(my_table)
  result_table$Chi_sq <- chi_sq$statistic
  result_table$`Pr>chi2` <- chi_sq$p.value
  
  return(result_table)
}


# # Example of using the function
# my_table <- generate_two_by_two_table(bovis, "period", "outcome")
# print(my_table)
#________________________________________________________________________________________________________

 

















#...............................................................
# Function to put glm into a table and then explain the output ---- 
#...............................................................

# Load required packages
library(dplyr)

# Define the function
generate_glm_summary_table <- function(model) {
  # Get the summary of the model
  summary_table <- summary(model)$coefficients %>%
    as.data.frame() %>%
    rownames_to_column(var = "Variable") %>%
    rename(Coefficient = Estimate, Std_Error = `Std. Error`, 
           Z_value = `z value`, Pvalue = `Pr(>|z|)`) %>%
    mutate(Exponentiate = exp(Coefficient),
           Significance = ifelse(Pvalue < 0.05, "<0.05","not significant")) %>%
    select(Variable,Coefficient,Exponentiate,Std_Error,Z_value,Pvalue,Significance)
  
  # Return the summary table data frame
  return(summary_table)
}



# summary variable 
generate_glm_explanation <- function(summary_df) {
  explanation <- "Summary of Generalized Linear Model:\n\n"
  
  for (i in 1:nrow(summary_df)) {
    variable <- summary_df$Variable[i]
    coefficient <- summary_df$Coefficient[i]
    exponentiate <- summary_df$Exponentiate[i]
    std_error <- summary_df$Std_Error[i]
    z_value <- summary_df$Z_value[i]
    p_value <- summary_df$Pvalue[i]
    
    explanation <- paste0(
      explanation,
      "For the predictor variable '", variable, "':\n",
      "- The coefficient is ", round(coefficient, 4), ", indicating ",
      "the change in the log-odds of the response for a unit change in this predictor.\n",
      "- The exponentiated coefficient is ", round(exponentiate, 4), ", which implies that a one-unit increase in the predictor ",
      "is associated with a ", round((exponentiate - 1) * 100, 2), "% increase in the odds of the response.\n",
      "- The standard error of the coefficient estimate is ", round(std_error, 4), ", reflecting the uncertainty in the estimate.\n",
      "- The z-value is ", round(z_value, 4), " with a p-value of ", format.pval(p_value, digits = 3), ". ",
      "This indicates that the predictor's effect is ", ifelse(p_value < 0.05, "statistically significant.", "not statistically significant."), "\n\n"
    )
  }
  
  return(explanation)
}

#...............................................................................
# # Example usage
# # Generate example data
# set.seed(123)
# your_data <- data.frame(
#   y = sample(c(0, 1), 100, replace = TRUE),
#   x1 = rnorm(100),
#   x2 = rnorm(100)
# )
# 
# # generate the model
# # Fit the glm model
# my_model <- glm(y ~ x1 + x2, data = your_data, family = "binomial")
#
# summary_df <- generate_glm_summary_table(my_model)
# explanation_text <- generate_glm_explanation(summary_df)
# cat(explanation_text)
#...............................................................................










#...............................................................................
# combine the rows  ---- 
#...............................................................................

combineRows <- function(data, id_var = "id", combined_var = "outcome") {
  # Group the data by the specified "id" variable
  grouped_data <- split(data, data[[id_var]])
  # Combine the rows for each group
  combined_data <- lapply(grouped_data, function(group) {
    # Combine the specified "combined" variable
    group[[combined_var]][group[[combined_var]] == "na"] <- group[[combined_var]][group[[combined_var]] != "na"]
    return(group[1, ])  # Take the first row as the combined row
  })
  # Combine the results back into a data frame
  combined_df <- do.call(rbind, combined_data)
  return(combined_df)
}


# gen an example
df <- data.frame(id = c(1, 1),sex = c("m", "m"),outcome = c("na", "1"), thing = c("qer","fdf"))

# test out the function
result <- combineRows(df, id_var = "id", combined_var = "outcome")

#...............................................................................









#...............................................................................
# how many variables in one df column is present within another df column
#...............................................................................

mergeDataframes <- function(df1, df1var = "names", df2, df2var = "names") {
  # Create a new dataframe with a binary variable
  new_df <- data.frame(
    df1var = df1[[df1var]],
    df2var = df1[[df2var]],
    is_present = ifelse(df1[[df1var]] %in% df2[[df2var]], 1, 0)
  )
  return(new_df)
}

# Example data
# df1 <- data.frame(
#   names = c("ben", "ben", "steve", "suzan", "susan", "bob"),
#   dead = c(0, 0, 0, 0, 1, 0),
#   cab = c("a", "s", "d", "f", "g", "g")
# )
# 
# df2 <- data.frame(
#   names = c("ben", "pete", "bob", "susan"),
#   skincomm = c(1, 1, 1, 1)
# )

# Call the function with the example data
# result_df <- mergeDataframes(df1, df1var = "names", df2, df2var = "names")

# Print the result
# print(result_df)

#...............................................................................




#...............................................................................
# Gen the worst case scinario of anything ----
#...............................................................................

# Example usage:
df <- data.frame(
  id = c(1, 1, 1, 1, 2, 2),
  amr = c("a : sensitive", "b : sensitive", "b : neg", "c : neg", "a : sensitive", "b : neg"),
  ther = c(1,2,3,5,6,7)
)

separate_amr <- function(data, amr_column_name, first_col_name = "name", second_col_name = "name", seperator = "") {
  # Load the tidyr package
  if (!require(tidyr)) {
    install.packages("tidyr")
    library(tidyr)
  }
  
  # Separate the "amr" variable into specified column names
  data <- data %>%
    separate({{ amr_column_name }}, into = c({{ first_col_name }}, {{ second_col_name }}), sep = seperator)
  
  return(data)
}


result <- separate_amr(df, amr, first_col_name = "drug", second_col_name = "sensitivity", seperator = " : ")
#print(result)

rm(df,result)
#...............................................................................







#...............................................................................
# Worst case scenario ----
#...............................................................................

# here we will generate a worst case scenario the de-dupe on

# get your test dataset
# df <- data.frame(
#   id = c(1, 1, 1, 1, 2, 2),
#   drug = c("a", "b", "b", "c", "a", "a"),
#   sensitivity = c("sensitive", "sensitive", "neg", "neg", "sensitive", "neg"),
#   there = c(1, 2, 3, 5, 6, 7)
# )

# get your function
filter_duplicates <- function(data, group_vars, filter_var, keep_value = "sensitive") {
  if (!keep_value %in% c("sensitive", "neg")) {
    stop("Invalid 'keep_value' argument. Please specify 'sensitive' or 'neg'.")
  }
  
  data %>%
    group_by(across(all_of(group_vars))) %>%
    filter(!(across(all_of(filter_var)) == "neg" & any(across(all_of(filter_var)) == keep_value)))
}

# apply your function
# result_sensitive <- filter_duplicates(df, group_vars = c("id", "drug"), filter_var = "sensitivity", keep_value = "sensitive")

# print out the result
# print(result_sensitive)

#...............................................................................


















#...............................................................................
# Condense a variable into one cell ----
#...............................................................................

# gen the datafram
# df <- data.frame(
#   id = c(1, 1, 1, 2),
#   drug = c("a:sensitive", "b:sensitive", "c:neg", "a:sensitive"),
#   sensitivity = c("sensitive", "sensitive", "neg", "sensitive"),
#   there = c(1, 2, 5, 6)
# )

# generate the function
condense_drug <- function(data) {
  data %>%
    group_by(id) %>%
    summarize(drug = paste(drug, collapse = " : "), sensitivity = first(sensitivity), there = first(there))
}


# quick easy version that is reproducable
cal_condense <- function(data, grouped_var, var_to_collapse = x, new_name = y) {
  data %>%
    group_by({{grouped_var}}) %>%
    #group_by(across(all_of(c(...)))) %>%
    summarize(!!new_name := paste0(!!sym(var_to_collapse), collapse = " : "))
}

# # Create a sample dataframe
# data <- data.frame(
#   Group = c("A", "A", "B", "B", "C"),
#   Value = c("1", "2", "3", "4", "5")
# )
# 
# # Test the function
# result <- cal_condense(data, Group, var_to_collapse = "Value", new_name = "CollapsedValue")
# print(result)















# apply the function
# result <- condense_drug(df)
# print(result)

# "Disclamer"
# this works however please know the "there" variable will be chopped also

#...............................................................................


#...............................................................................
# BACKFILL function
#...............................................................................

cal_fill_missing_values <- function(data, group_var, update_var) {
  filled_data <- data %>%
    group_by({{group_var}}) %>%
    fill({{update_var}}, .direction = "downup") %>%
    ungroup()
  
  return(filled_data)
}

# # Example usage:
# data <- data.frame(
#   key = c(1, 1, 1),
#   pre = c("pre", NA, NA),
#   co = c(NA, "co", NA),
#   sec = c(NA, NA, "sec")
# )
# 
# result <- fill_missing_values(data, group_var = key, update_var = pre)
# print(result)

#...............................................................................






#...............................................................................
# Condense rows into a single cell ----
#...............................................................................
library(dplyr)

condense_data <- function(data, group_by_var, var_to_condense) {
  data %>% group_by()
}

#...............................................................................




#...............................................................................
# UKHSA gen criteria ----
#...............................................................................

# # variables needed
# # this is the bare min of what we need in our demographics data
# spec_dt # date specimen was taken
# specno # this is specimen number
# lab_cd # an identifier as to what lab processed the sample
# nhsno # patients nhs number that may or may not be complete
# surname # can be complete or incomplete
# forename # can be complete or incomplete
# dob # date-of-birth
# sex # can be complete or incomplete.
# soundex <- phonics::soundex(surname)
# FI <- toupper(substr(forename,1,1))


# sgss episode id
sgss_episode_criteria <- function(df, nhsno,
                          spec_dt, 
                          dob, 
                          sex,
                          lab_cd, 
                          specno, 
                          hosno,
                          forename,
                          surname) {
  # set your criteria
  df <- df %>%
    mutate(spec_dt = as.Date(spec_dt),
           dob = as.Date(dob),
           lab_cd = as.integer(lab_cd),
           soundex = phonics::soundex(surname),
           FI = toupper(substr(forename,1,1)),
           sex = case_when(toupper(sex) == "MALE" ~ "M",
                           toupper(sex) == "FEMALE" ~ "F",
                           toupper(sex) == "UNKNOWN" ~ "U")) %>%
    mutate(
      cri_1 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA) | dob %in% c(dmy("01/01/1900"),NA), NA, paste(nhsno, dob, sep="")),
      cri_2 = ifelse(nhsno %in% c(".","","9999999999","0","123456789","9876543210",NA),NA,nhsno),
      cri_3 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA) | dob %in% c(dmy("01/01/1900"),NA) | soundex %in% c("", NA), NA,paste(hosno,dob,soundex, sep="")),
      cri_4 = ifelse(hosno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,hosno),
      cri_5 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
                       lab_cd %in% c(NA) |
                       soundex %in% c("", NA) |
                       FI %in% c("", NA) |
                       toupper(forename) %in% c("", "UNKNOWN", NA) |
                       sex %in% c("", "U", NA) |
                       dob %in% c(dmy("01/01/1900"),NA),
                     NA,
                     paste(specno,lab_cd,sex,FI,dob, soundex, sep="")
      ),
      cri_6 = ifelse(
        specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |
          lab_cd %in% c(NA) |
          FI %in% c("", NA) |
          toupper(forename) %in% c("", "UNKNOWN", NA) |
          sex %in% c("", "U", NA),
        NA,
        paste(specno,lab_cd,sex,FI, sep="")
      ),
      cri_7 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_8 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA) |dob %in% c(dmy("01/01/1900"),NA),NA,paste(specno,dob, sep="")),
      cri_9 = ifelse(specno %in% c("","NO PATIENT ID","UNKNOWN",NA),NA,specno),
      # fuzzy dob
      pt1=paste(year(dob), str_pad(month(dob), width = 2, pad = 0), sep=""),
      pt2=paste(year(dob), str_pad(day(dob), width = 2, pad = 0), sep=""),
      pt3=paste(str_pad(day(dob), width = 2, pad = 0), str_pad(month(dob), width = 2, pad = 0), sep="")
    )
}


# you need to add this bit / by adding in the and constructing the variables needed for episode_id and patient_id
# df$lk_sn <- 1:nrow(df)
# df$FI_2 <- df$FI
# df$soundex_2 <- df$soundex

#.....................................................................................................................................















#......................................................................................................................................





# GLM FUNCTIONS XXXXXXXXXXXXX----

#......................................................................................................................................
# Generate a complete glm table for poisson and negbinomial----
#......................................................................................................................................

# Explanation: this can be used for poisson regression analysis and negative binomial as the wording is for that specifically

tabulate_PoissonNegBinomial <- function(df) {
  
  estimate <- as.data.frame(coef(df))
  estimate <- estimate[!is.na(estimate[, 1]), ]
  
  exp <- as.data.frame(exp(coef(df)))
  exp <- exp[!is.na(exp[, 1]), ]
  
  exp_ci <- as.data.frame(exp(confint(df)))
  exp_ci <- exp_ci[!is.na(exp_ci[, 1]), ]
  
  coefficients <- coef(summary(df))
  pval <- as.data.frame(coefficients[, "Pr(>|z|)"])
  bound <- cbind(estimate,exp,exp_ci,pval)
  bound
  
  # rename variables
  colnames(bound)[1] <- "estimate"
  colnames(bound)[2] <- "exp"
  colnames(bound)[5] <- "pval"
  
  # renaming convention
  bound <- bound %>%
    mutate(sig = ifelse(pval > 0.05, "not_sig (<0.05)", "sig (>0.05)")) %>%
    rownames_to_column() %>%
    group_by(rowname) %>%
    mutate(a = rowname,
           b = round(exp,2),
           c = ifelse(exp < 1, "lower", "greater"),
           d = case_when(c == "lower" ~ (1-b)*100,
                         c == "greater" ~ (b-1)*100),
           f = sig,
           direction = c,
           percent = d,
           explanation = paste0("The average rate observed in ", 
                                a , 
                                " was ",
                                b , 
                                " times ", 
                                c , 
                                " than that of the reference group of this variable (explanatory variable) | or is ", 
                                d , 
                                "% " , 
                                c , 
                                " than the reference category ",
                                " | this was ", 
                                f),
           explanation = ifelse(rowname == "(Intercept)",
                                "In a linear regression model, the (Intercept) term represents the expected value of the response variable when all predictor variables are set to zero,
                               In a Poisson regression model, the (Intercept) term represents the expected count of events when all predictor variables are set to zero,
                               In a logistic regression model, the (Intercept) term represents the log-odds of the event occurring when all predictor variables are set to zero.",
                                explanation)) %>%
    dplyr::select(-a,-b,-c,-d,-f)
  
# remove the unused variables
  # NULL <- bound$a
  # NULL <- bound$b
  # NULL <- bound$c
  # NULL <- bound$d
  # NULL <- bound$f
  
  # return the data
  return(bound)
}

# display the test
#test <- cal_glm_table(neg_binomial_s1)
#test
#.........................................................................................................................................



#.........................................................................................................................................
# Generate a complete glm table for poisson and negbinomial----
#.........................................................................................................................................

tabulate_Binomial <- function(df) {
  
  # get the pvalues
  coefficients <- coef(summary(df))
  pval <- as.data.frame(coefficients[, "Pr(>|z|)"])
  # get the  the estimate
  estimate <- as.data.frame(coef(df)) 
  # get your exponentiated coeficients
  exp <- as.data.frame(exp(coef(df)))
  # exponentiate your confidence intervals
  exp_ci <- as.data.frame(exp(confint(df)))
  
  
  # generate the names that dont have variables
  names_1 <- pval %>% rownames_to_column()
  names_1 <- names_1[,1]
  names_2 <- estimate %>% rownames_to_column()
  names_2 <- names_2[,1]
  names_3 <- exp %>% rownames_to_column()
  names_3 <- names_3[,1]
  names_4 <- exp_ci %>% rownames_to_column()
  names_4 <- names_4[,1]
  
  
  # now find out the lowest number of names there are
  filter_lists <- list(names_1, names_2, names_3, names_4)
  # get the minimum
  min_length_index <- which.min(sapply(filter_lists, length))
  # get the minimum list
  names <- filter_lists[[min_length_index]]
  
  # filter for the correct variables in rowname
  pval <- pval %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  estimate <- estimate %>% rownames_to_column() %>% filter(rowname %in% names)
  exp <- exp %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  exp_ci <- exp_ci %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  
  # bind it all together
  bound <- cbind(estimate,exp,exp_ci,pval) 
  #bound
  
  # rename variables
  colnames(bound)[2] <- "estimate"
  colnames(bound)[3] <- "exp"
  colnames(bound)[6] <- "pval"
  
  # renaming convention
  bound <- bound %>%
    mutate(sig = ifelse(pval > 0.05, "not_sig (<0.05)", "sig (>0.05)")) %>%
    group_by(rowname) %>%
    mutate(a = rowname,
           b = round(exp,2),
           c = ifelse(exp < 1, "lower", "greater"),
           d = case_when(c == "lower" ~ (1-b)*100,
                         c == "greater" ~ (b-1)*100),
           f = sig,
           direction = c,
           percent = d,
           explanation = paste0("The average rate observed in ", 
                                a , 
                                " was ",
                                b , 
                                " times ", 
                                c , 
                                " than that of the reference group of this variable (explanatory variable) | or is ", 
                                d , 
                                "% " , 
                                c , 
                                " than the reference category ",
                                " | this was ", 
                                f),
           explanation = ifelse(rowname == "(Intercept)",
                                "In a linear regression model, the (Intercept) term represents the expected value of the response variable when all predictor variables are set to zero,
                               In a Poisson regression model, the (Intercept) term represents the expected count of events when all predictor variables are set to zero,
                               In a logistic regression model, the (Intercept) term represents the log-odds of the event occurring when all predictor variables are set to zero.",
                                explanation)) %>%
    dplyr::select(-a,-b,-c,-d,-f) %>%
    mutate(pval = round(pval,2))
  
  # return the data
  return(bound)
}

#......................................................................................................................................





#.........................................................................................................................................
# Generate a complete glm table for poisson and negbinomial----
#.........................................................................................................................................
# this is for the neg binomial but with the confint.default added instead of confint

# tabulate_Binomial_default <- function(df) {
#   
#   estimate <- as.data.frame(coef(df))
#   #estimate <- estimate[!is.na(estimate[, 1]), ]
#   
#   exp <- as.data.frame(exp(coef(df)))
#   #exp <- exp[!is.na(exp[, 1]), ]
#   
#   exp_ci <- as.data.frame(exp(confint.default(df)))
#   #exp_ci <- exp_ci[!is.na(exp_ci[, 1]), ]
#   
#   coefficients <- coef(summary(df))
#   pval <- as.data.frame(coefficients[, "Pr(>|z|)"])
#   bound <- cbind(estimate,exp,exp_ci,pval)
#   bound
#   
#   # rename variables
#   colnames(bound)[1] <- "estimate"
#   colnames(bound)[2] <- "exp"
#   colnames(bound)[5] <- "pval"
#   
#   # renaming convention
#   bound <- bound %>%
#     mutate(sig = ifelse(pval > 0.05, "not_sig (<0.05)", "sig (>0.05)")) %>%
#     rownames_to_column() %>%
#     group_by(rowname) %>%
#     mutate(a = rowname,
#            b = round(exp,2),
#            c = ifelse(exp < 1, "lower", "greater"),
#            d = case_when(c == "lower" ~ (1-b)*100,
#                          c == "greater" ~ (b-1)*100),
#            f = sig,
#            direction = c,
#            percent = d,
#            explanation = paste0("The average rate observed in ", 
#                                 a , 
#                                 " was ",
#                                 b , 
#                                 " times ", 
#                                 c , 
#                                 " than that of the reference group of this variable (explanatory variable) | or is ", 
#                                 d , 
#                                 "% " , 
#                                 c , 
#                                 " than the reference category ",
#                                 " | this was ", 
#                                 f),
#            explanation = ifelse(rowname == "(Intercept)",
#                                 "In a linear regression model, the (Intercept) term represents the expected value of the response variable when all predictor variables are set to zero,
#                                In a Poisson regression model, the (Intercept) term represents the expected count of events when all predictor variables are set to zero,
#                                In a logistic regression model, the (Intercept) term represents the log-odds of the event occurring when all predictor variables are set to zero.",
#                                 explanation)) %>%
#     dplyr::select(-a,-b,-c,-d,-f)
#   
#   # remove the unused variables
#   # NULL <- bound$a
#   # NULL <- bound$b
#   # NULL <- bound$c
#   # NULL <- bound$d
#   # NULL <- bound$f
#   
#   # return the data
#   return(bound)
# }



#.........................................................................................................................................
# Generate a complete glm table for poisson and negbinomial----
#.........................................................................................................................................

tabulate_Binomial_default <- function(df) {
  
  # get the pvalues
  coefficients <- coef(summary(df))
  pval <- as.data.frame(coefficients[, "Pr(>|z|)"])
  # get the  the estimate
  estimate <- as.data.frame(coef(df)) 
  # get your exponentiated coeficients
  exp <- as.data.frame(exp(coef(df)))
  # exponentiate your confidence intervals
  exp_ci <- as.data.frame(exp(confint.default(df)))
  
  
  # generate the names that dont have variables
  names_1 <- pval %>% rownames_to_column()
  names_1 <- names_1[,1]
  names_2 <- estimate %>% rownames_to_column()
  names_2 <- names_2[,1]
  names_3 <- exp %>% rownames_to_column()
  names_3 <- names_3[,1]
  names_4 <- exp_ci %>% rownames_to_column()
  names_4 <- names_4[,1]
  
  
  # now find out the lowest number of names there are
  filter_lists <- list(names_1, names_2, names_3, names_4)
  # get the minimum
  min_length_index <- which.min(sapply(filter_lists, length))
  # get the minimum list
  names <- filter_lists[[min_length_index]]
  
  # filter for the correct variables in rowname
  pval <- pval %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  estimate <- estimate %>% rownames_to_column() %>% filter(rowname %in% names)
  exp <- exp %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  exp_ci <- exp_ci %>% rownames_to_column() %>% filter(rowname %in% names) %>% dplyr::select(-rowname)
  
  # bind it all together
  bound <- cbind(estimate,exp,exp_ci,pval)
  #bound
  
  # rename variables
  colnames(bound)[2] <- "estimate"
  colnames(bound)[3] <- "exp"
  colnames(bound)[6] <- "pval"
  
  # renaming convention
  bound <- bound %>%
    mutate(sig = ifelse(pval > 0.05, "not_sig (<0.05)", "sig (>0.05)")) %>%
    group_by(rowname) %>%
    mutate(a = rowname,
           b = round(exp,2),
           c = ifelse(exp < 1, "lower", "greater"),
           d = case_when(c == "lower" ~ (1-b)*100,
                         c == "greater" ~ (b-1)*100),
           f = sig,
           direction = c,
           percent = d,
           explanation = paste0("The average rate observed in ", 
                                a , 
                                " was ",
                                b , 
                                " times ", 
                                c , 
                                " than that of the reference group of this variable (explanatory variable) | or is ", 
                                d , 
                                "% " , 
                                c , 
                                " than the reference category ",
                                " | this was ", 
                                f),
           explanation = ifelse(rowname == "(Intercept)",
                                "In a linear regression model, the (Intercept) term represents the expected value of the response variable when all predictor variables are set to zero,
                               In a Poisson regression model, the (Intercept) term represents the expected count of events when all predictor variables are set to zero,
                               In a logistic regression model, the (Intercept) term represents the log-odds of the event occurring when all predictor variables are set to zero.",
                                explanation)) %>%
    dplyr::select(-a,-b,-c,-d,-f)
  
  # return the data
  return(bound)
}




#......................................................................................................................................







#......................................................................................................................................
# Generate a complete glm explanation for poisson and negbinomial----
#......................................................................................................................................

# Explanation: this can be used for poisson regression analysis and negative binomial as the wording is for that specifically

explain_PoissonNegBinomial <- function(df) {
  
  estimate <- as.data.frame(coef(df))
  exp <- as.data.frame(exp(coef(df)))
  exp_ci <- as.data.frame(exp(confint(df)))
  coefficients <- coef(summary(df))
  pval <- as.data.frame(coefficients[, "Pr(>|z|)"])
  bound <- cbind(estimate,exp,exp_ci,pval)
  bound
  
  # rename variables
  colnames(bound)[1] <- "estimate"
  colnames(bound)[2] <- "exp"
  colnames(bound)[5] <- "pval"
  
  # renaming convention
  bound <- bound %>%
    mutate(sig = ifelse(pval > 0.05, "not_sig (<0.05)", "sig (>0.05)")) %>%
    rownames_to_column() %>%
    group_by(rowname) %>%
    mutate(a = rowname,
           b = round(exp,2),
           c = ifelse(exp < 1, "lower", "greater"),
           d = case_when(c == "lower" ~ (1-b)*100,
                         c == "greater" ~ (b-1)*100),
           f = sig,
           explanation = paste0("The average rate observed in ", 
                                a , 
                                " was ",
                                b , 
                                " times ", 
                                c , 
                                " than that of the reference group of this variable (explanatory variable) | or is ", 
                                d , 
                                "% " , 
                                c , 
                                " than the reference category ",
                                " | this was ", 
                                f),
           explanation = ifelse(rowname == "(Intercept)",
                                "In a linear regression model, the (Intercept) term represents the expected value of the response variable when all predictor variables are set to zero,
                               In a Poisson regression model, the (Intercept) term represents the expected count of events when all predictor variables are set to zero,
                               In a logistic regression model, the (Intercept) term represents the log-odds of the event occurring when all predictor variables are set to zero.",
                                explanation))
  
  # generate just the text
  text <- list(bound$explanation)
  
  # return the data
  return(text)
}

#.........................................................................................................................................







#.........................................................................................................................................
# function to summarise a glm function quick and easy ----
#.........................................................................................................................................
# function to summarise a glm function quick and easy
cal_sum_tab_glm <- function(model){
  # this is a setup
  sum <- summary(model)
  irr <- exp(coef(model))
  pval <- sum$coefficients[,4]
  # setup your confidence intervals
  ci <- as.data.frame(exp(confint(model, level = 0.95))) %>% 
    rename(CI1 = `2.5 %`,CI2 = `97.5 %`) %>% 
    mutate(CI1 = as.character(round(CI1,2)),CI2 = as.character(round(CI2,2)),CI = paste0(CI1,"-",CI2))
  # put everything together
  output <- cbind(irr,pval,ci) %>% 
    mutate(irr = round(irr,2)) %>% 
    dplyr::select(-CI1,-CI2)
  # display your output
  output
}

#.........................................................................................................................................





#.........................................................................................................................................
# Lowercase all var names ----
#.........................................................................................................................................
lowercase_column_names <- function(df) {
  # Get the current column names
  col_names <- colnames(df)
  
  # Convert to lowercase
  col_names_lower <- tolower(col_names)
  
  # Set the new column names to the data frame
  colnames(df) <- col_names_lower
  
  return(df)
}

# Example usage:
# Assuming 'your_dataframe' is your data frame
#your_dataframe <- lowercase_column_names(your_dataframe)
#.........................................................................................................................................


#...........
# generate z test from tabyl
#.............
z_test_proportions <- function(dead, alive) {
  # Perform a Z test for proportions
  prop_test_result <- prop.test(x = c(dead[1], dead[2]), 
                                n = c(alive[1] + dead[1], alive[2] + dead[2]),
                                alternative = "two.sided")
  
  # Display the test results
  print(prop_test_result)
  
  return(prop_test_result)
}

# # Example usage
# data <- data.frame(
#   period = c("pandemic", "pre_pandemic"),
#   alive = c(382, 701),
#   dead = c(110, 134)
# )
# 
# prop_test_result <- z_test_proportions(data$dead, data$alive)
# 
# # or 
# 
# prop_test_result <- data %>%
#   tabyl(pandemic_period,mort30fct) %>%
#   z_test_proportions()
#.........


#..........
# generate Agresti binomial test
#..........
#library(mosaic)
agresti_ci_proportions <- function(dead, alive) {
  # Perform Agresti confidence interval for proportions
  prop_test_result <- mosaic::binom.test(x = c(dead[1], dead[2]), 
                                 n = c(alive[1] + dead[1], alive[2] + dead[2]),
                                 ci.method = "agresti-coull")
  
  # Display the confidence interval results
  print(prop_test_result)
  
  return(prop_test_result)
}
#............




# .........
# format the z test results table
#..........
create_result_table_ztest <- function(z2) {
  a <- as.data.frame(z2$p.value)
  a <- round(a[1, 1], 2)
  
  b <- as.data.frame(z2$conf.int)
  bb <- paste0("(", round(b[1, 1],5), "-", round(b[2, 1],5), ")")
  
  c <- as.data.frame(z2$estimate * 100)
  
  result_table <- data.frame(
    pre_pandemic = round(c[2, 1], 2),
    post_pandemic = round(c[1, 1], 2),
    pvalue = a,
    confint = bb
  )
  
  return(result_table)
}

#..................




#..................
# cal_clean_emmeans
#..................

# gen your cal_clean_emmeans function
cal_clean_emmeans <- function(x) {
  x <- as.data.frame(x) %>% 
    mutate(rate = round(rate,2),
           asymp.LCL = round(asymp.LCL,2),
           asymp.UCL = round(asymp.UCL,2),
           SE = round(SE,2),
           CI95 = paste0("(",asymp.LCL,"-",asymp.UCL,")"),
           for_table = paste0(rate, " ", CI95))
  return(x)
}

#..................



#.................
# row_difference
#.................

row_difference <- function(df1, df2){
  n <- nrow(df1)-nrow(df2)

  
  diff <- abs(nrow(df1) - nrow(df2))
  percent <- round((diff / nrow(df1)) * 100,2)
  
  
  text <- paste0("The [", deparse(substitute(df1)), "] dataframe has ", n, " row difference as compared with [", deparse(substitute(df2)), "] dataframe // [this is a ",percent,"% movement]")
  return(text)
}
#..............

update_similar_string <- function(df, group, var, var_to_replace = "name", var_replacement = "name") {
  df %>%
    group_by({{group}}) %>%
    mutate(temp_key = ifelse({{var}} == var_to_replace,1,0)) %>%
    mutate({{var}} := ifelse(var_to_replace %in% {{var}} & var_replacement %in% {{var}} & temp_key == 1, var_replacement, {{var}})) %>%
    select(-temp_key)
}


#.....................




#.....................

# This generates number of days in a year
days_in_year <- function(year) if_else(leap_year(year), 366, 365)

#.....................




#...............................
# Generate date differences (this is from the imperial code section i did to generate episodes and cosecco linkage)
#.................................

# # this will need dplyr
# library(dplyr)
# 
# Create the data frame
 # data <- data.frame(id = c(1,1,1,1,2,2,3,3),
 #                    date = as.Date(c("2020-01-01","2020-01-08","2020-01-01","2020-01-09","2020-01-10","2020-01-11","2020-01-01","2020-01-20"),
 #                    format = "%Y-%m-%d"))



### IMPORTANT PLEASE READ ###
# your variable that you are putting as id needs to be a numeric or it will not work
# also the diff is in base so i have added that it will only work with base R


# Function to generate date_diff variable
# This is the one you really want here
cal_generate_date_diff <- function(df, id, date) {
  
  # Convert id variable to numeric if possible
  df <- df %>%
    mutate({{id}} := as.numeric({{id}})) %>%
    # Check if id is numeric
    {. ->> temp_df}
  
  if (any(is.na(temp_df[[as.character(substitute(id))]]))) {
    stop("
    *******************
    *****CAL ERROR***** The 'id' variable contains characters and cannot be converted into numeric without introducing NAs. Pick a new id or generate one that is numeric
    *******************
    ")
    return(df)
  }
  
  df <- temp_df %>%
    arrange({{id}}, desc({{date}})) %>%
    group_by({{id}}) %>%
    mutate(date_diff = c(NA, base::diff({{date}}))) %>%
    mutate(date_diff = abs(date_diff)) %>%
    ungroup() %>%
    mutate(date_diff_index = ifelse(is.na(date_diff),"index","after-index"))
  
  return(df)
}




# This is the descending version
cal_generate_date_diff_desc <- function(df,id,date) {
  
  # Convert id variable to numeric if possible
  df <- df %>%
    mutate({{id}} := as.numeric({{id}})) %>%
    # Check if id is numeric
    {. ->> temp_df}
  
  if (any(is.na(temp_df[[as.character(substitute(id))]]))) {
    stop("
    *******************
    *****CAL ERROR***** The 'id' variable contains characters and cannot be converted into numeric without introducing NAs. Pick a new id or generate one that is numeric
    *******************
    ")
    return(df)
  }
  
  
  df <- df %>%
    arrange({{id}}, desc({{date}})) %>%
    group_by({{id}}) %>%
    mutate(date_diff_desc = c(NA,base::diff({{date}}))) %>%
    mutate(date_diff_desc = abs(date_diff_desc)) %>%
    mutate(row_id = n() - row_number()) %>%
    ungroup() %>%
    mutate(date_diff_index_desc = ifelse(row_id == 0,"index","after-index")) %>%
    arrange({{id}},row_id) %>%
    select(-row_id)
  
  return(df)
}

cal_generate_date_diff_asc <- function(df,id,date) {
  
  # Convert id variable to numeric if possible
  df <- df %>%
    mutate({{id}} := as.numeric({{id}})) %>%
    # Check if id is numeric
    {. ->> temp_df}
  
  if (any(is.na(temp_df[[as.character(substitute(id))]]))) {
    stop("
    *******************
    *****CAL ERROR***** The 'id' variable contains characters and cannot be converted into numeric without introducing NAs. Pick a new id or generate one that is numeric
    *******************
    ")
    return(df)
  }
  
  df <- df %>%
    arrange({{id}}, {{date}}) %>%
    group_by({{id}}) %>%
    mutate(date_diff_asc = c(NA,base::diff({{date}}))) %>%
    mutate(date_diff_asc = abs(date_diff_asc)) %>%
    ungroup() %>%
    mutate(date_diff_index_asc = ifelse(is.na(date_diff_asc),"index","after-index"))
  
  return(df)
}


# # # apply the function to your test data
#temp1 <- cal_generate_date_diff_asc(data,id,date); print(temp1)
#   temp2 <- cal_generate_date_diff_desc(data,id,date); print(temp2)
#   temp3 <- cal_generate_date_diff(data,id,date); print(temp3)






#______________________________________________________________
convert_dates_to_string <- function(data) {
  for (i in seq_along(data)) {
    # Check if the column is character and contains non-empty values
    if (is.character(data[[i]]) && any(nchar(trimws(data[[i]])) > 0)) {
      tryCatch({
        # Attempt to parse the value as a date
        # If successful, convert it to string
        # If not, continue to the next column
        as.Date(data[[i]], tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%Y/%m/%d"))
        data[[i]] <- as.character(data[[i]])
      }, error = function(e) {
        next
      })
    }
  }
  return(data)
}
#______________________________________________________________



#______________________________________________________________
# clean whole NA variables
#______________________________________________________________
cal_clean_whole_df_na_strings <- function(df) {
  na_strings <- c("NA", "na", "n/a", "N/A", "null", "NaN")
  df %>% 
    mutate_all(~{
      v <- .x
      ifelse(v %in% na_strings, NA, v)
    })
}

# data <- data.frame(
#   Col1 = c("value1", "NA", "na", "n/a", "value2"),
#   Col2 = c("value3", "na", "N/A", "Value4", "value5")
# )
# 
# 
# clean_data <- cal_clean_whole_df_na_strings(data)
# print(clean_data)
# print(data)
#______________________________________________________________




#_______________________________________________________________
# cal var difference
#_______________________________________________________________
cal_var_difference <- function(df1, df2) {
  variables_df1 <- names(df1)
  variables_df2 <- names(df2)
  
  difference_df1_to_df2 <- setdiff(variables_df1, variables_df2)
  difference_df2_to_df1 <- setdiff(variables_df2, variables_df1)
  
  return(list(df1_to_df2 = difference_df1_to_df2, df2_to_df1 = difference_df2_to_df1))
}











#_______________________________________________________________
# base plot
#_______________________________________________________________

# Explanation: This is just a base plot, that will tell you the number grouped by a var in any dataset
cal_base_plot <- function(df, group_var, title, x, y) {
  # Ensure the group_var is treated as a string and then converted to a symbol
  group_var <- sym(group_var)
  
  # Manipulate and summarize the data
  df_var <- df %>%
    mutate(base = 1) %>%
    group_by(!!group_var) %>%
    summarize(total_cases = sum(base))
  
  # Plot the results
  ggplot(df_var, aes(x = !!group_var, y = total_cases)) +
    geom_col(fill = "steelblue") +
    labs(title = title, x = x, y = y) +
    theme_minimal()
}



#_______________________________________________________________
# dedupe collapsed data (used within condense_data function i made above)
#_______________________________________________________________

# df <- data.frame(
#   id = c(73, 74, 75),
#   text = c(
#     "NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA",
#     "KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676",
#     "KP7676 : KP7676 : KP7676 : KP7677 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676"
#   )
# )

cal_dedupe_collapsed_var <- function(text) {
  unique_elements <- unique(str_trim(unlist(strsplit(text, " : "))))
  paste(unique_elements, collapse = " : ")
}

# # Apply the function to the 'text' column
# df <- df %>%
#   mutate(text = sapply(text, remove_repeats))



#______________________________________________________________________
# keep non duplicates (used within condense_data function i made above)
#______________________________________________________________________
#
# df <- data.frame(
#   id = c(73, 74, 75),
#   text = c(
#     "NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA : NA",
#     "KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676",
#     "KP7676 : KP7676 : KP7676 : KP7677 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676 : KP7676"
#   )
# )
#
keep_non_duplicates <- function(text) {
  elements <- str_trim(unlist(strsplit(text, " : ")))
  non_duplicates <- elements[!duplicated(elements) & !duplicated(elements, fromLast = TRUE)]
  paste(non_duplicates, collapse = " : ")
}
#
# # Apply the function to the 'text' column
# df <- df %>%
#   mutate(text = sapply(text, keep_non_duplicates))
#



#______________________________________________________________________
# tolower all variable names
#______________________________________________________________________
# This does exactly what is says on the tin
tolower_colnames <- function(df) {
  names(df) <- tolower(names(df))
  return(df)
}
#______________________________________________________________________




#______________________________________________________________________
# opposit of %in%
#______________________________________________________________________
'%!in%' <- function(x,y)!('%in%'(x,y))
#______________________________________________________________________




#______________________________________________________________________
# make a beep every time you get to the end of whatever you highlight and run
#______________________________________________________________________
library(beepr)

long_running_function <- function() {
  # Set the beep sound to trigger when the function exits
  on.exit(beep())
  
  # Your code goes here
  Sys.sleep(5)  # Simulating long process
}

long_running_function()
#______________________________________________________________________




#______________________________________________________________________
# generate a hospital duration var so that i dont have to keep repeating this massive chunk

# library(dplyr)
# 
# hospital_duration_var <- function(df) {
#   df %>%
#     mutate(hospital_duration_days_groups = case_when(
#       hospital_duration_days >= 1000 ~ "1000 +",
#       hospital_duration_days >= 500 & hospital_duration_days <= 999 ~ "500-999",
#       hospital_duration_days >= 250 & hospital_duration_days <= 499 ~ "250-499",
#       hospital_duration_days >= 100 & hospital_duration_days <= 249 ~ "100-249",
#       hospital_duration_days >= 90 & hospital_duration_days <= 99 ~ "90-99",
#       hospital_duration_days >= 80 & hospital_duration_days <= 89 ~ "80-89",
#       hospital_duration_days >= 70 & hospital_duration_days <= 79 ~ "70-79",
#       hospital_duration_days >= 60 & hospital_duration_days <= 69 ~ "60-69",
#       hospital_duration_days >= 50 & hospital_duration_days <= 59 ~ "50-59",
#       hospital_duration_days >= 40 & hospital_duration_days <= 49 ~ "40-49",
#       hospital_duration_days >= 30 & hospital_duration_days <= 39 ~ "30-39",
#       hospital_duration_days >= 20 & hospital_duration_days <= 29 ~ "20-29",
#       hospital_duration_days >= 10 & hospital_duration_days <= 19 ~ "10-19",
#       hospital_duration_days >= 0 & hospital_duration_days <= 9 ~ "0-9",
#       is.na(hospital_duration_days) ~ "na"
#     )) %>%
#     mutate(hospital_duration_days_groups = factor(hospital_duration_days_groups,
#                                                   levels = c("1000 +", "500-999", "250-499", "100-249", "90-99", "80-89",
#                                                              "70-79", "60-69", "50-59", "40-49", "30-39", "20-29",
#                                                              "10-19", "0-9", "na")))
# }
# 
# bsi_total <- hospital_duration_var(c)

#______________________________________________________________________





#______________________________________________________________________
# tabulate_binomial_or
#______________________________________________________________________
#library(gt)

cal_tabulate_binomial_or <- function(model) {
  # Extract coefficients and standard errors
  coefficients <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate 95% confidence intervals using standard errors
  lower_ci <- coefficients - 1.96 * se
  upper_ci <- coefficients + 1.96 * se
  
  # Exponentiate coefficients and confidence intervals
  exp_coef <- exp(coefficients)
  exp_lower_ci <- exp(lower_ci)
  exp_upper_ci <- exp(upper_ci)
  
  # Combine lower and upper CI into a single string
  ci_combined <- paste0(format(round(exp_lower_ci, 2), nsmall = 2), " - ", 
                        format(round(exp_upper_ci, 2), nsmall = 2))
  
  # Extract p-values
  p_values <- summary(model)$coefficients[, "Pr(>|z|)"]
  
  # Convert p-values to character with custom formatting
  p_values_char <- ifelse(p_values < 0.001, 
                          paste0("<0.001"),
                          ifelse(p_values < 0.05, 
                                 format(round(p_values, 3), nsmall = 3),
                                 format(round(p_values, 2), nsmall = 2)))
  
  # Combine into a data frame with simplified column names
  result <- data.frame(
    Term = names(exp_coef),
    OR = exp_coef,
    CI = ci_combined,
    P_Value = p_values_char
  )
  
  # Create a gt table
  result_gt <- result %>%
    gt() %>%
    tab_header(
      title = "Binomial Regression Odds Ratios"
    ) %>%
    fmt_number(
      columns = c(OR),
      decimals = 2
    ) %>%
    cols_label(
      OR = "Odds Ratio",
      CI = "95% Confidence Interval",
      P_Value = "p-value"
    ) %>%
    # Center-align all columns
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = everything())
    )
  return(result_gt)
}



#_________________________________________________________________________


cal_tabulate_gamma_exp_coef <- function(model) {
  # Extract coefficients and standard errors
  coefficients <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate 95% confidence intervals using standard errors
  lower_ci <- coefficients - 1.96 * se
  upper_ci <- coefficients + 1.96 * se
  
  # Exponentiate coefficients and confidence intervals (for Gamma regression)
  exp_coef <- exp(coefficients)
  exp_lower_ci <- exp(lower_ci)
  exp_upper_ci <- exp(upper_ci)
  
  # Combine lower and upper CI into a single string
  ci_combined <- paste0(format(round(exp_lower_ci, 2), nsmall = 2), " - ", 
                        format(round(exp_upper_ci, 2), nsmall = 2))
  
  # Extract p-values (using the correct column for Gamma regression)
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  
  # Convert p-values to character with custom formatting
  p_values_char <- ifelse(p_values < 0.001, 
                          paste0("<0.001"),
                          ifelse(p_values < 0.05, 
                                 format(round(p_values, 3), nsmall = 3),
                                 format(round(p_values, 2), nsmall = 2)))
  
  # Combine into a data frame with simplified column names
  result <- data.frame(
    Term = names(exp_coef),
    Exponentiated_Coefficient = exp_coef,
    CI = ci_combined,
    P_Value = p_values_char
  )
  
  # Create a gt table
  result_gt <- result %>%
    gt() %>%
    tab_header(
      title = "Gamma Regression Exponentiated Coefficients"
    ) %>%
    fmt_number(
      columns = c(Exponentiated_Coefficient),
      decimals = 2
    ) %>%
    cols_label(
      Exponentiated_Coefficient = "Exponentiated Coefficient",
      CI = "95% Confidence Interval",
      P_Value = "p-value"
    ) %>%
    # Center-align all columns
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = everything())
    )
  return(result_gt)
}


#_________________________________________________________________________



cal_tabulate_inv_gaussian_exp_coef <- function(model) {
  # Extract coefficients and standard errors
  coefficients <- coef(model)
  se <- summary(model)$coefficients[, "Std. Error"]
  
  # Calculate 95% confidence intervals using standard errors
  lower_ci <- coefficients - 1.96 * se
  upper_ci <- coefficients + 1.96 * se
  
  # Exponentiate coefficients and confidence intervals (for Inverse Gaussian regression)
  exp_coef <- exp(coefficients)
  exp_lower_ci <- exp(lower_ci)
  exp_upper_ci <- exp(upper_ci)
  
  # Combine lower and upper CI into a single string
  ci_combined <- paste0(format(round(exp_lower_ci, 2), nsmall = 2), " - ", 
                        format(round(exp_upper_ci, 2), nsmall = 2))
  
  # Extract p-values (using the correct column for Inverse Gaussian regression)
  p_values <- summary(model)$coefficients[, "Pr(>|t|)"]
  
  # Convert p-values to character with custom formatting
  p_values_char <- ifelse(p_values < 0.001, 
                          paste0("<0.001"),
                          ifelse(p_values < 0.05, 
                                 format(round(p_values, 3), nsmall = 3),
                                 format(round(p_values, 2), nsmall = 2)))
  
  # Combine into a data frame with simplified column names
  result <- data.frame(
    Term = names(exp_coef),
    Exponentiated_Coefficient = exp_coef,
    CI = ci_combined,
    P_Value = p_values_char
  )
  
  # Create a gt table
  result_gt <- result %>%
    gt() %>%
    tab_header(
      title = "Inverse Gaussian Regression Exponentiated Coefficients"
    ) %>%
    fmt_number(
      columns = c(Exponentiated_Coefficient),
      decimals = 2
    ) %>%
    cols_label(
      Exponentiated_Coefficient = "Exponentiated Coefficient",
      CI = "95% Confidence Interval",
      P_Value = "p-value"
    ) %>%
    # Center-align all columns
    tab_style(
      style = cell_text(align = "center"),
      locations = cells_body(columns = everything())
    )
  return(result_gt)
}



#....................
hospital_duration_var <- function(df) {
  df %>%
    mutate(hospital_duration_days_groups = case_when(
      hospital_duration_days >= 1000 ~ "1000 +",
      hospital_duration_days >= 500 & hospital_duration_days <= 999 ~ "500-999",
      hospital_duration_days >= 250 & hospital_duration_days <= 499 ~ "250-499",
      hospital_duration_days >= 100 & hospital_duration_days <= 249 ~ "100-249",
      hospital_duration_days >= 90  & hospital_duration_days <= 99 ~  "90-99",
      hospital_duration_days >= 80  & hospital_duration_days <= 89 ~  "80-89",
      hospital_duration_days >= 70  & hospital_duration_days <= 79 ~  "70-79",
      hospital_duration_days >= 60  & hospital_duration_days <= 69 ~  "60-69",
      hospital_duration_days >= 50  & hospital_duration_days <= 59 ~  "50-59",
      hospital_duration_days >= 40  & hospital_duration_days <= 49 ~  "40-49",
      hospital_duration_days >= 30  & hospital_duration_days <= 39 ~  "30-39",
      hospital_duration_days >= 20  & hospital_duration_days <= 29 ~  "20-29",
      hospital_duration_days >= 10  & hospital_duration_days <= 19 ~  "10-19",
      hospital_duration_days >= 0   & hospital_duration_days <= 9 ~   "0-9",
      is.na(hospital_duration_days) ~ "na"),
      hospital_duration_days_groups = factor(hospital_duration_days_groups,
                                             levels = c("1000 +", "500-999", "250-499", "100-249", "90-99", "80-89",
                                                        "70-79", "60-69", "50-59", "40-49", "30-39", "20-29",
                                                        "10-19", "0-9", "na")))
}






# Compare models ----

# This is where you generate your glm model called model1 model2 that you have generated from the same df
# Thats all you need. 
cal_compare_models <- function(model1, model2, df) {
  # Get the model names dynamically
  model1_name <- deparse(substitute(model1))
  model2_name <- deparse(substitute(model2))
  
  # 1. Log-Likelihood Comparison
  logLik_model1 <- logLik(model1)
  logLik_model2 <- logLik(model2)
  cat("Log-Likelihood for", model1_name, ":", logLik_model1, "\n")
  cat("Log-Likelihood for", model2_name, ":", logLik_model2, "\n")
  cat("Winner:", ifelse(logLik_model1 > logLik_model2, model1_name, model2_name), "\n")
  
  # 2. AIC Comparison
  aic_model1 <- AIC(model1)
  aic_model2 <- AIC(model2)
  cat("AIC for", model1_name, ":", aic_model1, "\n")
  cat("AIC for", model2_name, ":", aic_model2, "\n")
  cat("Winner:", ifelse(aic_model1 < aic_model2, model1_name, model2_name), "\n")
  
  # 3. BIC Comparison
  bic_model1 <- BIC(model1)
  bic_model2 <- BIC(model2)
  cat("BIC for", model1_name, ":", bic_model1, "\n")
  cat("BIC for", model2_name, ":", bic_model2, "\n")
  cat("Winner:", ifelse(bic_model1 < bic_model2, model1_name, model2_name), "\n")
  
  # 4. MAE and RMSE Calculation
  predicted_model1 <- predict(model1, newdata = df, type = "response")
  predicted_model2 <- predict(model2, newdata = df, type = "response")
  
  mae_model1 <- mean(abs(df$hospital_duration_days - predicted_model1), na.rm = TRUE)
  mae_model2 <- mean(abs(df$hospital_duration_days - predicted_model2), na.rm = TRUE)
  
  rmse_model1 <- sqrt(mean((df$hospital_duration_days - predicted_model1)^2, na.rm = TRUE))
  rmse_model2 <- sqrt(mean((df$hospital_duration_days - predicted_model2)^2, na.rm = TRUE))
  
  cat("MAE for", model1_name, ":", round(mae_model1, 2), "\n")
  cat("RMSE for", model1_name, ":", round(rmse_model1, 2), "\n")
  cat("MAE for", model2_name, ":", round(mae_model2, 2), "\n")
  cat("RMSE for", model2_name, ":", round(rmse_model2, 2), "\n")
  
  cat("Winner (MAE):", ifelse(mae_model1 < mae_model2, model1_name, model2_name), "\n")
  cat("Winner (RMSE):", ifelse(rmse_model1 < rmse_model2, model1_name, model2_name), "\n")
}


# Generate CFR and ttest by organism ----
# Define the function
CFR_ttest_summary <- function(data, genus_name) {
  
  # Perform t-test on the raw data
  t_test_result <- t.test(mort_30_day_bin ~ cov_period, data = data %>% filter(organism_genus_name == genus_name))
  
  # Summarize the CFR by cov_period
  data_summary <- data %>% 
    filter(organism_genus_name == genus_name) %>%
    group_by(cov_period) %>%
    summarise(
      cfr = round((sum(mort_30_day_bin) / sum(mort_linked_bin)) * 100, 2),
      .groups = "drop"
    )
  
  # Pivot the data to wide format
  data_wide <- data_summary %>%
    pivot_wider(names_from = cov_period, values_from = cfr)
  
  # Extract t-test result including the 95% CI
  t_test_output <- broom::tidy(t_test_result)
  
  # Combine the t-test result with the wide data
  combined_result <- data_wide %>%
    bind_cols(t_test_output %>%
                mutate(p.value = round(p.value, 3),
                       temp = case_when(p.value < 0.05 ~ "<0.05",
                                        TRUE ~ "non-sig"),
                       p.value = as.character(p.value),
                       p.value = ifelse(temp == "<0.05", "<0.05", p.value),
                       pval = paste0(p.value, " (", round(conf.low, 2), "-", round(conf.high, 2), ")")) %>%
                select(pval))
  
  # Return the combined result
  return(combined_result)
}



# interpret model ----
cal_interpret_model <- function(model) {
  coef_summary <- summary(model)$coefficients
  conf_int <- tibble(
    characteristic = rownames(coef_summary),
    exp = exp(coef_summary[, "Estimate"]),
    lower = exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"]),
    upper = exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"])
  ) %>%
    mutate(CI = sprintf("%.2f-%.2f", lower, upper)) %>%
    select(characteristic, exp, CI)
  
  return(conf_int)
}


cal_interpret_model <- function(model) {
  coef_summary <- summary(model)$coefficients
  
  # Identify the correct column name for p-values
  p_col <- grep("Pr\\(>.*\\)", colnames(coef_summary), value = TRUE)
  
  if (length(p_col) == 0) {
    stop("Could not find the p-value column in model summary.")
  }
  
  conf_int <- tibble(
    characteristic = rownames(coef_summary),
    exp = exp(coef_summary[, "Estimate"]),
    lower = exp(coef_summary[, "Estimate"] - 1.96 * coef_summary[, "Std. Error"]),
    upper = exp(coef_summary[, "Estimate"] + 1.96 * coef_summary[, "Std. Error"]),
    p_value = coef_summary[, p_col] # Extract p-values dynamically
  ) %>%
    mutate(
      CI = sprintf("%.2f-%.2f", lower, upper),
      p_value_label = case_when(
        p_value < 0.001 ~ "<0.001",
        p_value < 0.05 ~ "<0.05",
        TRUE ~ sprintf("%.3f", p_value) # Show exact value for others
      )
    ) %>%
    select(characteristic, exp, CI, p_value_label)
  
  return(conf_int)
}
# Example usage:
# interpret_model(model)





# fast unique
# Explanation: generates a new variable which is an amalgamation of all the variables. 
cal_fast_unique <- function(df, sep = "|") {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame.")
  }
  
  # Create a temporary vector by pasting all columns together
  combined <- do.call(paste, c(df, sep = sep))
  
  # Filter out duplicated rows based on the combined vector
  df[!duplicated(combined), , drop = FALSE]
}

# apply it
#unique_df <- cal_fast_unique(my_dataframe)






# Get the most frequent between rows seperated by " : " ----
# Function to get the most frequent group
get_most_common <- function(x) {
  parts <- trimws(unlist(strsplit(x, " : ")))
  tab <- table(parts)
  names(tab)[which.max(tab)]
}

# # Sample data
# x <- data.frame(
#   ethnicity = c(
#     "black : white : white : white : white : white : white : white : white",
#     "asian : asian : black : asian : asian",
#     "mixed : mixed : mixed : white",
#     "white : white : white : white",
#     "black : black : white : white"
#   ),
#   stringsAsFactors = FALSE
# )
# 
# # Apply function
# x$dominant_ethnicity <- sapply(x$ethnicity, get_most_common)
# 
# print(x)


# cal_condense ----
library(rlang)
cal_condense <- function(data, grouped_var, var_to_collapse, new_name) {
  # Helper: get most common value
  get_most_common <- function(x) {
    parts <- trimws(unlist(strsplit(x, " : ")))
    tab <- table(parts)
    names(tab)[which.max(tab)]
  }
  data %>%
    group_by({{grouped_var}}) %>%
    summarize(
      !!new_name := paste0(get({{var_to_collapse}}), collapse = " : "),
      !!paste0("dominant_", new_name) := get_most_common(paste0(get({{var_to_collapse}}), collapse = " : "))
    )
}

# # Sample data
# df <- data.frame(
#   id = c(1,1,1,1,1,1,1,1,1,
#          2,2,2,2,2,
#          3,3,3,3,
#          4,4,4,4,
#          5,5,5,5),
#   ethnicity = c(
#     "black", "white", "white", "white", "white", "white", "white", "white", "white",
#     "asian", "asian", "black", "black", "black",
#     "mixed", "mixed", "mixed", "white",
#     "white", "white", "black", "black",
#     "black", "black", "white", "white"
#   ),
#   stringsAsFactors = FALSE
# )
# 
# # Call the function
# result <- cal_condense(df, grouped_var = id, var_to_collapse = "ethnicity", new_name = "ethnicity")
# 
# print(result)



####################################
# cal_forest_plot ----
####################################

# Explanation: Right then this is really cool, if you put in the output of a tbl_regression() it will produce a publication worthy forest plot

# library(dplyr)
# library(gtsummary)
# 
# set.seed(123)
# n <- 50000
# 
# example_data <- tibble(
#   age = rnorm(n, mean = 50, sd = 12),
#   sex = sample(c("Male", "Female"), n, replace = TRUE, prob = c(0.48, 0.52)),
#   treatment = sample(c("A", "B", "C"), n, replace = TRUE, prob = c(0.4, 0.4, 0.2)),
#   bmi = rnorm(n, mean = 27, sd = 5),
#   smoker = sample(c("Yes", "No"), n, replace = TRUE, prob = c(0.25, 0.75)),
#   ethnicity = sample(c("White", "Black", "Asian", "Mixed", "Other"), n, replace = TRUE,
#                      prob = c(0.6, 0.15, 0.15, 0.05, 0.05))
# ) %>%
#   mutate(
#     age_group = case_when(
#       age < 40 ~ "<40",
#       age >= 40 & age < 50 ~ "40-49",
#       age >= 50 & age < 60 ~ "50-59",
#       age >= 60 & age < 70 ~ "60-69",
#       age >= 70 ~ "70+"
#     )
#   )
# 
# # Simulate outcome (mortality) with age_group, bmi, smoker as risk factors
# log_odds <- -5 + 
#   0.5 * (example_data$age_group %in% c("50-59","60-69","70+")) +
#   0.05 * example_data$bmi +
#   0.5 * (example_data$smoker == "Yes") +
#   0.3 * (example_data$ethnicity == "Black")
# 
# # Convert to probability
# prob_mort <- 1 / (1 + exp(-log_odds))
# 
# # Add outcome column
# example_data <- example_data %>%
#   mutate(outcome = rbinom(n, size = 1, prob = prob_mort))
# 
# library(gtsummary)
# 
# fit <- glm(outcome ~ age_group + sex + treatment + bmi + smoker + ethnicity,
#            data = example_data, family = binomial)
# 
# tbl_fit <- tbl_regression(fit, exponentiate = TRUE)


cal_forest_plot <- function(x,
                            family = "OR",         
                            col_names = c("estimate", "ci", "p.value"),
                            graph.pos = 2,
                            boxsize = 0.3,
                            title_line_color = "darkblue") {
  
  if (!requireNamespace("forestplot", quietly = TRUE)) {
    stop("Package 'forestplot' is required for cal_forest_plot()", call. = FALSE)
  }
  if (!inherits(x, c("tbl_regression", "tbl_uvregression"))) {
    stop("`x=` must be class 'tbl_regression' or 'tbl_uvregression'", call. = FALSE)
  }
  
  library(dplyr)
  library(forestplot)
  
  # Determine log scale and x-axis label
  family <- toupper(family)
  xlog <- family %in% c("OR", "RR", "IRR")  # log scale for ratio measures
  xlab <- switch(family,
                 "OR"  = "Odds Ratio",
                 "RR"  = "Risk Ratio",
                 "IRR" = "Incidence Rate Ratio",
                 "RISK"= "Risk",
                 "Estimate")  # default
  
  # Prepare main text table
  txt_tb1 <- x %>%
    gtsummary::modify_column_unhide() %>%
    gtsummary::modify_fmt_fun(contains("stat") ~ gtsummary::style_number) %>%
    gtsummary::as_tibble(col_labels = FALSE)
  
  # Prepare header row and update "estimate" dynamically
  txt_tb2 <- x %>%
    gtsummary::modify_column_unhide() %>%
    gtsummary::as_tibble() %>%
    names()
  txt_tb2 <- data.frame(matrix(gsub("\\**", "", txt_tb2), nrow = 1), stringsAsFactors = FALSE)
  names(txt_tb2) <- names(txt_tb1)
  
  # Update header row for estimate column
  estimate_col <- which(names(txt_tb2) == "estimate")
  if(length(estimate_col) == 1) txt_tb2[1, estimate_col] <- xlab
  
  txt_tb <- bind_rows(txt_tb2, txt_tb1)
  
  # Combine with numeric stats
  line_stats <- x$table_body %>%
    select(all_of(c("estimate", "conf.low", "conf.high"))) %>%
    rename_with(~paste0(., "_num")) %>%
    tibble::add_row(.before = 0)
  
  forestplot_tb <- bind_cols(txt_tb, line_stats)
  
  # Add CI column
  forestplot_tb$ci <- c(NA, x$table_body$ci)
  
  # Bold only categorical summary rows
  summary_rows <- c(TRUE, x$table_body$row_type == "label"# & x$table_body$var_type != "continuous"
                    )
  forestplot_tb <- forestplot_tb %>% mutate(..summary_row.. = summary_rows)
  
  # Prepare labeltext as a list
  label_txt <- forestplot_tb %>%
    select(all_of(c("label", col_names))) %>%
    as.list() %>%  
    lapply(as.character)
  
  # Draw forest plot
  forestplot::forestplot(
    labeltext = label_txt,
    mean = forestplot_tb$estimate_num,
    lower = forestplot_tb$conf.low_num,
    upper = forestplot_tb$conf.high_num,
    is.summary = forestplot_tb$..summary_row..,
    graph.pos = graph.pos,
    lwd.zero = 2,
    boxsize = boxsize,
    graphwidth = grid::unit(5, "cm"),
    hrzl_lines = list("2" = grid::gpar(lwd = 2, col = title_line_color)),
    xlog = xlog,
    xlab = xlab,
    col = forestplot::fpColors(box = "darkblue", line = "darkblue", summary = "darkblue")
  )
}

# # Example of it being used
# forest_obj <- cal_forest_plot(tbl_fit, family = "OR");forest_obj   # Odds Ratio (default)
# forest_obj <- cal_forest_plot(tbl_fit, family = "RR");forest_obj   # Risk Ratio
# forest_obj <- cal_forest_plot(tbl_fit, family = "Risk");forest_obj # Absolute Risk




###################
# insert_blank_rows ----
###################

insert_blank_rows <- function(df, group_col) {
  out <- list()
  groups <- unique(df[[group_col]])
  for (grp in groups) {
    subset_df <- df %>% filter(!!rlang::sym(group_col) == grp)
    out <- append(out, list(subset_df))
    if (grp != tail(groups, 1)) {
      blank_row <- subset_df[1, ]
      blank_row[] <- NA
      blank_row$name_display <- " "
      out <- append(out, list(blank_row))
    }
  }
  bind_rows(out)
}
#insert_blank_rows(df, "var")




#########################
# cal_forestplot ----
#########################


# i am overwriting the ggfoerst function cheeky i know
# cal_forestplot <- forestplot

# # how to use
# library(ggforce)
# library(ggforestplot)
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(broom)

# # Simulate base data
# set.seed(123)
# n <- 100
# df <- tibble(
#   biomarker = rnorm(n),
#   trait1 = biomarker * 0.3 + rnorm(n),
#   trait2 = biomarker * -0.2 + rnorm(n),
#   trait3 = biomarker * 0.5 + rnorm(n),
#   group = sample(c("Metabolic", "Inflammatory"), size = n, replace = TRUE)
# )
# 
# # Reshape data to long format
# df_long <- df %>%
#   pivot_longer(cols = starts_with("trait"), names_to = "trait", values_to = "outcome")
# 
# # Fit GLMs per trait x group
# zzz <- df_long %>%
#   group_by(trait, group) %>%
#   nest() %>%
#   mutate(
#     model = map(data, ~ glm(outcome ~ biomarker, data = .x)),
#     results = map(model, ~ broom::tidy(.x)[2, ])
#   ) %>%
#   unnest(results) %>%
#   transmute(
#     name = paste(trait, group, sep = "_"),
#     trait = factor(trait),
#     group = factor(group),
#     beta = estimate,
#     se = std.error,
#     pvalue = p.value
#   ) %>%
#   # 👇 Factor `name` outside of grouping to avoid conflicting class errors
#   ungroup() %>%
#   mutate(name = factor(name, levels = unique(name), ordered = TRUE))
# 
# glimpse(zzz)
# # name   ||> <chr>
# # trait  ||> <fct> 
# # group  ||> <fct> 
# # beta   ||> <dbl> 
# # se     ||> <dbl> 
# # pvalue ||> <dbl>
# 
# 
# # Draw a forestplot of cross-sectional, linear associations.
# cal_forestplot(
#   df = zzz,
#   estimate = beta,
#   pvalue = pvalue,
#   psignif = 0.002,
#   xlab = "1-SD increment in cardiometabolic trait\nper 1-SD increment in biomarker concentration",
#   colour = trait
# ) +
#   ggforce::facet_col(
#     facets = ~group,
#     scales = "free_y",
#     space = "free"
#   )




############################
# cal_standardise_vars ----
############################

cal_standardise_vars <- function(col_names) {
  col_names <- tolower(col_names)                  # convert to lowercase
  col_names <- gsub(" ", "_", col_names, fixed = TRUE)  # replace spaces with underscores
  return(col_names)
}
# colnames(df) <- cal_standardise_vars(colnames(df))



############################
# cal_year_month ----
############################

# this generates year month and year_month from a date variable
cal_year_month <- function(data, date_col) {
  library(lubridate)
  data %>%
    mutate(
      year       = year({{ date_col }}),
      month      = month({{ date_col }}),
      year_month = format({{ date_col }}, "%Y-%m")
    )
}


# df <- data.frame(
#   id = 1:3,
#   my_date = as.Date(c("2025-09-18", "2024-03-05", "2023-12-25"))
# )
# 
# df %>%
#   cal_year_month(my_date)


#....................................
# Long date based off Sys.Date() ----
#....................................

# This generates a long hand version of todays date based off Sys.Date()
long.Sys.Date <- function() {
  today <- Sys.Date()
  day <- as.integer(format(today, "%d"))
  suffix <- ifelse(day %in% c(11,12,13), "th",
                   ifelse(day %% 10 == 1, "st",
                          ifelse(day %% 10 == 2, "nd",
                                 ifelse(day %% 10 == 3, "rd", "th"))))
  paste0(day, suffix, " ", format(today, "%B %Y"))
}

# Example
#long.Sys.Date()


#....................................
# Long date based off Sys.Date() ----
#....................................

hes_ethnicity_lookup <- function() {
  data.frame(
    code = c("A","B","C","D","E","F","G","H","J","K","L",
             "M","N","P","R","S","Z","99"),
    ethnos1 = c(
      "White - British",
      "White - Irish",
      "White - Any other White background",
      "Mixed - White and Black Caribbean",
      "Mixed - White and Black African",
      "Mixed - White and Asian",
      "Mixed - Any other mixed background",
      "Asian or Asian British - Indian",
      "Asian or Asian British - Pakistani",
      "Asian or Asian British - Bangladeshi",
      "Asian or Asian British - Any other Asian background",
      "Black or Black British - Caribbean",
      "Black or Black British - African",
      "Black or Black British - Any other Black background",
      "Other ethnic groups - Chinese",
      "Other ethnic groups - Any other ethnic group",
      "Not stated",
      "Not known"
    ),
    ethnos2 = c(
      "White",
      "White",
      "White",
      "Mixed",
      "Mixed",
      "Mixed",
      "Mixed",
      "Asian",
      "Asian",
      "Asian",
      "Asian",
      "Black",
      "Black",
      "Black",
      "Other",
      "Other",
      "Not stated",
      "Not known"
    ),
    stringsAsFactors = FALSE
  )
}

# ethnos_lookup <- hes_ethnicity_lookup()


#........................................
# Make a long date with correct th st etc ----
#........................................

cal_long_date <- function(x) {
  if (!inherits(x, "Date")) {
    warning("Input must be a Date object DUMMY. Turn it into a Date and try again.")
    x <- as.Date(x)
    if (any(is.na(x))) stop("Coercion failed. Please provide a valid Date.")
  }
  
  d <- as.integer(format(x, "%d"))  # get day as number
  paste0(d,
         ifelse(d %% 10 == 1 & d %% 100 != 11, "st",
                ifelse(d %% 10 == 2 & d %% 100 != 12, "nd",
                       ifelse(d %% 10 == 3 & d %% 100 != 13, "rd", "th"))),
         " ", format(x, "%B %Y"))
}

# # Example
# max_date <- as.Date("2025-10-10")
# cal_long_date(max_date)
# # [1] "10th October 2025"
# 
# # Example with character input
# cal_long_date("2025-10-10")
# # Warning: Input x must be a Date object. Attempting to coerce using as.Date().
# # [1] "10th October 2025"



#..............................
# admission_method_grouper ----
#..............................

# Uses data from this link https://archive.datadictionary.nhs.uk/DD%20Release%20May%202024/data_elements/admission_method_code__hospital_provider_spell_.html

admission_method_grouper <- function(admimeth_var) {
  case_when(
    admimeth_var == "11" ~ "Elective Admission: Waiting list",
    admimeth_var == "12" ~ "Elective Admission: Booked",
    admimeth_var == "13" ~ "Elective Admission: Planned",
    admimeth_var == "21" ~ "Emergency Admission: Emergency Care Department or dental casualty department of the Health Care Provider",
    admimeth_var == "22" ~ "Emergency Admission: General Practitioner after a request for immediate admission has been made direct to a Hospital Provider",
    admimeth_var == "23" ~ "Emergency Admission: Bed bureau",
    admimeth_var == "24" ~ "Emergency Admission: Consultant Clinic",
    admimeth_var == "25" ~ "Emergency Admission: Admission via Mental Health Crisis Resolution Team",
    admimeth_var == "2A" ~ "Emergency Admission: Emergency Care Department of another provider where the patient had not been admitted",
    admimeth_var == "2B" ~ "Emergency Admission: Transfer of an admitted patient from another Hospital Provider in an emergency",
    admimeth_var == "2C" ~ "Emergency Admission: Baby born at home as intended",
    admimeth_var == "2D" ~ "Emergency Admission: Other emergency admission",
    admimeth_var == "28" ~ "Emergency Admission: Other means",
    admimeth_var == "31" ~ "Maternity Admission: Admitted ante partum",
    admimeth_var == "32" ~ "Maternity Admission: Admitted post partum",
    admimeth_var == "81" ~ "Other Admission: Transfer of any admitted patient from other Hospital Provider other than in an emergency",
    admimeth_var == "82" ~ "Other Admission: The birth of a baby in this Health Care Provider",
    admimeth_var == "83" ~ "Other Admission: Baby born outside the Health Care Provider except when born at home as intended",
    admimeth_var == "99" ~ "Admission Method not known",
    TRUE ~ "Other / unclassified"
  )
}

# Example usage
# lab_hes_ita <- lab_hes_ita %>%
#   mutate(ADM_METHOD_LABEL = admission_method_grouper(ADMIMETH))







#....................................
# admission_method_grouper_broad ----
#....................................

# Uses data from this link https://archive.datadictionary.nhs.uk/DD%20Release%20May%202024/data_elements/admission_method_code__hospital_provider_spell_.html

admission_method_grouper_broad <- function(admimeth_var) {
  case_when(
    admimeth_var == "11" ~ "Elective Admission",
    admimeth_var == "12" ~ "Elective Admission",
    admimeth_var == "13" ~ "Elective Admission",
    admimeth_var == "21" ~ "Emergency Admission",
    admimeth_var == "22" ~ "Emergency Admission by General Practitioner",
    admimeth_var == "23" ~ "Emergency Admission",
    admimeth_var == "24" ~ "Emergency Admission",
    admimeth_var == "25" ~ "Emergency Admission",
    admimeth_var == "2A" ~ "Emergency Admission",
    admimeth_var == "2B" ~ "Emergency Admission",
    admimeth_var == "2C" ~ "Emergency Admission",
    admimeth_var == "2D" ~ "Emergency Admission",
    admimeth_var == "28" ~ "Emergency Admission",
    admimeth_var == "31" ~ "Maternity Admission",
    admimeth_var == "32" ~ "Maternity Admission",
    admimeth_var == "81" ~ "Other Admission",
    admimeth_var == "82" ~ "Other Admission",
    admimeth_var == "83" ~ "Other Admission",
    admimeth_var == "99" ~ "Admission Method not known",
    TRUE ~ "Other / unclassified"
  )
}

# Example usage
# lab_hes_ita <- lab_hes_ita %>%
#   mutate(ADM_METHOD_LABEL = admission_method_grouper_broad(ADMIMETH))












# Mid year population estimates ----
cal_eng_year <- function() {
  url <- "https://raw.githubusercontent.com/calpearson/Cal_Functions/main/data/eng_year.RData"
  env <- new.env()
  load(url(url), envir = env)
  env$eng_year
}

cal_eng_age_year <- function() {
  url <- "https://raw.githubusercontent.com/calpearson/Cal_Functions/main/data/eng_age_year.RData"
  env <- new.env()
  load(url(url), envir = env)
  env$eng_age_year
}

cal_eng_sex_year <- function() {
  url <- "https://raw.githubusercontent.com/calpearson/Cal_Functions/main/data/eng_sex_year.RData"
  env <- new.env()
  load(url(url), envir = env)
  env$eng_sex_year
}

cal_eng_age_sex_year <- function() {
  url <- "https://raw.githubusercontent.com/calpearson/Cal_Functions/main/data/eng_age_sex_year.RData"
  env <- new.env()
  load(url(url), envir = env)
  env$eng_age_sex_year
}

cal_eng_age_sex_year_geo <- function() {
  url <- "https://raw.githubusercontent.com/calpearson/Cal_Functions/main/data/eng_age_sex_year_geo.RData"
  env <- new.env()
  load(url(url), envir = env)
  env$eng_age_sex_year_geo
}


# Causal Impacts ----
cal_monthly_causal_impact <- function(df, date_col, outcome_col = NULL, intervention_date) {
  library(tidyverse)
  library(lubridate)
  library(CausalImpact)
  library(zoo)
  library(janitor)  # for tabyl()
  
  # -------------------------------
  # 1) Aggregate to monthly counts
  # -------------------------------
  ts_data <- df %>%
    tabyl({{date_col}}) %>%
    select({{date_col}}, n) %>%
    rename(
      month = {{date_col}},
      count = n
    ) %>%
    mutate(month = as.Date(month)) %>%
    arrange(month) %>%
    { zoo(.$count, order.by = .$month) }
  
  # -------------------------------
  # 2) Define pre/post periods
  # -------------------------------
  pre.period  <- c(min(index(ts_data)), intervention_date - 1)
  post.period <- c(intervention_date, max(index(ts_data)))
  
  # -------------------------------
  # 3) Run CausalImpact
  # -------------------------------
  impact <- CausalImpact(ts_data, pre.period, post.period)
  
  # -------------------------------
  # 4) Return results
  # -------------------------------
  list(
    impact_object = impact,
    pointwise_results = as_tibble(impact$series, rownames = "date") %>%
      mutate(date = as.Date(date))
  )
}

# library(tidyverse)
# library(lubridate)
# 
# set.seed(123)  # reproducibility
# 
# # Generate monthly dates and counts
# months <- seq(as.Date("2018-01-01"), as.Date("2021-12-01"), by = "month")
# counts <- rpois(length(months), lambda = 20)  # avg 20 cases per month
# 
# # Expand to one row per case
# myco_df <- tibble(month_year_date = rep(months, times = counts))
# 
# # Run your causal impact function
# results <- cal_monthly_causal_impact(
#   myco_df, 
#   date_col = month_year_date, 
#   intervention_date = as.Date("2020-01-01")
# )
# # Inspect results
# results$impact_object
# head(results$pointwise_results)
# 
# 
# # Run the function
# results <- cal_monthly_causal_impact(myco_df, 
#                                      date_col = month_year_date,
#                                      intervention_date = as.Date("2020-01-01"))
# 
# # Plot it
# results
# 
# # Access CausalImpact object
# impact <- results$impact_object
# impact
# 
# # Access tidy pointwise results
# pointwise_tbl <- results$pointwise_results
# head(pointwise_tbl)
# 
# # Plot results
# plot(impact)
# 
# # Plain english text
# summary(impact, "report")

