# Useful Code

# Appendix
# 1) Finding which duplicate to keep
# 2) Remove everything after a space in variable string
# 3) Row based calculations
# 4) group_by and mutate using case when
# 5) select only the top number of a group
# 6) pivot_longer
# 7) tolower all column names
# 8) group_by and summarise sum
# 9) replace all NA within each numeric column with 0
# 10) how to calculate spells from HES 
# 11) Apportion cdi + all others (HO and CO)
# 12) Apportion non-CDI (HOHA,COHA,COCA,COIA)
# 13) Apportion CDI (HOHA,COHA,COCA,COIA)
# 14) replace all NULL in df with NA
# 15) format your dates column if already in date class
# 16) widen data to de-duplicate
# 17) duplicate flags
# 18) apportion in SGSS
# 19) difference between two dates
# 20) gen unique id's based on groups
# 21) develop functions
# 22) develop functions pt 2
# 23) develop functions pt 3
# 24) make all columns data lowercase
# 25) download all df from a file
# 26) flip a table on its side (transpose)
# 27) save multi files to csv by a group

#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 1) Finding which duplicate to keep ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

# in your df using distinct or unique can pose problems as only one value is kept.
# however if you have two dataframes that have the last column as different and you want to decide which one to keep i.e. NA and 1 if you used unique() it wouldnt pick up those two rows
# so here is what we can do. 
# lets say there are 3 cols and we want to dedupe but the 3rd col has differences so unique wont fully dedupe it
# we say dedupe based on the first two columns including the first and last one. 
# we can then sort and deduplicate seperately at our leisuer
df$dup <- duplicated(df[,1:2]) | duplicated(df[,1:2], fromLast=TRUE)

# if you just use the following
# the first duplicated row will be set as FALSE 
df$dup <- duplicated(df[,1:2])


# if you just use the following
# the last duplicated row will be set as FALSE 
df$dup <- duplicated(df[,1:2], fromLast=TRUE)

# this means that the following will show
# all as duplicated
df$dup <- duplicated(df[,1:2]) | duplicated(df[,1:2], fromLast=TRUE)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 2) Remove everything after a space in variable string ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# lets say we want to make a forename and surname out of full name.
# it is saying after " " (space) we remove everything after it.
df <- df %>% 
  mutate(forename = str_replace_all(fullname, " .*",""))


str_replace(string, pattern, replacement)


#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 3) Row based calculations
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# One columns row against the row directly under
# this uses the lag function
df <- df %>%
  mutate(spec_dt = as.Date(spec_dt),
         diff = spec_dt - lag(spec_dt, default = first(spec_dt)))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 4) group_by and mutate using case_when ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# very useful lets say you have a patient id and want to select the a preferred module if it exists
# this means that all rows in each epid will have CDR_ABX if it exists in the module column in a epid group
# good for deduplication now you can take out the duplications based on the AMR.
df <- df %>% 
  group_by(epids) %>%
  mutate(module_to_keep = case_when(any(module == "CDR_ABX") ~ "CDR_ABX", TRUE ~ "not_preferred")) %>%
  filter(module_to_keep == module | module_to_keep == "not_preferred" & module == "AMR")
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 5) select only the top number of a group ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# if you want to select the highest number out of a group this is very useful 
a <- test_method_by_species %>%
  mutate(across(everything(), .fns = ~replace_na(.,0))) %>% # make all NA 0
  mutate(total = `2015`+`2016`+`2017`+`2018`+`2019`+`2020`+`2021`) %>% # make your total
  group_by(organism_species_name) %>% # group by 
  top_n(1,total) # filter the highest value out of each group
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 6) pivot_longer / pivot_wider ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
library(tidyr)
# i have just found a super easy way of doing this.
# essentially you want e.g. years months or categories that are linked to each other to become linear right
# so all you need to do is make sure that they all contain numeric classes
# then you need to make sure that the columns you want to make liniar all start with the same thing
long <- pivot_longer(df, starts_with("2")) # this is for if i have all the years with character data within them
long <- pivot_longer(df, cols = c(`2015`,`2016`,`2017`,`2018`,`2019`,`2020`,`2021`)) # this does the same as above but you specify the ones you want
long <- pivot_longer(df, starts_with("2"), names_to = "year", values_to = "value") # this adds in what you want to name the column to be and then the numbers name also

# this was used for the spices charts where i had 2015 etc 
table %>%
  ungroup() %>%
  select(-organism_species_name) %>%
  pivot_longer(!test_method_description, names_to = "year", values_to = "n")

relig_income<-relig_income

relig_income %>%
  pivot_longer(!religion, names_to = "income", values_to = "count")

billboard %>%
  pivot_longer(
    cols = starts_with("wk"),
    names_to = "week",
    names_prefix = "wk",
    values_to = "rank",
    values_drop_na = TRUE
  )



# pivot_wider ///

fish_encounters <- fish_encounters

fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen)

# this will add in 0 if NA
fish_encounters %>%
  pivot_wider(names_from = station, values_from = seen, values_fill = 0)

us_rent_income %>%
  pivot_wider(
    names_from = variable,
    values_from = c(estimate, moe)
  )

df %>%
  pivot_wider(names_from = , values_from = )
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 7) tolower all column names ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
names(df) <- toupper(names(df))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 8) group_by and summarise sum ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# you need to make sure that all numeric values that you are trying to sum have no NA within them
# quick code to make sure of this
df <- df %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 9) replace all NA within each numeric column with 0 ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# does what it says on the tin
df <- df %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 10) how to calculate spells from HES ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# Essentially when you download from HES you are left with every hospital record per person
# There are many duplicate records of each person if they have been to the hospital over 1 times
# This code needs patient_id which is nhs_number as well as the spell information from HES to work out
# A spell is essentially what we would consider a grouped case (if there are two records of a person coming in on the same day then that is one spell) 


# test code "F:\Projects & programmes\COVID-19\HPRU impact of COVID\8) total_script\11) cleaning HES data\Olisa version"

cp_spells <- mSpells_uid(
  strata = sus$patient_id, 
  hes_trust_cd = sus$organisation_code_code_of_provider,
  hospital_provider_spell_no = sus$hospital_provider_spell_no,
  hes_admission_dt = sus$start_date_hospital_provider_spell,
  hes_discharge_dt = sus$end_date_hospital_provider_spell,
  hes_episode_start_dt = sus$episode_start_date,
  hes_episode_end_dt = sus$episode_end_date,
  source_of_admission_hospital_provider_spell = sus$source_of_admission_hospital_provider_spell,
  discharge_destination_hospital_provider_spell = sus$discharge_destination_hospital_provider_spell, 
  admission_method_hospital_provider_spell = sus$admission_method_hospital_provider_spell, 
  open_spell_end_date = Sys.Date(),
  trust_specific_spells = TRUE,
  dev_mode = FALSE,
  trim_points = sus$hrg_trim_points)

#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 11) Apportion cdi + all others (HO and CO) ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

# this is 3 for cdi and 2 for everything else.
dcs_processed <- dcs_processed %>%
  mutate(date_diff = case_when(organism == "cdi" ~ 3,
                               organism != "cdi" ~ 2))

dcs_processed <- dcs_processed %>%
  mutate(days = specimendate-dateadmitted) %>% 
  mutate(AP = case_when((patientlocation %in% c("NHS Acute Trust","",NA) | patientlocation == "Unknown" & dateentered >= ymd("2015-10-26")) &
                          patientcategory %in% c("In-patient","Emergency Assessment","Day patient","Unknown","",NA) & 
                          (days >= date_diff | is.na(dateadmitted)) ~ 1,
                        TRUE ~ 0))


# STATA code
# *apportionment
# gen date_diff = 0
# replace date_diff = 3 if organism == "cdi"
# replace date_diff = 2 if organism != "cdi"
# 
# gen days = datespecstata-dateadmstata
# gen mand_inpatient = 1 if (inlist(patientlocation,"NHS Acute Trust", "") | (patientlocation == "Unknown" & dateentered_stata >= td(26oct2015))) & inlist(patientcategory,"In-patient","Emergency Assessment","Day patient","Unknown","")
# gen AP = 1 if mand_inpatient == 1 & (days >=date_diff | days==.)
# replace AP = 0 if AP ==.
# lab define AP 1 "Trust apportioned" 0 "Not-Trust apportioned", replace
# lab values AP AP
# *onset algorithm
# gen aec_onset = 1 if inpatient ==1  & (days >=date_diff & days!=.)
# replace aec_onset = 0 if aec_onset == .
# lab define aec_onset 1 "Hospital Onset" 0 "Community Onset", replace
# lab values aec_onset aec_onset



#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 12) Apportion non-CDI (HOHA,COHA,COCA,COIA) ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

# Prior trust for non CDI
dcs_processed <- dcs_processed %>%
  filter(organism != "cdi") %>% 
  mutate(mand_inpatient = case_when((patientlocation %in% c("NHS Acute Trust", "", NA) | (patientlocation == "Unknown" & dateentered >= dmy("26-10-2015"))) & 
                                      patientcategory %in% c("In-patient","Emergency Assessment","Day patient","Unknown","",NA) ~ 1,
                                    TRUE~0)) %>%
  mutate(mod_AP = case_when(AP == 0 & mand_inpatient == 1 & days == 2 ~ 1,
                            TRUE ~ AP)) %>%
  dplyr::rename(last28days = healthcareinteractionhasthepatientbeendischargedfromanelectiveoremergencyhospitaladmissioninthereportingtrustinthelast28days,
                last28days_date = healthcareinteractionpleaseprovidethedateofdischargeforthemostrecentelectiveoremergencyhospitaladmissionpriortothepatientaspositivespecimen) %>%
  mutate(last28days_date = dmy(last28days_date)) %>%
  mutate(spec_dt_diff = specimendate - last28days_date) %>%
  mutate(DX = ifelse(last28days == "Yes" & spec_dt_diff %in% 0:27 & mod_AP != 1,"COHA",""),
         DX = ifelse(last28days == "Yes" & spec_dt_diff < 0 & mod_AP != 1, "Unknown",DX),
         DX = ifelse(last28days == "Yes" & is.na(spec_dt_diff) & mod_AP != 1, "Unknown",DX),
         DX = ifelse(last28days == "Yes" & spec_dt_diff > 27 & !is.na(last28days_date) & mod_AP != 1, "COCA",DX),
         DX = ifelse(last28days == ""    & mod_AP != 1 , "Missing information",DX),
         DX = ifelse(last28days == "No"  & mod_AP != 1,"COCA",DX),
         DX = ifelse(last28days == "Don't know" & mod_AP != 1,"Unknown",DX),
         DX = ifelse(mod_AP == 1, "HOHA",DX))




# stata code
# * non-cdi
# *gen days = datespecstata-dateadmstata
# *gen mand_inpatient = 1 if (inlist(patientlocation, "NHS Acute Trust","") | (patientlocation == "Unknown" & dateentered_stata >= td(26oct2015))) & inlist(patientcategory, "In-patient","Emergency Assessment","Day patient","Unknown","")
# gen mod_AP_gnbsi = cond(AP==0 & mand_inpatient == 1 & days == 2 & organism != "cdi", 1, AP)
# 
# * format some stuff
# tostring healthcareinteractionpleaseprovi, replace
# gen last28days_date = date(healthcareinteractionpleaseprovi, "DMY")
# format last28days_date %td
# gen last28days = healthcareinteractionhasthepatie
# 
# * apply your apportionment to non-cdi or gnbsi
# gen DX_gnbsi = "COHA" if last28days == "Yes" & inrange(datespecstata-last28days_date,0,27) & mod_AP_gnbsi != 1
# replace DX_gnbsi = "Unknown" if last28days == "Yes" & datespecstata-last28days_date < 0 & mod_AP_gnbsi != 1
# replace DX_gnbsi = "Unknown" if last28days == "Yes" & datespecstata-last28days_date==. & mod_AP_gnbsi != 1
# replace DX_gnbsi = "COCA" if last28days == "Yes" & datespecstata-last28days_date > 27 & last28days_date !=. & mod_AP_gnbsi != 1
# replace DX_gnbsi = "Missing information" if last28days == "" & mod_AP_gnbsi != 1
# replace DX_gnbsi = "COCA" if last28days == "No" & mod_AP_gnbsi != 1
# replace DX_gnbsi = "Unknown" if last28days == "Don't know" & mod_AP_gnbsi != 1
# replace DX_gnbsi = "HOHA" if mod_AP_gnbsi == 1


#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 13) Apportion CDI (HOHA,COHA,COCA,COIA) ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________






# using the function
library(obai)

# this is a tester and needs testing
df <- df %>%
  mutate(mod_AP = ifelse(AP==0 & mand_inpatient ==1 & days ==2 & organism == "cdi", 1, AP),
         PX = NA,
         PX = ifelse(prior_exposure_03mn == "" & mod_AP != 1, 6, PX),
         PX = ifelse(prior_exposure_03mn == "Don't know" & mod_AP != 1, 5, PX),
         PX = ifelse(prior_exposure_03mn == "No" & mod_AP != 1, 4, PX),
         PX = ifelse(prior_exposure_03mn == "Yes" & prior_exposure_12wk = "Yes" & mod_AP != 1, 3, PX),
         PX = ifelse(prior_exposure_04wk == "Yes" & mod_AP != 1, 2, PX),
         PX = ifelse(mod_AP == 1, 1, PX))


# stata code
# *cdi onset (worse_case)
# gen mod_AP = cond(AP==0 & mand_inpatient ==1 & days ==2 & organism == "cdi",1,AP) /*organism=="cdi",  cal note i have removed this and added the organisms that require coha and coca*/ 
#   gen PX  = .
# replace PX  = 6 if prior_exposure_03mn == "" & mod_AP != 1
# replace PX  = 5 if prior_exposure_03mn == "Don't know" & mod_AP != 1
# replace PX  = 4 if prior_exposure_03mn == "No" & mod_AP != 1
# replace PX  = 3 if inlist("Yes",prior_exposure_03mn,prior_exposure_12wk) & mod_AP != 1
# replace PX  = 2 if prior_exposure_04wk == "Yes" & mod_AP != 1
# replace PX  = 1 if mod_AP == 1
# lab define PX  1 "HOHA"  2 "COHA"  3 "COIA"  4 "COCA"  5 "Unkown"  6 "Not-reported", replace
# lab values PX  PX 
# 
# tostring prior_exposure_date, replace
# gen px_dt = date(prior_exposure_date,"DMY")
# format px_dt %td
# gen DX = 2 if prior_exposure_03mn == "Yes" & inrange(datespecstata-px_dt, 0,27) & mod_AP != 1
# replace DX = 3 if prior_exposure_03mn == "Yes" & inrange(datespecstata-px_dt, 28,83) & mod_AP != 1
# replace DX = 4 if prior_exposure_03mn == "Yes" & datespecstata-px_dt >=84 & px_dt !=. & mod_AP != 1
# 
# *cdi onset (adjusted)
# clonevar PX2  = PX
# replace PX2 = 6 if prior_exposure_03mn == "" & mod_AP != 1 
# replace PX2 = 5 if prior_exposure_03mn == "Don't know" & mod_AP != 1
# replace PX2 = 4 if prior_exposure_03mn == "No" & mod_AP != 1
# 
# *date picker
# replace PX2 = DX if DX !=.



# using a function
library(obai)
dcs_processed$prior_exposure_cdi <- dcs_apportionment(days_to_onset = 3,
                                                      patientlocation = dcs_processed$patientlocation,
                                                      patientcategory = dcs_processed$patientcategory,
                                                      dateadmitted = dcs_processed$dateadmitted,
                                                      specimendate = dcs_processed$specimendate,
                                                      dateentered = dcs_processed$dateentered,
                                                      prior_interactions  = TRUE,
                                                      prior_exposure_03mn = dcs_processed$prior_exposure_03mn,
                                                      prior_exposure_12wk = dcs_processed$prior_exposure_12wk,
                                                      prior_exposure_04wk = dcs_processed$prior_exposure_04wk,
                                                      prior_exposure_date = dcs_processed$prior_exposure_date)$prior_exposure


#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 14) replace all NULL in df in all columns with NA ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

# base version
replace(df, df =="NULL", NA)

# pipe version
df <- df %>%
  mutate_all(funs(type.convert(as.character(replace(., .=='NULL', NA)))))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 15) format your dates column if already in date class ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# lets say you have date in yyyy-mm-dd and you want it in dd/mm/yyyy
# this is what you want it to look like once exported into csv or something like that
# i needed to use this for the UID linkage of SGSS patient_date_of_birth into HES

# pipe version
df <- df %>%
  mutate(patient_date_of_birth = format(patient_date_of_birth, format = "%d/%m/%Y"))

# base version
df$patient_date_of_birth <- format(df$patient_date_of_birth, format="%d/%m/%Y")

#__________________________________________________________________________________________________________________________________________________________________________________________________________________



















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 16) widen data to de-duplicate ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this is difficult to understand so i have HES data to every individual in a dataset meaning its duplicated as many times as that individual has been in hospital
# Now I want to take the derived_mega_spell_start_date and derived_mega_spell_end_date
# The way you figure it out is is using left joins






#__________________________________________________________________________________________________________________________________________________________________________________________________________________



















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 17) duplicate flags ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

flag_dupes(iris, dplyr::everything())
flag_dupes(iris, dplyr::everything(), .both = FALSE)


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 18) apportion in SGSS ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________


library(epidm)
df <- df %>%
  mutate(diff_pos_admit = key_event_date_field - derived_mega_spell_start_date, # this is your key date minus the start of their admission
         diff_pos_discharge = key_event_date_field - derived_mega_spell_end_date, # this is your key date minus the end of their admission
         hospital_lock = T, # this is in the ho_categorise function and essentially means was the person from a hospital, becuase my data is all HES that would be a yes 
         linkset = "HES") %>% # linkset is HES because thats the dataset that i have my SGSS data linked too
  filter(diff_pos_admit < 29) %>% # I am only really interested in those where the diff_pos_admit is 28 and under
  group_time(date_start="derived_mega_spell_start_date", 
             #date_end="derived_mega_spell_end_date", # we don't want this section
             group_vars=c("patient_nhs_number","specimen_number"),
             window = 14,
             window_type = 'static')


ho_categorise <- function(data){  
  data[,onset_category := fcase(
    diff_pos_admit < -14, "Unlinked",
    grepl("SUS",linkset) & diff_pos_discharge > 0, "Unlinked",
    diff_pos_admit %in% c(-14:-1),  "CO",
    linkset=="SGSS:ECDS" & diff_pos_admit > 90, "Unlinked",
    hospital_lock & diff_pos_admit >= 14, "HO.HA", 
    hospital_lock & diff_pos_admit %in% c(7:13), "HO.pHA",
    hospital_lock & diff_pos_admit %in% c(2:6), "HO.iHA",
    hospital_lock & diff_pos_admit %in% c(0:1), "CO",
    linkset=="SGSS:ECDS" & is.na(derived_mega_spell_end_date) & diff_pos_admit %in% c(0:14), "CO",
    default = "Unlinked")       ]}


# apply the function
ho_categorise(df)


#__________________________________________________________________________________________________________________________________________________________________________________________________________________











#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 19) difference between two dates ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# I had to do it this way because the between function appears not to work on dates (no idea why)
# it would be best to do it the manual way that you know works

df <- df %>%
  mutate(in_between = ifelse(key_date >= start & key_date <= end,1,0))


#__________________________________________________________________________________________________________________________________________________________________________________________________________________





















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 20) gen unique id's based on groups ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# in this example i am grouping patient_nhs_number and patient_date_of_birth meaning that these are the groupings to be unique

df$unique_id <- df %>% group_indices(patient_nhs_number,patient_date_of_birth)

#__________________________________________________________________________________________________________________________________________________________________________________________________________________





#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 21) develop functions ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this is well past due, on the methodology of using functions and pipes
# this is rather simple when you think about it

# average distance per speed
cars <- cars

# load up your packages
library(tidyverse)

# make your function
av_dist <- function(df) {
  df <- df %>%
    group_by(speed) %>%
    summarise(av_dist = mean(dist))
}

# test out your new function
test_fun <- av_dist(cars)

#__________________________________________________________________________________________________________________________________________________________________________________________________________________





#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 22) develop functions pt 2 ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this is well past due, on the methodology of using functions and pipes
# this is rather simple when you think about it

# average distance per speed
cars <- cars

# load up your packages
library(tidyverse)

# make your function
grouped_avg <- function(df, group, average) {
  df <- df %>%
    group_by(!!enquo(group)) %>%
    summarise(av_dist = mean(!!enquo(average)))
  
  return(df)
}

# test out your new function
test_fun <- av_dist(cars, group = speed, average = dist)

# lazy way of doing things
test_fun <- av_dist(cars, speed, dist)
#_______________________________________________________________________________





#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 23) develop functions pt 3 ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this is better because it is less dependent on other things

library(dplyr)

# make your function
av_dist <- function(group, average) {
  df <- data.frame(group = group, average = average) %>%
    group_by(group) %>%
    summarise(av_dist = mean(average))
  
  return(df)
}

av_dist(cars$speed, cars$dist)




#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 24) make all columns data lowercase ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this converts everything to uppercase or lowercase if its a character

df <- df %>%
  mutate_all(.funs = toupper)

df <- df %>%
  mutate_all(.funs = tolower)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________




#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 25) download all df from a file
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# this is how you use purrr to download all dta files
amr <- dir("F:/xxx/xxx/xxx/xxx/xxx/xxx/xxx.dta", full.names = T) %>% 
  map_df(read_dta)

#__________________________________________________________________________________________________________________________________________________________________________________________________________________





#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 26) flip a table on its side (transpose) ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# transpose your chart
library(janitor)
test <- data.frame(X_1 = c(NA, "Title", 1:3),
                X_2 = c(NA, "Title2", 4:6))

test %>%
  row_to_names(row_number = 2)

# remove your plot
rm(test)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________





#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 27) first row of data as column names ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 
mtcars


# remove your plot
rm(test)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________






#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 27) save multi files to csv by a group (group_walk)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

root <- "F:/Projects & programmes/COVID-19/HPRU impact of COVID/8) total_script/2) clean the data/ESPAUR gold standard/3) CC between mine and ESPAUR/data/"  

org <- compare_df_diff[,1]
one <- compare_df_diff[1:118,1]
two <- compare_df_diff[122:140,1]

s <- stata %>%
  mutate(Organism_Species_Name = toupper(Organism_Species_Name)) %>%
  #filter(Organism_Species_Name == org) %>%
  select(nhsno=Patient_NHS_No,
         Date,
         dob=Patient_Date_Of_Birth,
         organism=Organism_Species_Name) %>%
  mutate(type_s = "stata",
         Date = as.character(Date),
         dob = as.character(dob))

r <- df %>%
  #filter(organism == org) %>%
  select(nhsno,
         Date=spec_dt_backup,
         dob=dob_backup,
         forename,
         surname,
         organism) %>%
  mutate(type_r = "r",
         Date = as.character(Date),
         dob = as.character(dob))

# find out what doesn't match up
test <- full_join(r,s,by=c("nhsno","Date","dob","organism")) %>%
  arrange(nhsno)

#_______________________________________________________________________________
# now save out into excel each of the organisms that are not 
test %>%
  filter(organism %in% one) %>%
  mutate(bug=organism) %>%
  group_by(organism) %>%
  group_walk(~write_xlsx(.x, paste0(root,.y$organism,".xlsx")))



#__________________________________________________________________________________________________________________________________________________________________________________________________________________

















#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 28) replace "," with a "" ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

new_str <- gsub(',','',address_str)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________







#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 29) str_detect filter for a variable containing a string ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
library(tidyverse)
new_str <- as.data.frame(datasets::HairEyeColor) %>%
  filter(str_detect(Eye, "Br"))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________








#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 30) row wise calculations ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

# here i want to group by organism and nhsno to see the difference in dates in a single column
df <- df %>% 
group_by(organism,nhsno) %>%
  mutate(Date = as.Date(Date),
         diff = Date-lag(Date),
         Date = as.character(Date)) %>%
  arrange(organism,nhsno)


mtcars <- mtcars %>%
  arrange(gear) %>%
  group_by(gear) %>%
  mutate(diff = wt-lag(wt))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________











#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# GGPLOT) make a age sex pyramid ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
age_sex %>%
  filter(organism_genus_name == "CITROBACTER") %>%
  mutate(n = ifelse(sex=="male", proportion*(-1),
                    proportion*1)) %>%
  ggplot(aes(x = age_group,y = n, fill=sex)) + 
  geom_bar(stat = "identity", colour="black") +
  coord_flip() +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_minimal() +
  scale_fill_manual(values=c('#FF9999', '#009999'))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________



#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# GGPLOT) center a title in ggplot----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
  theme(plot.title = element_text(hjust = 0.5))
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# GGPLOT) many plots in one pic in ggplot----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

facet_grid(~organism_genus_name)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 31) make id the row number----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

mutate(id = row_number())
#__________________________________________________________________________________________________________________________________________________________________________________________________________________


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 32) generate IRR from glm ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# load the packages
library(foreign) ; library(tsModel) ; library("lmtest") ; library("Epi")
library("splines") ; library("vcd") ; library(dplyr)

# read data from csv file
setwd("F:/Projects & programmes/COVID-19/HPRU impact of COVID/8) total_script/XOXO - Final Logic/14) ARIMA model/paper_study_code")

data <- read.csv("sicily.csv")

#Poisson with the standardised population as an offset
model1 <- glm(aces ~ offset(log(stdpop)) + smokban + time, family=poisson, data)
summary(model1)
summary(model1)$dispersion
round(ci.lin(model1,Exp=T),3)

in_var <- "smokban"
irr <- exp(model1$coefficients[in_var])
conf_interval <- confint(model1)
ci_lower <- exp(conf_interval[in_var, "2.5 %"])
ci_upper <- exp(conf_interval[in_var, "97.5 %"])
p_value <- summary(model1)$coefficients[in_var, "Pr(>|z|)"]

stats_output <- data.frame(irr,ci_lower,ci_upper,p_value) %>%
  mutate(less_than_0.05 = ifelse(p_value < 0.05,"yes","no"))

stats_output

#__________________________________________________________________________________________________________________________________________________________________________________________________________________


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 33) GET 95% CI FROM RATES ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

library(PHEindicatormethods)

# For crude rates:
  
  phe_rate(data, x, n) # where x is the number of cases and n is the denominator

# For age-adjusted rates:
  
  phe_dsr(data, x, n, stdpop = esp2013) # where stdpop is the standard population you're using to compare; by default, it uses the European Standard Population 2013
#__________________________________________________________________________________________________________________________________________________________________________________________________________________


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 34) How many duplicates of a variable are there  ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
  # lets say if you want to generate how many duplicates there are in each col for episode_chr_id then this is how you do it
  get_dupes(episode_chr_id)
  
  # or 
  group_by(episode_chr_id) %>%
    mutate(nrow = n())
  
#__________________________________________________________________________________________________________________________________________________________________________________________________________________


#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 35) linkage algorithm ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
  
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
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

  

#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# patient id ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

linked_data$patient_id <- record_group(df = linked_data, # put in the data
                                         sn = lk_sn, # the unique id you created above "linked_data$lk_sn <- 1:nrow(linked_data)"
                                         criteria = c("cri_1", "cri_2", "cri_3", "cri_4", "cri_5", "cri_6", "cri_7", "cri_8","cri_9"), # list out the chriteria
                                         sub_criteria = list(s6 = c("FI","soundex"), s9.1 = c("pt1","pt2","pt3"), s9.2=c("FI_2","soundex_2")), to_s4 = T) # you are also identifying the sub_chriteria

# the above has been retired, and we use links instead
df$patient_id <- diyar::links(criteria = list(df$cri_1, df$cri_2, df$cri_3, 
                                              df$cri_4, df$cri_5, df$cri_6, 
                                              df$cri_7, df$cri_8, df$cri_9), 
                              sub_criteria = list(cr6 = sub_criteria(df$FI, df$soundex), 
                                                  cr9 = sub_criteria(df$pt1, df$pt2, df$pt3, 
                                                                     sub_criteria(df$FI, df$soundex), 
                                                                     operator = "and")),
                              sn = df$lk_sn)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________



#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# episode id ----
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
linked_data$episode_id <- diyar::episodes(sn = linked_data$lk_sn, # select the data
                                            date = linked_data$spec_dt, # isolate the date
                                            case_length = 13, # this is the 13 days that make it a group
                                            data_source = linked_data$extract, # SGSS or DCS
                                            strata = diyar::combi(linked_data$patient_id, linked_data$organism)) 
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

  
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# left and right function ----
  right = function (string, char) {substr(string,nchar(string)-(char-1),nchar(string))}
  left = function (string,char) {substr(string,1,char)}
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

  
  
  
  
  
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 36) looping in RMarkdown ----  
  
  for (i in seq_along(org)) {
    x <- org[i]
    
    # Generate the data
    data <- summary_year_long %>%
      filter(organism_species_name == x) %>%
      rename(value = case_number)
    
    # Create the line graph using ggplot2
    plot <- ggplot(data, aes(x = years, y = value, group = 1)) +
      geom_line() +
      labs(x = "Year", y = "Value") +
      ggtitle(x) +
      theme_minimal()
    
    # Display the plot using print()
    print(plot)
  }
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
  
  
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 37) Download infections from SGSS  
  pfunc <- function(x){
    x <- x$processed
    x <- x[!duplicated(paste(x$nhsno, x$hosno, x$forename, x$surname, x$dob, x$epids, x$organism, x$spec_dt, x$specno, x$specimen_type_code, sep="_")),]
    x
  }
  
   df <- sgss_infections(start_dt = "2015-01-01", # start date in dd/mm/yyyy
                        end_dt = "2021-12-31", # end date in dd/mm/yyyy
                        abx = F, # ???
                        bug = genus, # this is just what you want to make a line list of "sgss_specie_cds() to get the full list"
                        spec_type_cd = c("T0X000"), # # this is what type of specimen it is you can get the full list here sgss_specimen_types()
                        episode_length = 13, # this is how long we want to group them together, an episode is within 13 days of initial
                        country = "England",# the country is England
                        dedup_level = c("pids","organism"), # you will need to dedupe as there are amr and cdr values within this
                        episode_type = "rolling",
                        #dedup_module_preference = "cdr",
                        module = c("cdr_abx_opie")) # the datasets from SGSS we have cdr_opie (deduplicated database)- shorter data download/ cdr_abx (multiples of the same individual) / amr (antimicrobial resistant)
  # set classes to your pids and epids
  df$processed$pids <- as.numeric(df$processed$pids)
  df$processed$epids <- as.numeric(df$processed$epids)
  # map your list to a dataframe
  clean <- map_dfr(list(df),pfunc)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

  
  
#__________________________________________________________________________________________________________________________________________________________________________________________________________________
# 38) How to write data into datalake ----
  dl_con <- odbc::dbConnect(odbc(),
                            Driver = "ODBC Driver 17 for SQL Server",
                            Server = "SQLCLUSCOLLK19.phe.gov.uk\\LAKE19",
                            Database = "Y008_HCAI_comm_PID", # THIS IS THE DATAFOLDER YOU WANT TO PLACE DATA INTO
                            Trusted_Connection = "yes",
                            timeout = 120)
  
  odbc::dbWriteTable(dl_con,
                     name=DBI::Id(schema='dbo',
                                  table='name_you_want_to_save_df'), # ALTER THIS BIT TOO WHAT YOU NEED
                     as.data.frame(df_you_want_to_save_into_datalake), # ALTER THIS BIT TOO WHAT YOU NEED
                     encoding = 'latin1',
                     overwrite=TRUE)
#__________________________________________________________________________________________________________________________________________________________________________________________________________________

  
  
  
  
  
  
#_______________________________
# make wide with iterations ----
#_______________________________
library(dplyr)
library(tidyr)
library(lubridate)
  
# Create the dataset
df <- data.frame(
    row_id = c(1, 1, 2, 2, 2),
    hrg_code = c("a", "b", "a", "a", "c"),
    iteration = c(1, 2, 1, 2, 3),
    start_date = c("01/01/2001", "01/02/2001", "01/03/2001", "01/04/2001", "01/05/2001"),
    end_date = c("03/01/2001", "03/02/2001", "01/03/2001", "01/04/2001", "01/05/2001"),
    stringsAsFactors = FALSE
 )
  
# Convert date columns to Date objects
df <- df %>%
    mutate(start_date = dmy(start_date),
           end_date = dmy(end_date))
  
# Reshape the data to wide format
df_wide <- df %>%
    pivot_wider(
      names_from = iteration,
      values_from = c(hrg_code, start_date, end_date),
      names_glue = "{.value}_{iteration}"
    )
  
# View the wide format dataset
print(df_wide)
  
  
#______________________________________________
  
# Remove all special characters from a variable
library(stringr)
df <- data.frame(
  Patient_Forename = c("John@ Doe", "Jane#Smith!", "Emily@!"),
  Age = c(30, 25, 40)
)

# Use mutate to remove all special characters from the 'Patient_Forename' column
df <- df %>%
  mutate(Patient_Forename = str_replace_all(Patient_Forename, "[^[:alnum:] ]", ""))

# Print the modified data frame
print(df)


  