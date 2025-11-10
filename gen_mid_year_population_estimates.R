## 0) Get started ----

# Source Cal_Functions.R from GitHub
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_Functions.R")

# Source Cal_Packages.R from GitHub
source("https://raw.githubusercontent.com/calpearson/Cal_Functions/main/Cal_packages.R")

conn_pops <- odbc::dbConnect(odbc::odbc(),
                             Driver = "SQL Server",
                             Server = "SQLCLUSCOLHPO19\\HPO19",
                             Database = "Populations",
                             Trusted_connection = "True")


query <- glue::glue_sql("SELECT [Period]
      ,[OfficialCode]
      ,[OfficialCodeOld]
      ,[GeoName]
      ,[Sex]
      ,[SexDesc]
      ,[Age]
      ,[AgeDesc]
      ,[Population]
      ,[ONSPubDate]
      ,[VersionNumber]
  FROM [Populations].[dbo].[vRes21_PHEC15_SingleYear]",.con = conn_pops)


ons_denoms_age <-DBI::dbGetQuery(conn_pops,query) %>%
  clean_names() %>%
  select(-sex, -age_desc, -version_number, -official_code_old) %>%
  rename(sex = sex_desc) %>%
  mutate(country = "ENGLAND") %>%
  filter(sex != "Persons")

# ENGLAND age_sex_year_geo
eng_age_sex_year_geo <- ons_denoms_age

# ENGLAND age_sex_year
eng_age_sex_year <- ons_denoms_age %>%
  group_by(period,country,sex,age) %>%
  summarise(pop = sum(population))

# ENGLAND age_year
eng_age_year <- ons_denoms_age %>%
  group_by(period,country,age) %>%
  summarise(pop = sum(population))

# ENGLAND sex_year
eng_sex_year <- ons_denoms_age %>%
  group_by(period,country,sex) %>%
  summarise(pop = sum(population))

# ENGLAND year
eng_year <- ons_denoms_age %>%
  group_by(period,country) %>%
  summarise(pop = sum(population))

# Save it out. 
save(eng_age_sex_year_geo, file = "data/eng_age_sex_year_geo.RData")
save(eng_age_sex_year,     file = "data/eng_age_sex_year.RData")
save(eng_age_year,         file = "data/eng_age_year.RData")
save(eng_sex_year,         file = "data/eng_sex_year.RData")
save(eng_year,             file = "data/eng_year.RData")

