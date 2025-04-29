rm(list=ls())
library(tidyverse)
library(countrycode)
library(readxl)
library(haven)

arms <- read_dta("Strategic Trade 1995-2021 (1).dta")
data <- readRDS("Gravity_V202211.rds")

data <- data %>% filter(year >= 1995) %>%
  filter(!(iso3_o == iso3_d)) %>% 
  select(c(year,iso3_o,iso3_d,iso3num_o,iso3num_d,dist,contig,
           comlang_off,comcol,col45,gdp_o,gdp_d,pop_o,pop_d,comrelig,eu_o,eu_d))
to_drop <- c(
  "TUV", "NRU", "SMR", "MCO", "PLW", "KNA", "MHL", "LIE", "DMA", "AND",
  "ATG", "SYC", "FSM", "TON", "STP", "KIR", "GRD", "VCT", "WSM", "LCA",
  "VUT", "BLZ", "MDV", "BRB", "BTN", "GUY", "ABW", "GRL", "BMU", "CYM",
  "GIB", "VGB", "AIA", "MSR", "PYF", "NCL", "FRO", "CUW", "SXM", "TCA",
  "SPM", "MYT", "REU", "GUM", "ASM", "MNP", "VIR", "NIU", 
  "ANT", "BES", "CCK", "COK", "CSK", "CXR", "DDR", "ESH", "FLK",
  "GLP", "GUF", "IOT", "MTQ", "NFK", "PCN", "SCG", "SHN",
  "SUN", "TKL", "VAT", "VDR", "WLF", "YMD", "YUG", "BHS", "COM", "CPV", "FJI", "ISL", "MAC", "MLT", "SLB", "SUR"
)

data <- data %>%
  filter(!iso3_o %in% to_drop & !iso3_d %in% to_drop)

#b <- data %>% select(c(iso3_o, pop_o, year)) %>% filter(pop_o < 1000 & year == 2021)  %>% group_by(iso3_o)  
#na<-data %>% filter(is.na(gdp_o) | is.na(pop_d)) %>% select(c(iso3_o,gdp_o,iso3_d,pop_d))

######################## AGREEMENT SCORE (OK)
ideal_points <- read_csv("IdealpointestimatesAll_Jun2024.csv")
agreement_scores <- read_csv("AgreementScoresAll_Jun2024.csv")
agreement <- agreement_scores %>%
  select(ccode1, ccode2, year, agree) %>%
  rename(Exporter_Code = ccode1, Importer_Code = ccode2, Year = year, Agreement_Score = agree) %>%
  mutate(Exporter = countrycode(Exporter_Code, origin = "cown", destination = "iso3c"),
         Importer = countrycode(Importer_Code, origin = "cown", destination = "iso3c"))

####################### DEMOCRACY LEVEL (choose the index but OK)
democracy <- read_csv("democracy-index-eiu.csv")
dem <- readRDS("V-Dem-CY-Core-v15.rds")
dem <- dem %>% select(c(country_text_id,country_id ,year,v2x_libdem)) %>% filter(year >= 1995)

###################### military expenditure (to deal with missing data, zeros vs interpolation)
arms_ex <- read_csv("Arms_ex.csv", skip = 3)
arms_ex <- arms_ex %>% select(-c(4:39))
arms_ex <- arms_ex %>%
  filter(!`Country Code` %in% to_drop)

###################### embargos (super annoying)
sanctions <- read.csv("GSDB_V4.csv")
sanctions <- sanctions %>% filter(end >= 1995) %>% mutate(sanctions = arms*military) %>%
  filter(sanctions == 1) %>% select(c(sanctioned_state,sanctioning_state, begin, end, sanctions))

####################### conflicts (deal with arabic coalitions)
n <- readRDS("ucdp-prio-acd-241.rds") 
n <- n%>% filter(year>=1995)
conflicts <-n %>% select(c(side_a,side_b,start_date2,ep_end_date,ep_end))