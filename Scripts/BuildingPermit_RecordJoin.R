rm(list=ls(all=TRUE))
library(tidyverse)
library(rio)
library(lubridate)
library(janitor)

#permit reports downloaded from the following City of Dallas link
### https://dallascityhall.com/departments/sustainabledevelopment/buildinginspection/Pages/permit_reports2.aspx

# Permits current up to June 2021 #

#directory pointing towards where all building permits are located on drive
PermitDirectory <- "Raw Data"
#create a dataframe with the name of all files in the above building directory folder
PermitFiles <- list.files(path = PermitDirectory, pattern = "*.xlsx",full.names = TRUE)

#function which imports all files in above directory and joins into a single dataframe
#must be certain that all new files do not have extraneous starting rows in dataset
PermitDF <- plyr::ldply(PermitFiles, import)

#data cleaning which reformats all names of columns in dataframe and sets up datetimes columns
PermitClean <- PermitDF %>%
  clean_names(.) %>%
  mutate(permit_number = coalesce(permit_number, permit_no),
         issued_date = mdy(coalesce(issued_date, issued)),
         city = "Dallas",
         state = "TX") %>%
  select(-issued, -permit_no, -mapsco)

test <- PermitClean %>%
  filter(issued_date >= as.Date("2015-01-01"))

export(test, "test_records_all.csv")
# edit issued_date filter to only include most recent permit files.
permitsNew <- PermitClean %>%
  filter(issued_date >= as.Date("2020-06-01"))

# export to file that will be geocoded with ESRI geocoding service
# file name should be changed based on current dates
export(permitsNew, "Working/Permits_June2020thruJune2021.csv")

#selection of permits of interest into distinct groups
#add new permit selections based on the above PermitType dataframe
#modify issued_year filter to select for timeframe of interest
PermitDemolition <- PermitClean %>%
  filter(permit_type %in% c("Demolition Permit Commercial"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

PermitCommercial <- PermitClean %>%
  filter(permit_type %in% c("Building (BU) Commercial  Reconstruction", "Building (BU) Commercial  Renovation"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())


PermitMultiFamilyImp <- PermitClean %>%
  filter(permit_type %in% c("Building (BU) Multi Family  Renovation", "Building (BU) Multi Family  Alteration"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())


mfnewconstruction <- PermitClean %>%
  filter(permit_type %in% c("Building (BU) Multi Family  New Construction"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())


PermitNewCommercial <- PermitClean %>%
  filter(permit_type %in% "Building (BU) Commercial  New Construction",
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

##Demolition Single Family##
sfnewconstruction <- PermitClean %>%
  filter(permit_type %in% c("Building (BU) Single Family  New Construction"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

sfdemolition <- PermitClean %>%
  filter(permit_type %in% c("Demolition Permit SFD/Duplex"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

sfimprovement <- PermitClean %>%
  filter(permit_type %in% c("Single Family  Alteration or Reconstruction or Renovation"),
         issued_date >= as.Date("2020-07-01")) %>%
  group_by(address, zip_code, issued_date) %>%
  summarise(count = n())

#export files into csv files to later geocode
export(PermitDemolition, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_Demolition.csv")
export(PermitCommercial, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_Commercial.csv")
export(PermitNewCommercial, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_NewCommercial.csv")

export(sfimprovement, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_SingleFamilyImprovement.csv")
export(sfdemolition, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_SingleFamilyDemolition.csv")
export(sfnewconstruction, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_SingleFamilyNew.csv")
export(mfnewconstruction, "Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/Permit Clean/Permits_2021_MultiFamilyNew.csv")


####### TESTING 
library(sf)
st_layers("Data Library/City of Dallas/01_Administrative Records/Building Permits/Final Table/DallasPermits.gdb")
st_layers("Data Library/Reference USA/ReferenceUSA_Geocoding.gdb")
  